"""
Answer mode implementations for diagnostic evaluation.

Four modes:
1. Pre-extracted: Look up answers from vignette dict (fast baseline)
2. Interactive: Clinician answers via terminal prompts
3. LLM-led: GPT-5-mini infers from clinical text only
4. Hybrid: Routes subjective criteria to LLM, others to base mode
"""

import json
import logging
from typing import Callable, Optional

from openai import OpenAI
from src.diagnosis.driver import DiagnosticItem
from src.extraction.config import Config

logger = logging.getLogger(__name__)


def create_preextracted_answer_fn(answers: dict):
    """Mode 1: Look up pre-extracted answers from vignette data."""

    def answer_fn(item: DiagnosticItem) -> tuple[str, str, float, Optional[int]]:
        if item.item_type == 'symptom':
            return answers.get(item.item_id, 'unclear'), '', 1.0, None
        elif item.item_type == 'exclusion':
            return answers.get(item.item_id, 'unknown'), '', 1.0, None
        elif item.item_type == 'subjective':
            return answers.get(item.item_id, 'unclear'), '', 1.0, None
        elif item.item_type == 'duration':
            days = answers.get('duration_days')
            return ('present', '', 1.0, int(days)) if days else ('unclear', '', 0.5, None)
        elif item.item_type == 'onset':
            age = answers.get('onset_age')
            if age:
                return 'present', '', 1.0, int(age)
            # For event-based onset (e.g. PTSD), check if trauma symptoms present
            if any(k.startswith('ptsd_a') and v == 'present' for k, v in answers.items()):
                return 'present', '', 1.0, None
            return 'unclear', '', 0.5, None
        elif item.item_type == 'settings':
            return 'present', 'home,school', 1.0, None
        return 'unclear', '', 0.5, None

    return answer_fn


def create_interactive_answer_fn(clinical_text: str):
    """Mode 2: Clinician manually answers queries via terminal prompts."""
    displayed = False

    def answer_fn(item: DiagnosticItem) -> tuple[str, str, float, Optional[int]]:
        nonlocal displayed
        if not displayed:
            print(f"\nCLINICAL VIGNETTE\n{clinical_text}\n")
            displayed = True

        print(f"\n{item.item_type.upper()}: {item.description}")

        if item.item_type == 'symptom':
            while True:
                r = input("[p]resent/[a]bsent/[u]nclear: ").strip().lower()
                if r.startswith('p'):
                    return 'present', input("Evidence: ").strip(), 1.0, None
                elif r.startswith('a'):
                    return 'absent', '', 1.0, None
                elif r.startswith('u'):
                    return 'unclear', '', 1.0, None
                print("Invalid input. Please enter 'p', 'a', or 'u'.")

        elif item.item_type == 'exclusion':
            while True:
                r = input("[c]leared/[e]xcluded/[u]nknown: ").strip().lower()
                if r.startswith('c'):
                    return 'cleared', '', 1.0, None
                elif r.startswith('e'):
                    return 'excluded', '', 1.0, None
                elif r.startswith('u'):
                    return 'unknown', '', 1.0, None
                print("Invalid input. Please enter 'c', 'e', or 'u'.")

        elif item.item_type == 'subjective':
            while True:
                r = input("[m]et/[n]ot_met/[u]nclear: ").strip().lower()
                if r.startswith('m'):
                    return 'met', '', 1.0, None
                elif r.startswith('n'):
                    return 'not_met', '', 1.0, None
                elif r.startswith('u'):
                    return 'unclear', '', 1.0, None
                print("Invalid input. Please enter 'm', 'n', or 'u'.")

        elif item.item_type == 'duration':
            while True:
                r = input("Duration in days (or 'u' for unclear): ").strip().lower()
                if r == 'u':
                    return 'unclear', '', 0.5, None
                try:
                    return 'present', '', 1.0, int(r)
                except ValueError:
                    print("Invalid input. Please enter a number or 'u' for unclear.")

        elif item.item_type == 'onset':
            while True:
                r = input("Onset age (or 'u' for unclear): ").strip().lower()
                if r == 'u':
                    return 'unclear', '', 0.5, None
                try:
                    return 'present', '', 1.0, int(r)
                except ValueError:
                    print("Invalid input. Please enter a number or 'u' for unclear.")

        elif item.item_type == 'settings':
            while True:
                s = input("Settings (e.g. home,school) or 'u' for unclear: ").strip()
                if s.lower() == 'u':
                    return 'unclear', '', 0.5, None
                elif s:
                    return 'present', s, 1.0, None
                print("Invalid input. Please enter settings or 'u' for unclear.")

        return 'unclear', '', 0.5, None

    return answer_fn


def create_llm_answer_fn(clinical_text: str, client: OpenAI = None):
    """Mode 3: LLM infers answers from clinical text only."""
    if client is None:
        config = Config.from_env()
        client = OpenAI(api_key=config.openai_api_key)

    def answer_fn(item: DiagnosticItem) -> tuple[str, str, float, Optional[int]]:
        status_opts = {
            'symptom': ['present', 'absent', 'unclear'],
            'exclusion': ['cleared', 'excluded', 'unknown'],
            'subjective': ['met', 'not_met', 'unclear'],
            'duration': ['present', 'unclear'],
            'onset': ['present', 'unclear'],
            'settings': ['present', 'unclear'],
        }.get(item.item_type, ['present', 'absent', 'unclear'])

        prompt = f"""Assess whether this criterion is present in the clinical note.

# Clinical Note
{clinical_text}

# Criterion to Assess
{item.description}

# Instructions
- Base your assessment ONLY on what is explicitly stated or clearly implied
- If the information is not mentioned, use "unclear" or "unknown"
- For duration/onset, extract the numeric value if mentioned

Respond with ONLY valid JSON:
{{"status": "{'/'.join(status_opts)}", "confidence": 0.0-1.0, "evidence": "quote or null", "value": number_or_null}}"""

        try:
            logger.debug(f"    LLM query | disorder={item.disorder_id} | item={item.item_id} | type={item.item_type}")
            resp = client.chat.completions.create(
                model="gpt-5-mini",
                messages=[
                    {"role": "system", "content": "You are a psychiatrist reading clinical notes. Respond with valid JSON only."},
                    {"role": "user", "content": prompt}
                ],
                max_completion_tokens=2500  # Needs room for reasoning tokens
            )
            content = resp.choices[0].message.content or "{}"
            result = _parse_json_response(content, item.item_type)
            evidence_short = result[1][:50] + '...' if len(result[1]) > 50 else result[1]
            logger.debug(f"    LLM response | disorder={item.disorder_id} | item={item.item_id} | status={result[0]} | confidence={result[2]} | evidence=\"{evidence_short}\"")
            return result
        except Exception as e:
            logger.error(f"LLM error | item={item.item_id} error={e}")
            return 'unclear', '', 0.5, None

    return answer_fn


def _parse_json_response(content: str, item_type: str) -> tuple[str, str, float, Optional[int]]:
    """Parse JSON LLM response into answer tuple."""
    content = content.strip()
    if content.startswith('```'):
        content = content.split('```')[1]
        if content.startswith('json'):
            content = content[4:]
        content = content.strip()

    try:
        data = json.loads(content)
    except json.JSONDecodeError as e:
        logger.warning(f"JSON parse error: {e} | content={content[:100]}")
        return 'unclear', '', 0.5, None

    status = _validate_status(data.get('status', 'unclear'), item_type)
    confidence = max(0.0, min(1.0, float(data.get('confidence', 0.7))))
    evidence = str(data.get('evidence', '')) if data.get('evidence') else ''
    value = int(data['value']) if data.get('value') and item_type in ['duration', 'onset'] else None

    return status, evidence, confidence, value


def _validate_status(status: str, item_type: str) -> str:
    """Validate status is appropriate for item type."""
    valid = {
        'symptom': {'present', 'absent', 'unclear'},
        'exclusion': {'cleared', 'excluded', 'unknown'},
        'subjective': {'met', 'not_met', 'unclear'},
        'duration': {'present', 'unclear'},
        'onset': {'present', 'unclear'},
        'settings': {'present', 'unclear'},
    }
    defaults = {
        'symptom': 'unclear', 'exclusion': 'unknown', 'subjective': 'unclear',
        'duration': 'unclear', 'onset': 'unclear', 'settings': 'unclear'
    }
    return status if status in valid.get(item_type, set()) else defaults.get(item_type, 'unclear')


def create_subjective_llm_answer_fn(clinical_text: str, provider: str = 'claude'):
    """Create LLM answer function for subjective criteria only.

    Args:
        clinical_text: The clinical vignette text
        provider: 'claude' (default) or 'openai'

    Returns:
        Answer function that only handles subjective item types
    """
    config = Config.from_env()

    if provider == 'claude':
        from anthropic import Anthropic
        client = Anthropic(api_key=config.anthropic_api_key)
        model = config.default_anthropic_model  # claude-sonnet-4-5
    else:
        client = OpenAI(api_key=config.openai_api_key)
        model = "gpt-5-mini"

    def answer_fn(item: DiagnosticItem) -> tuple[str, str, float, Optional[int]]:
        if item.item_type != 'subjective':
            raise ValueError(f"Subjective LLM only handles subjective items, got: {item.item_type}")

        if provider == 'claude':
            prompt = f"""<task>
Assess this subjective clinical criterion based on the clinical note.
</task>

<clinical_note>
{clinical_text}
</clinical_note>

<criterion>
{item.description}
</criterion>

<instructions>
- Assess whether the criterion is MET, NOT_MET, or UNCLEAR
- Consider clinical significance thresholds defined in DSM-5-TR
- Provide confidence score (0.0-1.0) based on evidence strength
- Summarise supporting evidence (do NOT use quotation marks in the evidence field)
</instructions>

<response_format>
Respond with valid JSON only. Do not include quotation marks inside string values:
{{"status": "met|not_met|unclear", "confidence": 0.0-1.0, "evidence": "brief summary or null"}}
</response_format>"""
        else:
            prompt = f"""# Task
Assess this subjective clinical criterion based on the clinical note.

## Clinical Note
{clinical_text}

## Criterion to Assess
{item.description}

## Instructions
- Assess whether the criterion is **MET**, **NOT_MET**, or **UNCLEAR**
- Consider clinical significance thresholds defined in DSM-5-TR
- Provide confidence score (0.0-1.0) based on evidence strength
- Summarise supporting evidence (do NOT use quotation marks in the evidence field)

## Response Format
Respond with valid JSON only. Do not include quotation marks inside string values:
```json
{{"status": "met|not_met|unclear", "confidence": 0.0-1.0, "evidence": "brief summary or null"}}
```"""

        try:
            logger.debug(f"    Subjective LLM | provider={provider} | item={item.item_id}")

            if provider == 'claude':
                resp = client.messages.create(
                    model=model,
                    max_tokens=5000,
                    messages=[{"role": "user", "content": prompt}]
                )
                content = resp.content[0].text or "{}"
            else:
                resp = client.chat.completions.create(
                    model=model,
                    messages=[
                        {"role": "system", "content": "You are a psychiatrist assessing clinical criteria. Respond with valid JSON only."},
                        {"role": "user", "content": prompt}
                    ],
                    max_completion_tokens=5000
                )
                content = resp.choices[0].message.content or "{}"

            result = _parse_json_response(content, 'subjective')
            logger.debug(f"    Subjective LLM response | item={item.item_id} | status={result[0]} | confidence={result[2]}")
            return result

        except Exception as e:
            logger.error(f"Subjective LLM error | provider={provider} | item={item.item_id} | error={e}")
            return 'unclear', '', 0.5, None

    return answer_fn


def _display_llm_suggestion(item: DiagnosticItem, result: tuple):
    """Display LLM assessment for clinician review."""
    status, evidence, confidence, _ = result
    print(f"\n{'─' * 60}")
    print(f"SUBJECTIVE CRITERION: {item.description}")
    print(f"LLM Assessment: {status.upper()} (confidence: {confidence:.0%})")
    if evidence:
        print(f"Evidence: \"{evidence}\"")
    print(f"{'─' * 60}")


def _get_override_input(llm_result: tuple) -> tuple[str, str, float, Optional[int]]:
    """Prompt user to accept, override, or mark unclear."""
    while True:
        choice = input("[a]ccept LLM / [o]verride / [u]nclear: ").strip().lower()
        if not choice or choice[0] not in ('a', 'o', 'u'):
            print("Invalid input. Please enter 'a' to accept, 'o' to override, or 'u' for unclear.")
            continue
        if choice[0] == 'a':
            return llm_result  # Accept LLM suggestion as-is
        elif choice[0] == 'o':
            while True:
                status = input("Your assessment - [m]et / [n]ot_met: ").strip().lower()
                if not status or status[0] not in ('m', 'n'):
                    print("Invalid input. Please enter 'm' for met or 'n' for not_met.")
                    continue
                if status[0] == 'm':
                    return 'met', '', 1.0, None
                else:
                    return 'not_met', '', 1.0, None
        else:  # choice[0] == 'u'
            return 'unclear', '', 0.5, None


def create_hybrid_answer_fn(
    base_answer_fn: Callable[[DiagnosticItem], tuple],
    llm_answer_fn: Callable[[DiagnosticItem], tuple],
    interactive_override: bool = False
) -> Callable[[DiagnosticItem], tuple]:
    """Create hybrid answer function that routes subjective criteria to LLM.

    Args:
        base_answer_fn: Handles symptom, exclusion, duration, onset, settings
        llm_answer_fn: Handles subjective criteria with clinical judgment
        interactive_override: If True, show LLM suggestion and allow user override

    Returns:
        Hybrid answer function matching standard callback signature
    """
    def answer_fn(item: DiagnosticItem) -> tuple[str, str, float, Optional[int]]:
        if item.item_type == 'subjective':
            llm_result = llm_answer_fn(item)

            if interactive_override:
                _display_llm_suggestion(item, llm_result)
                return _get_override_input(llm_result)

            return llm_result

        return base_answer_fn(item)

    return answer_fn
