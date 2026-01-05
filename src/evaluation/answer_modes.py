"""
Answer mode implementations for diagnostic evaluation.

Three modes:
1. Pre-extracted: Look up answers from vignette dict (fast baseline)
2. Interactive: Clinician answers via terminal prompts
3. LLM-led: GPT-5-mini infers from clinical text only
"""

import json
import logging
from typing import Optional

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
            r = input("[p]resent/[a]bsent/[u]nclear: ").strip().lower()
            if r.startswith('p'):
                return 'present', input("Evidence: ").strip(), 1.0, None
            return ('absent' if r.startswith('a') else 'unclear'), '', 1.0, None

        elif item.item_type == 'exclusion':
            r = input("[c]leared/[e]xcluded/[u]nknown: ").strip().lower()
            status = 'cleared' if r.startswith('c') else ('excluded' if r.startswith('e') else 'unknown')
            return status, '', 1.0, None

        elif item.item_type == 'subjective':
            r = input("[m]et/[n]ot_met/[u]nclear: ").strip().lower()
            status = 'met' if r.startswith('m') else ('not_met' if r.startswith('n') else 'unclear')
            return status, '', 1.0, None

        elif item.item_type == 'duration':
            try:
                return 'present', '', 1.0, int(input("Duration in days: ").strip())
            except ValueError:
                return 'unclear', '', 0.5, None

        elif item.item_type == 'onset':
            try:
                return 'present', '', 1.0, int(input("Onset age: ").strip())
            except ValueError:
                return 'unclear', '', 0.5, None

        elif item.item_type == 'settings':
            s = input("Settings (e.g. home,school): ").strip()
            return ('present', s, 1.0, None) if s else ('unclear', '', 0.5, None)

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
