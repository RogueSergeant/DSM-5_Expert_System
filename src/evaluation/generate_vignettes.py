"""
Vignette Generation - creates clinical vignettes with Prolog-keyed answers.

Usage:
    python -m src.evaluation.generate_vignettes --count 50
"""

import argparse
import json
import logging
import random
import time
import uuid
from datetime import datetime
from pathlib import Path

from openai import OpenAI
from tqdm import tqdm
from src.reasoning.engine import PrologEngine
from src.extraction.config import Config

# Logging setup - logs to project root /logs/vignettes directory
LOG_DIR = Path(__file__).parent.parent.parent / 'logs' / 'vignettes'
LOG_DIR.mkdir(parents=True, exist_ok=True)
LOG_FILE = LOG_DIR / f"{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter('%(asctime)s | %(levelname)s | %(message)s'))
logger.addHandler(file_handler)

# Distribution: 40% CLEAR, 35% MODERATE, 15% AMBIGUOUS, 10% COMORBID
CASE_WEIGHTS = {'CLEAR': 0.4, 'MODERATE': 0.35, 'AMBIGUOUS': 0.15, 'COMORBID': 0.1}

# Common comorbid pairs (clinically realistic)
COMORBID_PAIRS = [
    ('mdd', 'gad'),
    ('mdd', 'ptsd'),
    ('gad', 'ptsd'),
    ('adhd', 'gad'),
    ('adhd', 'mdd'),
]


def load_disorders(engine: PrologEngine) -> dict:
    """Load disorder specs from Prolog KB (no truncation)."""
    specs = {}
    for d in engine.query("disorder(ID, Name, Category)"):
        did = str(d['ID'])
        if did == 'ptsd_preschool':
            continue

        specs[did] = {
            'id': did, 'name': str(d['Name']),
            'symptoms': [{'id': str(s['SID']), 'desc': str(s['Desc'])}
                         for s in engine.query(f"symptom({did}, SID, _, Desc)")],
            'exclusions': [{'id': str(e['ExcID']), 'desc': str(e['Desc'])}
                           for e in engine.query(f"exclusion_criterion({did}, ExcID, _, Desc)")],
            'subjective': [{'id': str(s['CritID']), 'desc': str(s['Desc'])}
                           for s in engine.query(f"subjective_criterion({did}, CritID, Desc, _)")],
            'categories': engine.query(f"symptom_category({did}, CatID, SymList, Req, ReqType)"),
            'duration': engine.query_one(f"duration_requirement({did}, N, Unit)"),
            'onset': engine.query_one(f"onset_requirement({did}, Type, Val)"),
        }
    return specs


def days_from_duration(dur: dict) -> int:
    """Convert duration dict to days."""
    if not dur:
        return 14
    return dur['N'] * {'days': 1, 'weeks': 7, 'months': 30, 'years': 365}.get(str(dur['Unit']), 1)


def generate_answers_single(spec: dict, difficulty: str) -> tuple[dict, bool]:
    """Generate answers respecting symptom category requirements."""
    answers = {}
    present_ids = set()

    sorted_cats = sorted(spec['categories'], key=lambda c: (
        0 if str(c['ReqType']) in ['all', 'exactly'] else 1,
        len(c['SymList'])
    ))

    for cat in sorted_cats:
        cat_symptoms = [str(s) for s in cat['SymList']]
        req_count = cat['Req']
        req_type = str(cat['ReqType'])
        already_present = sum(1 for s in cat_symptoms if s in present_ids)

        if req_type in ['all', 'exactly']:
            needed = req_count - already_present
        elif req_type == 'at_least':
            base_need = req_count if difficulty != 'CLEAR' else len(cat_symptoms)
            needed = max(0, base_need - already_present)
        elif req_type == 'at_least_one_of':
            needed = 0 if already_present > 0 else 1
        else:
            needed = max(0, req_count - already_present)

        if needed > 0:
            available = [s for s in cat_symptoms if s not in present_ids]
            random.shuffle(available)
            for sid in available[:needed]:
                present_ids.add(sid)

    if difficulty == 'AMBIGUOUS':
        meets = random.random() < 0.5
        if not meets:
            essential = set()
            for cat in spec['categories']:
                if str(cat['ReqType']) in ['all', 'exactly']:
                    essential.update(str(s) for s in cat['SymList'])
            removable = [s for s in present_ids if s not in essential]
            if removable:
                present_ids.discard(random.choice(removable))
    else:
        meets = True

    for s in spec['symptoms']:
        answers[s['id']] = 'present' if s['id'] in present_ids else 'absent'
    for e in spec['exclusions']:
        answers[e['id']] = 'cleared'
    for subj in spec['subjective']:
        answers[subj['id']] = 'met' if meets else random.choice(['met', 'not_met'])

    base_days = days_from_duration(spec['duration'])
    answers['duration_days'] = base_days + (7 if meets else -3)

    if spec['onset'] and str(spec['onset']['Type']) == 'before_age':
        threshold = spec['onset']['Val']
        answers['onset_age'] = threshold - 2 if meets else threshold + 5

    return answers, meets


def generate_answers_comorbid(specs: list[dict]) -> dict:
    """Generate answers for comorbid case with deduplication."""
    answers = {}
    seen_ids = set()

    for spec in specs:
        for s in spec['symptoms']:
            if s['id'] not in seen_ids:
                answers[s['id']] = 'present'
                seen_ids.add(s['id'])
        for e in spec['exclusions']:
            answers[e['id']] = 'cleared'
        for subj in spec['subjective']:
            answers[subj['id']] = 'met'

        base_days = days_from_duration(spec['duration'])
        answers['duration_days'] = max(answers.get('duration_days', 0), base_days + 14)

        if spec['onset'] and str(spec['onset']['Type']) == 'before_age':
            current = answers.get('onset_age', float('inf'))
            answers['onset_age'] = min(current, spec['onset']['Val'] - 3)

    if answers.get('onset_age') == float('inf'):
        answers.pop('onset_age', None)

    return answers


def get_exclusion_instructions(specs: list[dict]) -> str:
    """Build exclusion criteria evidence instructions."""
    needs = set()
    for spec in specs:
        for e in spec['exclusions']:
            desc = e['desc'].lower()
            if any(x in desc for x in ['substance', 'drug', 'alcohol']):
                needs.add('substance')
            if any(x in desc for x in ['medical', 'physiological']):
                needs.add('medical')
            if any(x in desc for x in ['medication']):
                needs.add('medication')

    lines = []
    if 'substance' in needs:
        lines.append("- Substance use: alcohol frequency, recreational drugs, caffeine")
    if 'medical' in needs:
        lines.append("- Labs: 'TSH normal', 'metabolic panel unremarkable', 'toxicology negative'")
    if 'medication' in needs:
        lines.append("- Medications: current prescriptions and OTC")
    return '\n'.join(lines) if lines else "- Brief medical history and current medications"


def get_mse_requirements(specs: list[dict]) -> str:
    """Build MSE requirements based on disorder category."""
    combined = ' '.join(s['desc'].lower() for spec in specs for s in spec['symptoms'])

    if any(x in combined for x in ['hallucination', 'delusion', 'disorganiz']):
        return "Appearance, behaviour, speech, mood, affect, thought process, thought content (delusions), perceptions (hallucinations), insight"
    if any(x in combined for x in ['suicide', 'death', 'self-harm']):
        return "Appearance, behaviour, mood, affect, thought content (suicidal ideation: frequency, plan, intent, protective factors), cognition, insight"
    if any(x in combined for x in ['memory', 'cognitive', 'orientation']):
        return "Appearance, orientation, attention, memory (immediate/recent/remote), language, executive function, insight"
    return "Appearance, behaviour, speech, mood (subjective), affect (observed), thought process, cognition, insight"


def build_prompt(specs: list[dict], answers: dict, difficulty: str) -> str:
    """Build comprehensive LLM prompt with all clinical requirements."""
    # Deduplicated symptom collection
    present_symptoms, absent_symptoms = [], []
    seen_ids = set()
    for spec in specs:
        for s in spec['symptoms']:
            if s['id'] in seen_ids:
                continue
            seen_ids.add(s['id'])
            if answers.get(s['id']) == 'present':
                present_symptoms.append(s['desc'])
            else:
                absent_symptoms.append(s['desc'])

    # Duration text
    duration = answers.get('duration_days', 14)
    if duration >= 365:
        duration_text = f"{duration // 365} year(s)"
    elif duration >= 30:
        duration_text = f"{duration // 30} month(s)"
    elif duration >= 7:
        duration_text = f"{duration // 7} week(s)"
    else:
        duration_text = f"{duration} days"

    # Onset instruction
    onset_text = ""
    for spec in specs:
        if spec['onset'] and str(spec['onset']['Type']) == 'before_age':
            onset_age = answers.get('onset_age', spec['onset']['Val'] - 2)
            onset_text = f"\nONSET: Symptoms first appeared around age {onset_age}. State this explicitly."
            break

    # Exclusion and MSE
    exclusion_text = get_exclusion_instructions(specs)
    mse_text = get_mse_requirements(specs)

    # Pertinent negatives: scale with complexity (min 4, max 8)
    n_negatives = min(8, max(4, len(absent_symptoms) // 2))
    selected_negatives = absent_symptoms[:n_negatives]

    # Style
    style = {
        'CLEAR': "Symptoms clearly evident. Patient describes experiences vividly with concrete examples.",
        'MODERATE': "Symptoms present but subtly described. Patient uses hedging ('kind of', 'sometimes').",
        'AMBIGUOUS': "Borderline presentation. Patient minimises or is uncertain about severity.",
        'COMORBID': "Multiple distinct symptom clusters clearly present."
    }.get(difficulty, "Symptoms present but require clinical inference.")

    return f"""Write a realistic psychiatric intake note (250-400 words).

ABSOLUTE RULES:
- NEVER name any psychiatric diagnosis, disorder, or syndrome
- NEVER use clinical labels (e.g., "depression", "anxiety", "ADHD", "trauma", "PTSD")
- Write as an intake note describing observations and patient reports only

STYLE: {style}

SYMPTOMS TO PORTRAY (describe phenomenologically with concrete examples):
{chr(10).join(f'- {s}' for s in present_symptoms)}

PERTINENT NEGATIVES (explicitly deny these):
{chr(10).join(f'- {s}' for s in selected_negatives)}

DURATION: Symptoms present for approximately {duration_text}. State when they began.
{onset_text}

FUNCTIONAL IMPACT (required): Describe specific ways symptoms affect:
- Work/academic performance
- Relationships
- Daily activities

REQUIRED SECTIONS:

1. DEMOGRAPHICS AND CHIEF COMPLAINT
   "[Age]-year-old [occupation] presents with [chief complaint in patient's words]"

2. HISTORY OF PRESENT ILLNESS
   Narrative with 2-3 direct quotes, clear timeline

3. PAST PSYCHIATRIC HISTORY
   Prior episodes, treatment, or "No prior psychiatric history"

4. FAMILY HISTORY
   One sentence

5. MEDICAL HISTORY AND SUBSTANCES
{exclusion_text}

6. MENTAL STATUS EXAMINATION
   {mse_text}

FORBIDDEN PHRASES:
- "preserved interest" or "maintained interest"
- "consistent with" or "suggestive of"
- Any diagnostic terminology

PERTINENT NEGATIVE LANGUAGE:
- "Denies [symptom]"
- "No [symptom] observed"
- "[Domain] intact/normal"

Output ONLY the clinical vignette. No preamble."""


def get_forbidden_labels(specs: list[dict]) -> list[str]:
    """Build forbidden diagnostic labels from disorder specs."""
    label_map = {
        'mdd': ['major depressive', 'mdd', 'major depression', 'clinical depression'],
        'gad': ['generalized anxiety', 'gad', 'anxiety disorder'],
        'adhd': ['adhd', 'add', 'attention deficit', 'attention-deficit'],
        'ptsd': ['ptsd', 'post-traumatic', 'posttraumatic'],
        'asd': ['autism spectrum', 'asd', 'autism', 'asperger'],
    }
    forbidden = set()
    for spec in specs:
        forbidden.update(label_map.get(spec['id'], []))
        forbidden.add(spec['name'].lower())
    return list(forbidden)


def verify_vignette(client: OpenAI, text: str, specs: list[dict], requires_onset: bool = False) -> dict:
    """Verify generated vignette using LLM-based validation."""
    word_count = len(text.split())
    logger.debug(f"Verifying vignette: {word_count} words, requires_onset={requires_onset}")

    if word_count < 50:
        logger.warning(f"Vignette too short: {word_count} words")
        return {'valid': False, 'issues': [f'Too short: {word_count} words'], 'warnings': [], 'word_count': word_count}

    forbidden = get_forbidden_labels(specs)
    onset_instruction = "- onset_stated: Does the text explicitly state when symptoms first appeared (childhood, specific age, or early life)?" if requires_onset else ""

    prompt = f"""Validate this psychiatric intake note against these criteria. Respond ONLY with valid JSON.

<criteria>
- no_diagnostic_labels: Text must NOT contain any diagnostic labels or disorder names: {', '.join(forbidden)}
- has_timeline: Text must state when symptoms began or how long they've been present
- has_pertinent_negatives: Text must explicitly deny at least 2 symptoms (using "denies", "no evidence of", "normal", "intact", etc.)
- has_medical_context: Text should mention labs, substances, or medications
{onset_instruction}
</criteria>

<text>
{text}
</text>

Respond with this exact JSON structure:
{{"status": "accepted" or "rejected", "reason": "rejection reason or empty string if accepted"}}"""

    try:
        response = client.chat.completions.create(
            model="gpt-5-nano",
            messages=[{"role": "user", "content": prompt}],
            max_completion_tokens=5000  # Higher limit for models with internal reasoning
        )

        logger.debug(f"Validation response ID: {response.id}")
        logger.debug(f"Validation finish reason: {response.choices[0].finish_reason}")

        content = response.choices[0].message.content
        if not content:
            logger.warning("Validation API returned empty content")
            logger.debug(f"Validation message: {response.choices[0].message}")
            return {'valid': False, 'issues': ['Validation returned empty'], 'warnings': [], 'word_count': word_count}

        logger.debug(f"Validation raw response: {content}")

        # Parse JSON from response (handle potential markdown wrapping)
        content = content.strip()
        if content.startswith('```'):
            content = content.split('```')[1]
            if content.startswith('json'):
                content = content[4:]
        result = json.loads(content.strip())

        logger.debug(f"Validation parsed: {result}")

        if result.get('status') == 'accepted':
            return {'valid': True, 'issues': [], 'warnings': [], 'word_count': word_count}
        else:
            return {'valid': False, 'issues': [result.get('reason', 'Unknown')], 'warnings': [], 'word_count': word_count}

    except (json.JSONDecodeError, KeyError) as e:
        logger.error(f"Validation parse error: {e}")
        logger.debug(f"Failed to parse: {content if 'content' in dir() else 'no content'}")
        return {'valid': False, 'issues': [f'Validation parse error: {e}'], 'warnings': [], 'word_count': word_count}


def generate_vignette(client: OpenAI, specs: list[dict], difficulty: str, idx: int,
                      max_retries: int = 2) -> dict:
    """Generate one vignette with verification and retry."""
    disorder_ids = [s['id'] for s in specs]
    logger.info(f"=== Vignette {idx} | {disorder_ids} | {difficulty} ===")

    if len(specs) > 1:
        answers = generate_answers_comorbid(specs)
        meets = True
    else:
        answers, meets = generate_answers_single(specs[0], difficulty)

    prompt = build_prompt(specs, answers, difficulty)
    requires_onset = any(spec['onset'] and str(spec['onset']['Type']) == 'before_age' for spec in specs)

    logger.debug(f"Prompt length: {len(prompt)} chars")
    logger.debug(f"Prompt preview: {prompt[:500]}...")

    vignette_text = ""
    verification = {'valid': False, 'issues': ['Not generated'], 'warnings': []}

    for attempt in range(max_retries + 1):
        logger.info(f"Generation attempt {attempt + 1}/{max_retries + 1}")
        try:
            response = client.chat.completions.create(
                model="gpt-5-mini",
                messages=[
                    {"role": "system", "content": "You are an expert psychiatrist writing clinical intake notes. Be realistic and follow the format exactly."},
                    {"role": "user", "content": prompt}
                ],
                max_completion_tokens=10_000  # Higher limit for models with internal reasoning
            )

            # Log full response metadata
            logger.debug(f"Response ID: {response.id}")
            logger.debug(f"Model: {response.model}")
            logger.debug(f"Finish reason: {response.choices[0].finish_reason}")
            if response.usage:
                logger.debug(f"Usage: prompt={response.usage.prompt_tokens}, completion={response.usage.completion_tokens}, total={response.usage.total_tokens}")

            content = response.choices[0].message.content
            refusal = getattr(response.choices[0].message, 'refusal', None)

            if refusal:
                logger.warning(f"API refusal: {refusal}")
                tqdm.write(f"    Refusal: {refusal}")

            if not content or not content.strip():
                logger.warning(f"Empty content. Finish reason: {response.choices[0].finish_reason}")
                logger.debug(f"Full message object: {response.choices[0].message}")
                tqdm.write(f"    API returned empty content (attempt {attempt + 1})")
                time.sleep(0.5)
                continue

            logger.debug(f"Content length: {len(content)} chars")
            logger.debug(f"Content preview: {content[:300]}...")

            vignette_text = content.strip()
            verification = verify_vignette(client, vignette_text, specs, requires_onset)

            logger.info(f"Verification: valid={verification['valid']}, issues={verification['issues']}")

            if verification['valid']:
                break
            elif attempt < max_retries:
                tqdm.write(f"    Retry {attempt + 1}: {verification['issues']}")
                time.sleep(0.5)

        except Exception as e:
            logger.error(f"API exception: {type(e).__name__}: {e}")
            tqdm.write(f"    API error: {e}")
            time.sleep(1)

    difficulty_label = 'COMORBID' if len(specs) > 1 else difficulty

    return {
        "id": f"vig_{idx:03d}_{'_'.join(disorder_ids)}_{difficulty_label.lower()}_{uuid.uuid4().hex[:8]}",
        "ground_truth": disorder_ids,
        "difficulty": difficulty_label,
        "meets_criteria": meets,
        "clinical_text": vignette_text,
        "answers": answers,
        "verification": verification
    }


def select_case_type() -> str:
    """Weighted random case type selection."""
    r = random.random()
    cumulative = 0
    for case_type, weight in CASE_WEIGHTS.items():
        cumulative += weight
        if r < cumulative:
            return case_type
    return 'MODERATE'


def generate_vignettes(count: int = 50, output_dir: Path = None):
    """Generate vignettes and save to JSON."""
    print(f"Log file: {LOG_FILE}", flush=True)
    logger.info(f"Starting vignette generation: count={count}")

    output_dir = output_dir or Path(__file__).parent.parent.parent / 'data' / 'vignettes'
    output_dir.mkdir(parents=True, exist_ok=True)

    prolog_dir = Path(__file__).parent.parent / 'prolog'
    engine = PrologEngine(prolog_dir)
    engine.load_file('schema.pl')
    engine.load_file('gold_standard/loader.pl')
    specs = load_disorders(engine)
    disorder_ids = list(specs.keys())

    print(f"Loaded {len(specs)} disorders: {disorder_ids}", flush=True)
    logger.info(f"Loaded disorders: {disorder_ids}")

    config = Config.from_env()
    client = OpenAI(api_key=config.openai_api_key)
    logger.info(f"OpenAI client initialised")

    vignettes = []
    pbar = tqdm(range(count), desc="Generating", unit="vignette")
    for i in pbar:
        case_type = select_case_type()

        if case_type == 'COMORBID':
            pair = random.choice(COMORBID_PAIRS)
            spec_list = [specs[pair[0]], specs[pair[1]]]
            difficulty = 'COMORBID'
        else:
            disorder_id = disorder_ids[i % len(disorder_ids)]
            spec_list = [specs[disorder_id]]
            difficulty = case_type

        pbar.set_postfix(disorder=spec_list[0]['id'], difficulty=difficulty)
        try:
            v = generate_vignette(client, spec_list, difficulty, i)
            vignettes.append(v)
            if not v['verification']['valid']:
                tqdm.write(f"  WARN [{spec_list[0]['id']}]: {v['verification']['issues']}")
            time.sleep(0.3)
        except Exception as e:
            tqdm.write(f"  Error [{spec_list[0]['id']}]: {e}")

    # Summary
    valid_count = sum(1 for v in vignettes if v['verification']['valid'])
    print(f"\nGenerated {len(vignettes)} vignettes ({valid_count} passed verification)")

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_file = output_dir / f"vignettes_{timestamp}.json"
    with open(output_file, 'w') as f:
        json.dump(vignettes, f, indent=2)

    print(f"Saved to {output_file}")


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--count', type=int, default=50)
    parser.add_argument('--output', type=Path, default=None)
    args = parser.parse_args()
    generate_vignettes(args.count, args.output)
