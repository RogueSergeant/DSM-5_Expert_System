"""
Vignette Generation - creates clinical vignettes with Prolog-keyed answers.

Usage:
    python -m src.evaluation.generate_vignettes --count 50
"""

import argparse
import json
import random
import time
from datetime import datetime
from pathlib import Path

from openai import OpenAI
from src.reasoning.engine import PrologEngine
from src.extraction.config import Config

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
    """Load disorder specs from Prolog KB."""
    specs = {}
    for d in engine.query("disorder(ID, Name, Category)"):
        did = str(d['ID'])
        if did == 'ptsd_preschool':
            continue

        specs[did] = {
            'id': did, 'name': str(d['Name']),
            'symptoms': [{'id': str(s['SID']), 'desc': str(s['Desc'])[:100]}
                         for s in engine.query(f"symptom({did}, SID, _, Desc)")],
            'exclusions': [{'id': str(e['ExcID']), 'desc': str(e['Desc'])[:80]}
                           for e in engine.query(f"exclusion_criterion({did}, ExcID, _, Desc)")],
            'subjective': [{'id': str(s['CritID']), 'desc': str(s['Desc'])[:80]}
                           for s in engine.query(f"subjective_criterion({did}, CritID, Desc, _)")],
            'categories': engine.query(f"symptom_category({did}, CatID, SymList, Req, ReqType)"),
            'duration': engine.query_one(f"duration_requirement({did}, N, Unit)"),
            'onset': engine.query_one(f"onset_requirement({did}, Type, Val)"),
        }
    return specs


def get_required_count(spec: dict) -> int:
    """Get minimum required symptom count."""
    for cat in spec['categories']:
        if len(cat['SymList']) > 5:
            return cat['Req']
    return sum(c['Req'] for c in spec['categories']) if spec['categories'] else 5


def days_from_duration(dur: dict) -> int:
    """Convert duration dict to days."""
    if not dur:
        return 14
    return dur['N'] * {'days': 1, 'weeks': 7, 'months': 30, 'years': 365}.get(str(dur['Unit']), 1)


def generate_answers_single(spec: dict, difficulty: str) -> tuple[dict, bool]:
    """Generate answers for single disorder."""
    answers = {}
    required = get_required_count(spec)
    symptoms = spec['symptoms'].copy()
    random.shuffle(symptoms)

    if difficulty == 'CLEAR':
        for s in symptoms:
            answers[s['id']] = 'present'
        meets = True
    elif difficulty == 'MODERATE':
        for i, s in enumerate(symptoms):
            answers[s['id']] = 'present' if i < required else 'absent'
        meets = True
    else:  # AMBIGUOUS
        meets = random.random() < 0.5
        target = required if meets else required - 1
        for i, s in enumerate(symptoms):
            answers[s['id']] = 'present' if i < target else 'absent'

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
    """Generate answers for comorbid case - meets criteria for all disorders."""
    answers = {}

    for spec in specs:
        # All symptoms present
        for s in spec['symptoms']:
            answers[s['id']] = 'present'
        # All exclusions cleared
        for e in spec['exclusions']:
            answers[e['id']] = 'cleared'
        # All subjective met
        for subj in spec['subjective']:
            answers[subj['id']] = 'met'
        # Duration: use longest requirement
        base_days = days_from_duration(spec['duration'])
        answers['duration_days'] = max(answers.get('duration_days', 0), base_days + 14)
        # Onset: use earliest if applicable
        if spec['onset'] and str(spec['onset']['Type']) == 'before_age':
            if 'onset_age' not in answers:
                answers['onset_age'] = spec['onset']['Val'] - 3

    return answers


def build_prompt(specs: list[dict], answers: dict, difficulty: str) -> str:
    """Build LLM prompt."""
    disorder_names = ' and '.join(s['name'] for s in specs)

    symptom_lines = []
    for spec in specs:
        symptom_lines.extend(
            f"- {s['id']}: {answers[s['id']]} - {s['desc']}" for s in spec['symptoms']
        )
    excl_lines = []
    for spec in specs:
        excl_lines.extend(
            f"- {e['id']}: cleared - {e['desc']}" for e in spec['exclusions']
        )
    subj_lines = []
    for spec in specs:
        subj_lines.extend(
            f"- {s['id']}: {answers[s['id']]} - {s['desc']}" for s in spec['subjective']
        )

    comorbid_note = ""
    if len(specs) > 1:
        comorbid_note = f"\nThis is a COMORBID case: patient meets criteria for BOTH {disorder_names}. Ensure symptoms from all disorders are clearly present.\n"

    return f"""Generate a clinical vignette for evaluation of {disorder_names}.

DIFFICULTY: {difficulty}
{comorbid_note}
FINDINGS TO MATCH:

Symptoms:
{chr(10).join(symptom_lines)}

Exclusions (cleared = does NOT apply):
{chr(10).join(excl_lines)}

Clinical judgment:
{chr(10).join(subj_lines)}

Duration: {answers.get('duration_days', 14)} days

Write a 300-500 word clinical vignette including:
- Demographics
- Chief complaint with patient quotes
- History of present illness
- Mental status findings

For 'present' symptoms: describe clearly
For 'absent' symptoms: note absence or contradicting evidence

Output ONLY the clinical text."""


def generate_vignette(client: OpenAI, specs: list[dict], difficulty: str, idx: int) -> dict:
    """Generate one vignette."""
    if len(specs) > 1:  # Comorbid
        answers = generate_answers_comorbid(specs)
        meets = True
    else:
        answers, meets = generate_answers_single(specs[0], difficulty)

    prompt = build_prompt(specs, answers, difficulty)

    response = client.chat.completions.create(
        model="gpt-5-mini",
        messages=[
            {"role": "system", "content": "You are an expert psychiatrist writing clinical vignettes. Be realistic and clinically precise."},
            {"role": "user", "content": prompt}
        ],
        max_completion_tokens=1800
    )

    disorder_ids = [s['id'] for s in specs]
    difficulty_label = 'COMORBID' if len(specs) > 1 else difficulty

    return {
        "id": f"vig_{idx:03d}_{'_'.join(disorder_ids)}_{difficulty_label.lower()}",
        "ground_truth": disorder_ids,
        "difficulty": difficulty_label,
        "meets_criteria": meets,
        "clinical_text": response.choices[0].message.content.strip(),
        "answers": answers
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
    output_dir = output_dir or Path(__file__).parent.parent.parent / 'data' / 'vignettes'
    output_dir.mkdir(parents=True, exist_ok=True)

    # Load KB
    prolog_dir = Path(__file__).parent.parent / 'prolog'
    engine = PrologEngine(prolog_dir)
    engine.load_file('schema.pl')
    engine.load_file('gold_standard/loader.pl')
    specs = load_disorders(engine)
    disorder_ids = list(specs.keys())

    print(f"Loaded {len(specs)} disorders: {disorder_ids}")

    # OpenAI client
    config = Config.from_env()
    client = OpenAI(api_key=config.openai_api_key)

    vignettes = []
    for i in range(count):
        case_type = select_case_type()

        if case_type == 'COMORBID':
            pair = random.choice(COMORBID_PAIRS)
            spec_list = [specs[pair[0]], specs[pair[1]]]
            difficulty = 'COMORBID'
        else:
            disorder_id = disorder_ids[i % len(disorder_ids)]
            spec_list = [specs[disorder_id]]
            difficulty = case_type

        print(f"[{i+1}/{count}] {[s['id'] for s in spec_list]} - {difficulty}")
        try:
            v = generate_vignette(client, spec_list, difficulty, i)
            vignettes.append(v)
            time.sleep(0.3)
        except Exception as e:
            print(f"  Error: {e}")

    # Save
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_file = output_dir / f"vignettes_{timestamp}.json"
    with open(output_file, 'w') as f:
        json.dump(vignettes, f, indent=2)

    print(f"\nSaved {len(vignettes)} vignettes to {output_file}")


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--count', type=int, default=50)
    parser.add_argument('--output', type=Path, default=None)
    args = parser.parse_args()
    generate_vignettes(args.count, args.output)
