"""Debug question mapping to see what's matching."""
import json
import sys
from pathlib import Path

# Add project root to path for imports
project_root = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(project_root))

from src.search.manager import SessionManager
from tests.validate_sequential_diagnoses import _build_question_id_map

# Load first vignette
with open('outputs/batch_experiments/exp_20260103_144737/openai_sequential.json') as f:
    results = json.load(f)

first = results[0]

# Build mapping
manager = SessionManager()
manager.start_new_session()
question_map = _build_question_id_map(manager)

print(f'Total questions in map: {len(question_map)}')
print(f'Total questions in answers: {len(first["answers"])}')

# Check match rate
matched = 0
unmatched_samples = []
matched_samples = []

for q_text, answer in first['answers'].items():
    if q_text in question_map:
        matched += 1
        if len(matched_samples) < 5:
            matched_samples.append((q_text[:60], question_map[q_text], answer))
    else:
        if len(unmatched_samples) < 10:
            unmatched_samples.append((q_text[:80], answer))

print(f'\nMatched: {matched}/{len(first["answers"])} ({matched/len(first["answers"])*100:.1f}%)')

print(f'\nSample MATCHED questions:')
for q, qid, ans in matched_samples:
    print(f'  {qid:30s} "{q}..." = {ans}')

print(f'\nSample UNMATCHED questions:')
for q, ans in unmatched_samples:
    print(f'  "{q}..." = {ans}')

# Check specific duration and subjective questions
print('\n\nDuration/Subjective questions in answers:')
for q_text, answer in first['answers'].items():
    if 'duration' in q_text.lower() or 'adhere' in q_text.lower() or 'distress' in q_text.lower():
        status = '✓ MAPPED' if q_text in question_map else '✗ NOT MAPPED'
        print(f'  {status}: {q_text[:100]}... = {answer}')
        if len([x for x in first['answers'].keys() if 'duration' in x.lower() or 'adhere' in x.lower()]) < 15:
            continue
        break
