"""Debug to check if duration/subjective assertions are working."""
import json
import sys
from pathlib import Path

# Add project root to path for imports
project_root = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(project_root))

from src.search.manager import SessionManager
from tests.validate_sequential_diagnoses import _build_question_id_map

# Load first vignette (GAD)
with open('outputs/batch_experiments/exp_20260103_144737/openai_sequential.json') as f:
    results = json.load(f)

first = results[0]  # GAD vignette
print(f"Vignette: {first['vignette_id']} (ground_truth: {first['ground_truth']})")

# Build mapping and answer questions
manager = SessionManager()
manager.start_new_session()
question_map = _build_question_id_map(manager)

# Answer all questions
questions_answered = 0
for q_text, answer in first['answers'].items():
    q_id = question_map.get(q_text)
    if q_id:
        manager.answer_question(q_id, answer)
        questions_answered += 1

print(f"Answered {questions_answered} questions")

# Check if GAD duration was asserted
print("\n=== Checking Duration Assertions ===")
query = f"patient_duration('{manager.state.patient_id}', gad, Duration)"
res = list(manager.prolog.query(query))
if res:
    print(f"✓ GAD duration asserted: {res[0]['Duration']} days")
else:
    print(f"✗ GAD duration NOT asserted")

    # Check if duration question was answered
    print("\nDuration questions answered:")
    for q_text, ans in first['answers'].items():
        if 'gad_duration_check' in question_map.get(q_text, ''):
            print(f"  {q_text[:80]}... = {ans}")
            print(f"  Mapped to ID: {question_map[q_text]}")

# Check if GAD subjective criteria were asserted
print("\n=== Checking Subjective Assertions ===")
query2 = f"subjective_assessment('{manager.state.patient_id}', CritID, Status, Conf)"
res2 = list(manager.prolog.query(query2))
print(f"Total subjective assessments: {len(res2)}")

# Check for GAD-specific subjective
gad_subjective = [r for r in res2 if str(r['CritID']).startswith('gad_subj_')]
print(f"GAD subjective assessments: {len(gad_subjective)}")
for r in gad_subjective:
    print(f"  {r['CritID']}: {r['Status']} (conf: {r['Conf']})")

if len(gad_subjective) == 0:
    print("\n✗ No GAD subjective assessments found!")
    print("\nGAD subjective questions in answers:")
    for q_text, ans in first['answers'].items():
        q_id = question_map.get(q_text, '')
        if q_id.startswith('gad_subj_'):
            print(f"  {q_id}: {q_text[:60]}... = {ans}")

# Check actual meets_subjective_criteria
print("\n=== Checking meets_subjective_criteria ===")
query3 = "meets_subjective_criteria(current_patient, gad)"
res3 = list(manager.prolog.query(query3))
print(f"meets_subjective_criteria(gad): {'✓ MET' if res3 else '✗ NOT MET'}")

# List all GAD subjective criteria defined
query4 = "subjective_criterion(gad, CritID, Category, Question)"
res4 = list(manager.prolog.query(query4))
print(f"\nGAD subjective criteria defined in Prolog: {len(res4)}")
for r in res4:
    print(f"  {r['CritID']}: {r['Question'][:60]}...")

print("\n=== Checking meets_duration_criteria ===")
query5 = "meets_duration_criteria(current_patient, gad)"
res5 = list(manager.prolog.query(query5))
print(f"meets_duration_criteria(gad): {'✓ MET' if res5 else '✗ NOT MET'}")
