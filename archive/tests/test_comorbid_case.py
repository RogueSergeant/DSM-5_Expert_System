"""Test the comorbid case to see if system can detect both diagnoses."""

import json
import sys
from pathlib import Path

# Add project root to path for imports
project_root = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(project_root))

from src.search.manager import SessionManager

# Load the comorbid case
with open('outputs/batch_experiments/exp_20260103_144737/openai_sequential.json') as f:
    results = json.load(f)

comorbid = results[9]  # The comorbid case
print(f"Vignette: {comorbid['vignette_id']}")
print(f"Ground truth: {comorbid['ground_truth']}")
print(f"Questions asked: {comorbid['num_questions']}")
print()

# Replay into Prolog to check both disorders
manager = SessionManager()
manager.start_new_session()

# Build question map
prolog = manager.prolog
mapping = {}
disorders = list(prolog.query("disorder(ID, Name, Category)"))
for d_result in disorders:
    did = d_result['ID']

    symptoms = list(prolog.query(f"symptom({did}, SID, Cat, Desc)"))
    for s in symptoms:
        mapping[s['Desc']] = s['SID']

    subj = list(prolog.query(f"subjective_criterion({did}, SID, Cat, Q)"))
    for s in subj:
        mapping[s['Q']] = s['SID']

    exc = list(prolog.query(f"exclusion_criterion({did}, EID, Type, Q)"))
    for e in exc:
        mapping[e['Q']] = e['EID']

# Answer all questions
for qtext, ans in comorbid['answers'].items():
    qid = mapping.get(qtext)
    if qid:
        manager.answer_question(qid, ans)

# Check both diagnoses
for disorder in ['ptsd', 'mdd']:
    print(f'\nChecking {disorder.upper()}:')
    print("=" * 60)

    # Try diagnosis_candidate
    query = f"diagnosis_candidate('{manager.state.patient_id}', {disorder}, Conf)"
    result = list(prolog.query(query))

    if result:
        print(f'  ✓ diagnosis_candidate SUCCESS (conf: {result[0]["Conf"]})')
    else:
        print(f'  ✗ diagnosis_candidate FAILED')
        print(f'  Checking individual criteria:')

        criteria = ['symptom', 'duration', 'onset', 'exclusion', 'subjective']
        for crit in criteria:
            q = f"meets_{crit}_criteria('{manager.state.patient_id}', {disorder})"
            r = list(prolog.query(q))
            status = '✓ MET' if r else '✗ NOT MET'
            print(f'    {crit:12s}: {status}')

            # If subjective failed, show which ones are missing
            if crit == 'subjective' and not r:
                # Show all subjective criteria for this disorder
                subj_criteria = list(prolog.query(f"subjective_criterion({disorder}, CritID, Cat, Q)"))
                print(f'\n    Subjective criteria defined for {disorder}: {len(subj_criteria)}')
                for sc in subj_criteria:
                    # Check if assessed
                    assessment = list(prolog.query(f"subjective_assessment('{manager.state.patient_id}', {sc['CritID']}, Status, Conf)"))
                    if assessment:
                        print(f'      ✓ {sc["CritID"]}: {assessment[0]["Status"]} (conf: {assessment[0]["Conf"]})')
                    else:
                        print(f'      ✗ {sc["CritID"]}: NOT ASSESSED')
