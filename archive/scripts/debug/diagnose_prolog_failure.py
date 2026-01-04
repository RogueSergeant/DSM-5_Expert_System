"""
Diagnostic script to understand why diagnosis_candidate is failing.

Checks each of the 5 criteria individually to see which one is blocking
diagnosis confirmation.

Author: Alfie Roberts
Date: January 2026
"""

import json
import sys
from pathlib import Path

# Add project root to path for imports
project_root = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(project_root))

from src.search.manager import SessionManager


def check_criteria_for_case(answers, ground_truth, vignette_id):
    """
    Check each criterion individually to diagnose failures.

    Args:
        answers: Dict mapping question text to YES/NO/UNKNOWN
        ground_truth: Expected disorder
        vignette_id: Vignette ID for reporting
    """
    manager = SessionManager()
    manager.start_new_session()

    # Build question map
    prolog = manager.prolog
    mapping = {}
    try:
        disorders = list(prolog.query("disorder(ID, Name, Category)"))
        for disorder_result in disorders:
            disorder_id = disorder_result['ID']

            symptoms = list(prolog.query(
                f"symptom({disorder_id}, SymptomID, Category, Description)"
            ))
            for symptom in symptoms:
                mapping[symptom['Description']] = symptom['SymptomID']

            subjective = list(prolog.query(
                f"subjective_criterion({disorder_id}, SubjID, Category, Question)"
            ))
            for subj in subjective:
                mapping[subj['Question']] = subj['SubjID']

            exclusions = list(prolog.query(
                f"exclusion_criterion({disorder_id}, ExcID, Type, Question)"
            ))
            for exc in exclusions:
                mapping[exc['Question']] = exc['ExcID']
    except Exception as e:
        print(f"Error building question map: {e}")
        return

    # Answer all questions
    for question_text, answer in answers.items():
        question_id = mapping.get(question_text)
        if question_id:
            manager.answer_question(question_id, answer)

    # Check each criterion for expected disorder
    if isinstance(ground_truth, list):
        disorders_to_check = ground_truth
    else:
        disorders_to_check = [ground_truth]

    print(f"\n{'='*80}")
    print(f"Vignette: {vignette_id}")
    print(f"Expected: {ground_truth}")
    print(f"{'='*80}")

    for disorder in disorders_to_check:
        print(f"\nChecking criteria for: {disorder}")
        print("-" * 40)

        # Check each criterion
        criteria = {
            'symptoms': f"meets_symptom_criteria('{manager.state.patient_id}', {disorder})",
            'duration': f"meets_duration_criteria('{manager.state.patient_id}', {disorder})",
            'onset': f"meets_onset_criteria('{manager.state.patient_id}', {disorder})",
            'exclusions': f"meets_exclusion_criteria('{manager.state.patient_id}', {disorder})",
            'subjective': f"meets_subjective_criteria('{manager.state.patient_id}', {disorder})"
        }

        for criterion_name, query in criteria.items():
            try:
                result = list(prolog.query(query))
                status = "✓ MET" if result else "✗ NOT MET"
                print(f"  {criterion_name:12s}: {status}")
            except Exception as e:
                print(f"  {criterion_name:12s}: ERROR - {e}")

        # Try full diagnosis_candidate
        try:
            query = f"diagnosis_candidate('{manager.state.patient_id}', {disorder}, Conf)"
            result = list(prolog.query(query))
            if result:
                conf = result[0]['Conf']
                print(f"\n  → diagnosis_candidate: ✓ SUCCESS (confidence: {conf})")
            else:
                print(f"\n  → diagnosis_candidate: ✗ FAILED (one or more criteria not met)")
        except Exception as e:
            print(f"\n  → diagnosis_candidate: ERROR - {e}")


def main():
    results_file = Path("outputs/batch_experiments/exp_20260103_144737/openai_sequential.json")

    with open(results_file) as f:
        results = json.load(f)

    print(f"Loaded {len(results)} vignettes")
    print("\nDiagnosing Prolog criterion failures...")

    # Check first 3 vignettes (including the comorbid one at index 9)
    for i in [0, 1, 9]:  # First, second, and comorbid case
        result = results[i]
        check_criteria_for_case(
            result['answers'],
            result['ground_truth'],
            result['vignette_id']
        )


if __name__ == "__main__":
    main()
