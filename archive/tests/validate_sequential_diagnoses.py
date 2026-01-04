"""
Validate Sequential Baseline Diagnoses Against Ground Truth

This script loads sequential experiment results and validates whether the
final diagnoses match the vignette ground truth labels. This is critical
because batch experiment comparison only measures agreement between batch
and sequential modes, not correctness against ground truth.

Author: Alfie Roberts
Date: January 2026
"""

import json
import sys
from pathlib import Path
from typing import Dict, List, Tuple

# Add project root to path for imports
project_root = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(project_root))

from src.reasoning.engine import PrologEngine
from src.search.manager import SessionManager


def load_sequential_results(results_file: Path) -> List[Dict]:
    """Load sequential mode results from JSON file."""
    with open(results_file) as f:
        return json.load(f)


def get_final_diagnosis(questions_list: List[Dict], ground_truth: str) -> Tuple[List[Tuple[str, float]], List[str], str]:
    """
    Determine ALL diagnoses that meet full DSM-5-TR criteria.

    This function queries diagnosis_candidate/3 which only succeeds when ALL
    criteria are met (symptoms, duration, onset, exclusions, subjective).

    Args:
        questions_list: List of {id, text, answer} dicts from batch experiment
        ground_truth: Expected disorder (for reporting only, not used in logic)

    Returns:
        (valid_diagnoses, diagnosis_ids, top_diagnosis)
        - valid_diagnoses: List of (disorder, confidence) tuples for ALL that meet criteria
        - diagnosis_ids: List of disorder IDs that meet full DSM-5-TR criteria
        - top_diagnosis: Top diagnosis ID (for backwards compatibility)
    """
    manager = SessionManager()
    manager.start_new_session()

    # Answer each question in Prolog using IDs directly (no mapping needed!)
    for question in questions_list:
        question_id = question['id']
        answer = question['answer']
        manager.answer_question(question_id, answer)

    # Get all active candidates
    active = manager.state.active_candidates

    if not active:
        return [], [], "none"

    # Query diagnosis_candidate for EACH active disorder
    # This predicate only succeeds if ALL DSM-5-TR criteria are met
    valid_diagnoses = []
    diagnosis_ids = []

    for disorder in active:
        query = f"diagnosis_candidate('{manager.state.patient_id}', {disorder}, Conf)"
        res = list(manager.prolog.query(query))

        if res:  # diagnosis_candidate succeeded = meets all criteria
            conf = float(res[0]['Conf'])
            valid_diagnoses.append((disorder, conf))
            diagnosis_ids.append(disorder)

    # Sort by confidence (highest first), use disorder ID as tiebreaker
    sorted_valid = sorted(valid_diagnoses, key=lambda x: (-x[1], x[0]))

    # Get top diagnosis for backwards compatibility
    top_diagnosis = sorted_valid[0][0] if sorted_valid else "none"

    return sorted_valid, diagnosis_ids, top_diagnosis


def score_diagnosis(predicted_ids: List[str], ground_truth) -> Dict:
    """
    Score diagnosis with separate metrics for exact vs partial matches.

    Supports both single disorder and comorbid cases with detailed match tracking.

    Args:
        predicted_ids: List of disorder IDs that meet full DSM-5-TR criteria
        ground_truth: String (single disorder) or list (comorbid)

    Returns:
        Dict with:
        - is_correct: bool - True if exact match
        - match_type: str - 'exact', 'partial_under', 'partial_over', 'partial_mixed', 'incorrect'
        - correct: List - Disorders correctly identified
        - missed: List - Disorders in truth but not predicted
        - extra: List - Disorders predicted but not in truth
        - is_comorbid_truth: bool - Whether ground truth is comorbid
        - is_comorbid_pred: bool - Whether prediction is comorbid
        - partial_score: float - Proportion of truth disorders found (0.0-1.0)
    """
    # Normalize ground truth to set
    if isinstance(ground_truth, str):
        truth_set = {ground_truth}
    elif isinstance(ground_truth, list):
        truth_set = set(ground_truth)
    else:
        truth_set = set()

    pred_set = set(predicted_ids)

    # Calculate overlap
    correct = pred_set & truth_set  # Intersection - what we got right
    missed = truth_set - pred_set   # In truth but not predicted
    extra = pred_set - truth_set    # Predicted but not in truth

    # Determine match type
    if pred_set == truth_set:
        match_type = 'exact'
        is_correct = True
    elif len(correct) > 0 and len(extra) == 0:
        match_type = 'partial_under'  # Found some, missed others, no false positives
        is_correct = False
    elif len(correct) > 0 and len(missed) == 0:
        match_type = 'partial_over'   # Found all + extras (false positives)
        is_correct = False
    elif len(correct) > 0:
        match_type = 'partial_mixed'  # Some correct, some missed, some extra
        is_correct = False
    else:
        match_type = 'incorrect'      # No overlap at all
        is_correct = False

    return {
        'is_correct': is_correct,
        'match_type': match_type,
        'correct': sorted(list(correct)),
        'missed': sorted(list(missed)),
        'extra': sorted(list(extra)),
        'is_comorbid_truth': len(truth_set) > 1,
        'is_comorbid_pred': len(pred_set) > 1,
        'partial_score': len(correct) / len(truth_set) if len(truth_set) > 0 else 0.0
    }


def _build_question_id_map(manager: SessionManager) -> Dict[str, str]:
    """
    Build mapping using formatted question text to match answers.

    Uses manager.get_symptom_description() to get text with prefixes,
    matching the format used when storing answers.

    Returns:
        Dict mapping formatted question text -> question ID
    """
    prolog = manager.prolog
    mapping = {}

    # Get all disorders
    try:
        disorders = list(prolog.query("disorder(ID, Name, Category)"))

        for disorder_result in disorders:
            disorder_id = disorder_result['ID']

            # Get all symptom IDs and use get_symptom_description for formatted text
            symptoms = list(prolog.query(
                f"symptom({disorder_id}, SymptomID, Category, Description)"
            ))

            for symptom in symptoms:
                symptom_id = symptom['SymptomID']
                # Use get_symptom_description to get formatted text (no prefix for symptoms)
                formatted_text = manager.get_symptom_description(symptom_id)
                if formatted_text:
                    mapping[formatted_text] = symptom_id

            # Get subjective criteria IDs
            subjective = list(prolog.query(
                f"subjective_criterion({disorder_id}, SubjID, Category, Question)"
            ))

            for subj in subjective:
                subj_id = subj['SubjID']
                # Use get_symptom_description to get formatted text with prefix
                formatted_text = manager.get_symptom_description(subj_id)
                if formatted_text:
                    mapping[formatted_text] = subj_id

            # Get exclusion criteria IDs
            exclusions = list(prolog.query(
                f"exclusion_criterion({disorder_id}, ExcID, Type, Question)"
            ))

            for exc in exclusions:
                exc_id = exc['ExcID']
                # Use get_symptom_description to get formatted text with prefix
                formatted_text = manager.get_symptom_description(exc_id)
                if formatted_text:
                    mapping[formatted_text] = exc_id

            # Add duration check questions
            dur_check = list(prolog.query(
                f"duration_requirement({disorder_id}, _, _)"
            ))
            if dur_check:
                dur_id = f"{disorder_id}_duration_check"
                formatted_text = manager.get_symptom_description(dur_id)
                if formatted_text:
                    mapping[formatted_text] = dur_id

            # Add onset age check questions
            onset_age = list(prolog.query(
                f"onset_requirement({disorder_id}, before_age, _)"
            ))
            for _ in onset_age:
                onset_id = f"{disorder_id}_onset_age_check"
                formatted_text = manager.get_symptom_description(onset_id)
                if formatted_text:
                    mapping[formatted_text] = onset_id

            # Add onset event check questions
            onset_event = list(prolog.query(
                f"onset_requirement({disorder_id}, after_event, _)"
            ))
            for _ in onset_event:
                onset_id = f"{disorder_id}_onset_event_check"
                formatted_text = manager.get_symptom_description(onset_id)
                if formatted_text:
                    mapping[formatted_text] = onset_id

    except Exception as e:
        print(f"Warning: Error building question map: {e}")

    return mapping


def validate_experiment_results(results_file: Path) -> Dict:
    """
    Validate all vignettes with separate comorbidity metrics.

    Returns:
        Dict with overall stats, single-disorder stats, comorbid stats, and details
    """
    results = load_sequential_results(results_file)

    total = len(results)
    exact_matches = 0
    partial_matches = 0

    # Separate stats for single vs comorbid
    single_total = 0
    single_correct = 0
    comorbid_total = 0
    comorbid_exact = 0
    comorbid_partial = 0

    details = []

    print(f"\nValidating {total} vignettes from {results_file.name}")
    print("=" * 80)

    for result in results:
        vignette_id = result["vignette_id"]
        ground_truth = result["ground_truth"]
        questions_list = result["questions"]

        try:
            # Get ALL valid diagnoses (those meeting full DSM-5-TR criteria)
            valid_diagnoses, diagnosis_ids, top_diagnosis = get_final_diagnosis(
                questions_list, ground_truth
            )

            # Score with comorbidity support
            score = score_diagnosis(diagnosis_ids, ground_truth)

            # Track stats
            if score['is_correct']:
                exact_matches += 1

            if len(score['correct']) > 0:
                partial_matches += 1

            if score['is_comorbid_truth']:
                comorbid_total += 1
                if score['is_correct']:
                    comorbid_exact += 1
                if len(score['correct']) > 0:
                    comorbid_partial += 1
            else:
                single_total += 1
                if score['is_correct']:
                    single_correct += 1

            # Format status
            status_symbol = "✓" if score['is_correct'] else "✗"
            match_label = score['match_type'].upper()

            detail = {
                "vignette_id": vignette_id,
                "ground_truth": ground_truth,
                "predicted_all": diagnosis_ids,
                "predicted_top": top_diagnosis,
                "valid_diagnoses": valid_diagnoses,
                **score,
                "num_questions": result["num_questions"]
            }
            details.append(detail)

            # Print result
            print(f"\n{vignette_id}")
            print(f"  Ground Truth:  {ground_truth}")
            print(f"  Predicted:     {diagnosis_ids if len(diagnosis_ids) > 1 else top_diagnosis}")
            if valid_diagnoses:
                print(f"  Confidences:   {[(d, f'{c:.2f}') for d, c in valid_diagnoses]}")
            print(f"  Match Type:    {match_label}")
            if score['correct']:
                print(f"  Correct:       {score['correct']}")
            if score['missed']:
                print(f"  Missed:        {score['missed']}")
            if score['extra']:
                print(f"  Extra:         {score['extra']}")
            print(f"  Status:        {status_symbol} {match_label}")
            print(f"  Questions:     {result['num_questions']}")

        except Exception as e:
            print(f"\n{vignette_id}: ERROR - {e}")
            import traceback
            traceback.print_exc()
            detail = {
                "vignette_id": vignette_id,
                "ground_truth": ground_truth,
                "error": str(e),
                "is_correct": False
            }
            details.append(detail)

    # Calculate accuracies
    exact_accuracy = (exact_matches / total * 100) if total > 0 else 0
    partial_accuracy = (partial_matches / total * 100) if total > 0 else 0
    single_accuracy = (single_correct / single_total * 100) if single_total > 0 else 0
    comorbid_exact_accuracy = (comorbid_exact / comorbid_total * 100) if comorbid_total > 0 else 0
    comorbid_partial_accuracy = (comorbid_partial / comorbid_total * 100) if comorbid_total > 0 else 0

    summary = {
        "total": total,
        "exact_matches": exact_matches,
        "partial_matches": partial_matches,
        "exact_accuracy": exact_accuracy,
        "partial_accuracy": partial_accuracy,
        "single_disorder": {
            "total": single_total,
            "correct": single_correct,
            "accuracy": single_accuracy
        },
        "comorbid": {
            "total": comorbid_total,
            "exact_matches": comorbid_exact,
            "partial_matches": comorbid_partial,
            "exact_accuracy": comorbid_exact_accuracy,
            "partial_accuracy": comorbid_partial_accuracy
        },
        "details": details
    }

    # Print summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"Total Vignettes:           {total}")
    print(f"  Single-disorder:         {single_total}")
    print(f"  Comorbid:                {comorbid_total}")
    print()
    print(f"Exact Matches:             {exact_matches} ({exact_accuracy:.1f}%)")
    print(f"Partial Matches (≥1 correct): {partial_matches} ({partial_accuracy:.1f}%)")
    print()
    print(f"Single-Disorder Accuracy:  {single_accuracy:.1f}% ({single_correct}/{single_total})")
    if comorbid_total > 0:
        print(f"Comorbid Exact:            {comorbid_exact_accuracy:.1f}% ({comorbid_exact}/{comorbid_total})")
        print(f"Comorbid Partial:          {comorbid_partial_accuracy:.1f}% ({comorbid_partial}/{comorbid_total})")

    return summary


def main():
    """Main entry point."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Validate sequential baseline diagnoses against ground truth"
    )
    parser.add_argument(
        "--results-file",
        type=str,
        required=True,
        help="Path to sequential results JSON (e.g., outputs/batch_experiments/exp_*/openai_sequential.json)"
    )
    parser.add_argument(
        "--save",
        action="store_true",
        help="Save validation report to same directory as results"
    )

    args = parser.parse_args()

    results_file = Path(args.results_file)
    if not results_file.exists():
        print(f"Error: Results file not found: {results_file}")
        sys.exit(1)

    # Run validation
    summary = validate_experiment_results(results_file)

    # Save if requested
    if args.save:
        output_file = results_file.parent / f"{results_file.stem}_validation.json"
        with open(output_file, 'w') as f:
            json.dump(summary, f, indent=2)
        print(f"\n✓ Saved validation report to: {output_file}")


if __name__ == "__main__":
    main()
