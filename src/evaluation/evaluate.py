"""
Vignette Evaluation - tests diagnostic driver against generated vignettes.

Usage:
    python -m src.evaluation.evaluate --vignettes data/vignettes/*.json
    python -m src.evaluation.evaluate --vignettes data/vignettes/*.json --mode llm
    python -m src.evaluation.evaluate --vignettes data/vignettes/*.json --mode interactive
    python -m src.evaluation.evaluate --vignettes data/vignettes/*.json --subjective-model claude
"""

import argparse
import json
import logging
from dataclasses import dataclass, asdict, field
from datetime import datetime
from pathlib import Path
from typing import Optional

from src.diagnosis.driver import DiagnosticDriver, DiagnosisResult
from src.evaluation.answer_modes import (
    create_preextracted_answer_fn,
    create_interactive_answer_fn,
    create_llm_answer_fn,
    create_subjective_llm_answer_fn,
    create_hybrid_answer_fn,
)
from src.utils.formatting import (
    format_header,
    format_table,
    format_metric,
    format_run_header,
    format_vignette_result,
    status_badge,
)
from src.utils.explain import format_proof_tree

# Logging setup
LOG_DIR = Path(__file__).parent.parent.parent / 'logs' / 'evaluation'
LOG_DIR.mkdir(parents=True, exist_ok=True)
TIMESTAMP = datetime.now().strftime('%Y%m%d_%H%M%S')
LOG_FILE = LOG_DIR / f"{TIMESTAMP}.log"

# Results output
RESULTS_DIR = Path(__file__).parent.parent.parent / 'data' / 'results' / 'evaluation'
RESULTS_DIR.mkdir(parents=True, exist_ok=True)
RESULTS_FILE = RESULTS_DIR / f"{TIMESTAMP}_results.json"

# Configure file handler for all evaluation loggers
file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter('%(asctime)s | %(levelname)s | %(message)s'))

# Add handler to this module (handles __main__ when run directly), answer_modes, and driver
for logger_name in ['src.evaluation.evaluate', 'src.evaluation.answer_modes', 'src.diagnosis.driver', '__main__']:
    lg = logging.getLogger(logger_name)
    lg.setLevel(logging.DEBUG)
    lg.addHandler(file_handler)

logger = logging.getLogger(__name__)


@dataclass
class EvaluationResult:
    """Result from evaluating a single vignette."""
    vignette_id: str
    ground_truth: list[str]
    difficulty: str
    meets_criteria: bool
    predicted_disorder: str
    predicted_status: str
    correct: bool
    questions_asked: int
    confidence: float
    explanation: dict = None  # Proof tree explanation from Prolog


def evaluate_vignette(
    driver: DiagnosticDriver,
    vignette: dict,
    mode: str = 'preextracted',
    verbose: bool = False,
    subjective_model: str = 'none'
) -> list[EvaluationResult]:
    """Evaluate a single vignette using differential diagnosis.

    Args:
        driver: The diagnostic driver instance
        vignette: Vignette data dict
        mode: Answer mode ('preextracted', 'interactive', 'llm')
        verbose: Print detailed output
        subjective_model: LLM for subjective criteria ('none', 'claude', 'openai')
    """
    is_comorbid = len(vignette['ground_truth']) > 1
    disorders_str = '+'.join(vignette['ground_truth'])
    expected = 'meets' if vignette['meets_criteria'] else 'does_not_meet'

    logger.info(f"VIGNETTE | {vignette['id']} | disorders={disorders_str} | difficulty={vignette['difficulty']} | comorbid={is_comorbid} | expected={expected} | subjective_model={subjective_model}")

    if mode == 'preextracted':
        base_answer_fn = create_preextracted_answer_fn(vignette['answers'])
    elif mode == 'interactive':
        base_answer_fn = create_interactive_answer_fn(vignette['clinical_text'])
    elif mode == 'llm':
        # LLM mode handles all questions - subjective_model doesn't apply
        answer_fn = create_llm_answer_fn(vignette['clinical_text'])
    else:
        raise ValueError(f"Unknown mode: {mode}")

    # Apply hybrid routing for subjective criteria if LLM provider specified
    if subjective_model != 'none' and mode in ('preextracted', 'interactive'):
        llm_fn = create_subjective_llm_answer_fn(
            vignette['clinical_text'],
            provider=subjective_model
        )
        answer_fn = create_hybrid_answer_fn(
            base_answer_fn,
            llm_fn,
            interactive_override=(mode == 'interactive')
        )
    elif mode != 'llm':
        answer_fn = base_answer_fn

    # Extract patient age for age-based disorder pruning (e.g., ptsd vs ptsd_preschool)
    # In clinical settings, patient age is always known from the patient record
    patient_age = vignette['answers'].get('age')

    # Run differential diagnosis across ALL disorders
    diagnosis_results = driver.run_differential_diagnosis(
        patient_id=f"patient_{vignette['id']}",
        answer_fn=answer_fn,
        verbose=verbose,
        patient_age=patient_age
    )

    # Evaluate against ground truth
    results = []
    expected_status = 'met' if vignette['meets_criteria'] else 'not_met'

    patient_id = f"patient_{vignette['id']}"
    for disorder_id in vignette['ground_truth']:
        if disorder_id in diagnosis_results:
            result = diagnosis_results[disorder_id]
            correct = result.status == expected_status
        else:
            # Disorder was pruned - counts as 'not_met'
            correct = (expected_status == 'not_met')
            result = DiagnosisResult(disorder_id, disorder_id, 'pruned', 0.0, 0, 0)

        # Get explanation for this disorder
        explanation = driver.get_explanation(disorder_id, patient_id)

        logger.info(f"  RESULT | {disorder_id} | predicted={result.status} | expected={expected_status} | correct={correct} | questions={result.questions_asked} | confidence={result.confidence:.2f}")

        results.append(EvaluationResult(
            vignette_id=vignette['id'],
            ground_truth=vignette['ground_truth'],
            difficulty=vignette['difficulty'],
            meets_criteria=vignette['meets_criteria'],
            predicted_disorder=disorder_id,
            predicted_status=result.status,
            correct=correct,
            questions_asked=result.questions_asked,
            confidence=result.confidence,
            explanation=explanation
        ))

    return results


def load_vignettes(paths: list[Path], disorder: Optional[str] = None,
                   difficulty: Optional[str] = None) -> list[dict]:
    """Load vignettes from JSON file(s), optionally filtering."""
    vignettes = []
    for path in paths:
        logger.info(f"Loading vignettes from {path}")
        with open(path) as f:
            vignettes.extend(json.load(f))

    initial_count = len(vignettes)
    if disorder:
        vignettes = [v for v in vignettes if disorder in v['ground_truth']]
    if difficulty:
        vignettes = [v for v in vignettes if v['difficulty'] == difficulty.upper()]

    logger.info(f"Loaded {len(vignettes)} vignettes (from {initial_count}, filter: disorder={disorder}, difficulty={difficulty})")
    return vignettes


def evaluate_on_vignettes(
    vignettes_paths: list[Path],
    mode: str = 'preextracted',
    disorder: Optional[str] = None,
    difficulty: Optional[str] = None,
    verbose: bool = False,
    subjective_model: str = 'none'
) -> list[EvaluationResult]:
    """Run evaluation on vignettes and report metrics."""
    logger.info(f"Starting evaluation | mode={mode} | subjective_model={subjective_model} | paths={vignettes_paths}")
    print(f"Log file: {LOG_FILE}")

    vignettes = load_vignettes(vignettes_paths, disorder, difficulty)
    if not vignettes:
        print("No vignettes found matching filters")
        logger.warning("No vignettes found matching filters")
        return []

    # Print run header with metadata
    print(format_run_header(
        mode=mode,
        vignette_count=len(vignettes),
        disorder_filter=disorder,
        difficulty_filter=difficulty,
        subjective_model=subjective_model
    ))
    print()

    driver = DiagnosticDriver()
    if not driver.load():
        print("Failed to load Prolog engine")
        logger.error("Failed to load Prolog engine")
        return []

    logger.info("Prolog engine loaded successfully")

    all_results = []
    for v in vignettes:
        if verbose:
            print(f"\nEvaluating {v['id']} ({v['difficulty']})...")

        results = evaluate_vignette(driver, v, mode, verbose, subjective_model)
        all_results.extend(results)

        if verbose:
            for r in results:
                print(f"  {format_vignette_result(
                    vignette_id=r.vignette_id,
                    difficulty=r.difficulty,
                    ground_truth=r.ground_truth,
                    predicted=r.predicted_disorder,
                    status=r.predicted_status,
                    correct=r.correct,
                    questions=r.questions_asked
                )}")
                # Show proof tree in verbose mode
                if r.explanation:
                    print()
                    print(format_proof_tree(r.explanation))
                    print()

    print_metrics(all_results, mode, disorder, difficulty, subjective_model)
    save_results(all_results, mode, disorder, difficulty, subjective_model)
    logger.info(f"Evaluation complete | total={len(all_results)}")

    return all_results


def print_metrics(
    results: list[EvaluationResult],
    mode: str = 'preextracted',
    disorder_filter: str = None,
    difficulty_filter: str = None,
    subjective_model: str = 'none'
):
    """Print and log evaluation metrics with formatted tables."""
    if not results:
        print("No results to analyse")
        return

    total = len(results)
    correct = sum(1 for r in results if r.correct)
    accuracy = correct / total
    avg_questions = sum(r.questions_asked for r in results) / total

    # By difficulty
    difficulties = {}
    for r in results:
        if r.difficulty not in difficulties:
            difficulties[r.difficulty] = {'correct': 0, 'total': 0, 'questions': 0}
        difficulties[r.difficulty]['total'] += 1
        difficulties[r.difficulty]['questions'] += r.questions_asked
        if r.correct:
            difficulties[r.difficulty]['correct'] += 1

    # By disorder
    disorders = {}
    for r in results:
        if r.predicted_disorder not in disorders:
            disorders[r.predicted_disorder] = {'correct': 0, 'total': 0}
        disorders[r.predicted_disorder]['total'] += 1
        if r.correct:
            disorders[r.predicted_disorder]['correct'] += 1

    # Status breakdown
    statuses = {}
    for r in results:
        statuses[r.predicted_status] = statuses.get(r.predicted_status, 0) + 1

    # Log summary
    logger.info(f"RESULTS | accuracy={accuracy:.1%} ({correct}/{total}) | avg_questions={avg_questions:.1f}")
    for diff, stats in sorted(difficulties.items()):
        diff_acc = stats['correct'] / stats['total'] if stats['total'] > 0 else 0
        logger.info(f"  {diff}: {diff_acc:.1%} ({stats['correct']}/{stats['total']})")
    for disorder, stats in sorted(disorders.items()):
        dis_acc = stats['correct'] / stats['total'] if stats['total'] > 0 else 0
        logger.info(f"  {disorder}: {dis_acc:.1%} ({stats['correct']}/{stats['total']})")

    # Print results header
    print()
    print(format_header("Results"))
    print(format_metric("Overall Accuracy", accuracy, total))
    print(f"Average Questions: {avg_questions:.1f}")

    # Difficulty table
    print("\nBy Difficulty:")
    diff_rows = []
    for diff, stats in sorted(difficulties.items()):
        diff_acc = stats['correct'] / stats['total'] if stats['total'] > 0 else 0
        avg_q = stats['questions'] / stats['total'] if stats['total'] > 0 else 0
        diff_rows.append([
            diff,
            f"{diff_acc:.1%}",
            f"{stats['correct']}/{stats['total']}",
            f"{avg_q:.1f}"
        ])
    print(format_table(['Difficulty', 'Accuracy', 'Correct', 'Avg Qs'], diff_rows))

    # Disorder table
    print("\nBy Disorder:")
    disorder_rows = []
    for disorder, stats in sorted(disorders.items()):
        dis_acc = stats['correct'] / stats['total'] if stats['total'] > 0 else 0
        disorder_rows.append([
            disorder.upper(),
            f"{dis_acc:.1%}",
            f"{stats['correct']}/{stats['total']}"
        ])
    print(format_table(['Disorder', 'Accuracy', 'Correct'], disorder_rows))

    # Status breakdown
    print("\nPredicted Statuses:")
    for status, count in sorted(statuses.items()):
        print(f"  {status_badge(status)} {count}")


def _serialize_for_json(obj):
    """Convert Prolog objects to JSON-serializable format."""
    if obj is None:
        return None
    if isinstance(obj, (str, int, float, bool)):
        return obj
    if isinstance(obj, dict):
        return {str(k): _serialize_for_json(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_serialize_for_json(item) for item in obj]
    # Handle Prolog terms
    if hasattr(obj, 'functor') and hasattr(obj, 'args'):
        return {'functor': str(obj.functor), 'args': [_serialize_for_json(a) for a in obj.args]}
    return str(obj)


def save_results(
    results: list[EvaluationResult],
    mode: str,
    disorder_filter: str,
    difficulty_filter: str,
    subjective_model: str
):
    """Save evaluation results to JSON file."""
    total = len(results)
    correct = sum(1 for r in results if r.correct)

    # Serialize results, handling Prolog objects
    serialized_results = []
    for r in results:
        result_dict = asdict(r)
        result_dict['explanation'] = _serialize_for_json(r.explanation)
        serialized_results.append(result_dict)

    output = {
        'timestamp': TIMESTAMP,
        'mode': mode,
        'subjective_model': subjective_model,
        'filters': {
            'disorder': disorder_filter,
            'difficulty': difficulty_filter
        },
        'summary': {
            'total': total,
            'correct': correct,
            'accuracy': correct / total if total > 0 else 0,
            'avg_questions': sum(r.questions_asked for r in results) / total if total > 0 else 0
        },
        'results': serialized_results
    }

    with open(RESULTS_FILE, 'w') as f:
        json.dump(output, f, indent=2)

    print(f"\nResults saved to: {RESULTS_FILE}")


def main():
    parser = argparse.ArgumentParser(description="Evaluate diagnostic driver on vignettes")
    parser.add_argument('--vignettes', type=Path, required=True, nargs='+',
                        help="Path(s) to vignettes JSON file(s)")
    parser.add_argument('--mode', choices=['preextracted', 'interactive', 'llm'],
                        default='preextracted', help="Answer mode")
    parser.add_argument('--subjective-model', choices=['none', 'claude', 'openai'],
                        default='none', dest='subjective_model',
                        help="LLM for subjective criteria (default: none)")
    parser.add_argument('--disorder', type=str, default=None,
                        help="Filter by disorder ID (e.g., mdd)")
    parser.add_argument('--difficulty', type=str, default=None,
                        help="Filter by difficulty (CLEAR, MODERATE, AMBIGUOUS, COMORBID)")
    parser.add_argument('--verbose', '-v', action='store_true',
                        help="Print detailed output")
    args = parser.parse_args()

    # Validate all paths exist
    for path in args.vignettes:
        if not path.exists():
            print(f"Vignettes file not found: {path}")
            return

    evaluate_on_vignettes(
        vignettes_paths=args.vignettes,
        mode=args.mode,
        disorder=args.disorder,
        difficulty=args.difficulty,
        verbose=args.verbose,
        subjective_model=args.subjective_model
    )


if __name__ == '__main__':
    main()
