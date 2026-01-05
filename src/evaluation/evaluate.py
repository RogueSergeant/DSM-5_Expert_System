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
from dataclasses import dataclass
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

# Logging setup
LOG_DIR = Path(__file__).parent.parent.parent / 'logs' / 'evaluation'
LOG_DIR.mkdir(parents=True, exist_ok=True)
LOG_FILE = LOG_DIR / f"{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"

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

    # Run differential diagnosis across ALL disorders
    diagnosis_results = driver.run_differential_diagnosis(
        patient_id=f"patient_{vignette['id']}",
        answer_fn=answer_fn,
        verbose=verbose
    )

    # Evaluate against ground truth
    results = []
    expected_status = 'met' if vignette['meets_criteria'] else 'not_met'

    for disorder_id in vignette['ground_truth']:
        if disorder_id in diagnosis_results:
            result = diagnosis_results[disorder_id]
            correct = result.status == expected_status
        else:
            # Disorder was pruned - counts as 'not_met'
            correct = (expected_status == 'not_met')
            result = DiagnosisResult(disorder_id, disorder_id, 'pruned', 0.0, 0, 0)

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
            confidence=result.confidence
        ))

    return results


def load_vignettes(path: Path, disorder: Optional[str] = None,
                   difficulty: Optional[str] = None) -> list[dict]:
    """Load vignettes from JSON, optionally filtering."""
    logger.info(f"Loading vignettes from {path}")
    with open(path) as f:
        vignettes = json.load(f)

    initial_count = len(vignettes)
    if disorder:
        vignettes = [v for v in vignettes if disorder in v['ground_truth']]
    if difficulty:
        vignettes = [v for v in vignettes if v['difficulty'] == difficulty.upper()]

    logger.info(f"Loaded {len(vignettes)} vignettes (from {initial_count}, filter: disorder={disorder}, difficulty={difficulty})")
    return vignettes


def evaluate_on_vignettes(
    vignettes_path: Path,
    mode: str = 'preextracted',
    disorder: Optional[str] = None,
    difficulty: Optional[str] = None,
    verbose: bool = False,
    subjective_model: str = 'none'
) -> list[EvaluationResult]:
    """Run evaluation on vignettes and report metrics."""
    logger.info(f"Starting evaluation | mode={mode} | subjective_model={subjective_model} | path={vignettes_path}")
    print(f"Log file: {LOG_FILE}")

    vignettes = load_vignettes(vignettes_path, disorder, difficulty)
    if not vignettes:
        print("No vignettes found matching filters")
        logger.warning("No vignettes found matching filters")
        return []

    subj_info = f", subjective: {subjective_model}" if subjective_model != 'none' else ""
    print(f"Loaded {len(vignettes)} vignettes (mode: {mode}{subj_info})")

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
                status = "✓" if r.correct else "✗"
                print(f"  {status} {r.predicted_disorder}: {r.predicted_status} "
                      f"(expected {'met' if r.meets_criteria else 'not_met'}, "
                      f"{r.questions_asked} questions)")

    print_metrics(all_results)
    logger.info(f"Evaluation complete | total={len(all_results)}")

    return all_results


def print_metrics(results: list[EvaluationResult]):
    """Print and log evaluation metrics."""
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

    # Print to terminal
    print("\nEVALUATION RESULTS")
    print(f"Overall Accuracy: {accuracy:.1%} ({correct}/{total})")
    print(f"Average Questions: {avg_questions:.1f}")

    print("\nBy Difficulty:")
    for diff, stats in sorted(difficulties.items()):
        diff_acc = stats['correct'] / stats['total'] if stats['total'] > 0 else 0
        avg_q = stats['questions'] / stats['total'] if stats['total'] > 0 else 0
        print(f"  {diff:12s}: {diff_acc:5.1%} ({stats['correct']}/{stats['total']}), avg {avg_q:.1f} questions")

    print("\nBy Disorder:")
    for disorder, stats in sorted(disorders.items()):
        dis_acc = stats['correct'] / stats['total'] if stats['total'] > 0 else 0
        print(f"  {disorder:12s}: {dis_acc:5.1%} ({stats['correct']}/{stats['total']})")

    print("\nPredicted Statuses:")
    for status, count in sorted(statuses.items()):
        print(f"  {status:12s}: {count}")


def main():
    parser = argparse.ArgumentParser(description="Evaluate diagnostic driver on vignettes")
    parser.add_argument('--vignettes', type=Path, required=True,
                        help="Path to vignettes JSON file")
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

    if not args.vignettes.exists():
        print(f"Vignettes file not found: {args.vignettes}")
        return

    evaluate_on_vignettes(
        vignettes_path=args.vignettes,
        mode=args.mode,
        disorder=args.disorder,
        difficulty=args.difficulty,
        verbose=args.verbose,
        subjective_model=args.subjective_model
    )


if __name__ == '__main__':
    main()
