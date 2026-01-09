"""
Compare Hybrid Prolog+LLM vs Pure LLM diagnostic approaches.

This script runs both evaluation methods on the same vignettes and saves
comparison results for display in the notebook.

Usage:
    python -m src.evaluation.compare_llm --vignettes data/vignettes/*.json
    python -m src.evaluation.compare_llm --vignettes data/vignettes/*.json --count 20
"""

import argparse
import json
import logging
from dataclasses import dataclass, asdict
from datetime import datetime
from pathlib import Path
from typing import Optional

from openai import OpenAI
from tqdm import tqdm

from src.diagnosis.driver import DiagnosticDriver
from src.evaluation.answer_modes import create_preextracted_answer_fn
from src.extraction.config import Config

# Logging setup
LOG_DIR = Path(__file__).parent.parent.parent / 'logs' / 'comparison'
LOG_DIR.mkdir(parents=True, exist_ok=True)
TIMESTAMP = datetime.now().strftime('%Y%m%d_%H%M%S')
LOG_FILE = LOG_DIR / f"{TIMESTAMP}.log"

# Results output
RESULTS_DIR = Path(__file__).parent.parent.parent / 'data' / 'results' / 'comparison'
RESULTS_DIR.mkdir(parents=True, exist_ok=True)

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter('%(asctime)s | %(levelname)s | %(message)s'))
logger.addHandler(file_handler)


@dataclass
class ComparisonResult:
    """Result from comparing hybrid vs pure LLM on a single vignette."""
    vignette_id: str
    ground_truth: list[str]
    difficulty: str
    meets_criteria: bool
    # Hybrid results
    hybrid_status: str
    hybrid_correct: bool
    hybrid_confidence: float
    hybrid_questions: int
    # Pure LLM results
    pure_llm_predicted: list[str]
    pure_llm_correct: bool
    pure_llm_reasoning: str


def evaluate_pure_llm_single(vignette: dict, client: OpenAI) -> tuple[list[str], bool, str]:
    """Evaluate a single vignette using pure LLM approach.

    Returns:
        Tuple of (predicted_disorders, correct, reasoning)
    """
    prompt = f"""# Task
Evaluate this patient against DSM-5-TR criteria and determine which disorder(s) they meet criteria for.

## Clinical Note
{vignette['clinical_text']}

## Disorders
Choose from: **MDD**, **GAD**, **ADHD**, **PTSD**, **ASD**

## Instructions
- Evaluate against DSM-5-TR diagnostic criteria
- A patient may meet criteria for multiple disorders or none
- Respond with JSON only

## Response Format
```json
{{"diagnoses": ["DISORDER1", "DISORDER2"], "reasoning": "brief explanation"}}
```"""

    try:
        response = client.chat.completions.create(
            model="gpt-5-mini",
            messages=[
                {"role": "system", "content": "You are a psychiatrist. Respond with valid JSON only."},
                {"role": "user", "content": prompt}
            ],
            max_completion_tokens=5000
        )

        content = response.choices[0].message.content or "{}"

        # Parse JSON response
        if content.startswith('```'):
            content = content.split('```')[1]
            if content.startswith('json'):
                content = content[4:]

        data = json.loads(content.strip())
        predicted = [d.lower() for d in data.get('diagnoses', [])]
        reasoning = data.get('reasoning', '')

    except Exception as e:
        logger.error(f"Pure LLM error | vignette={vignette['id']} | error={e}")
        predicted = []
        reasoning = f"Error: {e}"

    # Check correctness
    target = vignette['ground_truth'][0]
    expected_met = vignette['meets_criteria']

    if expected_met:
        correct = target in predicted
    else:
        correct = target not in predicted

    return predicted, correct, reasoning


def run_comparison(
    vignettes_path: Path,
    count: Optional[int] = None
) -> list[ComparisonResult]:
    """Run comparison between hybrid and pure LLM approaches."""
    logger.info(f"Starting comparison | vignettes={vignettes_path} | count={count}")
    print(f"Log file: {LOG_FILE}")

    with open(vignettes_path) as f:
        vignettes = json.load(f)

    if count:
        vignettes = vignettes[:count]

    logger.info(f"Loaded {len(vignettes)} vignettes")

    config = Config.from_env()
    openai_client = OpenAI(api_key=config.openai_api_key)

    driver = DiagnosticDriver()
    if not driver.load():
        raise RuntimeError("Failed to load Prolog engine")

    results = []
    for i, vignette in enumerate(tqdm(vignettes, desc="Evaluating")):
        # Hybrid evaluation (Prolog reasoning with preextracted answers)
        answer_fn = create_preextracted_answer_fn(vignette['answers'])
        patient_id = f"compare_{i}"

        hybrid_results = driver.run_differential_diagnosis(
            patient_id=patient_id, answer_fn=answer_fn, verbose=False
        )

        target = vignette['ground_truth'][0]
        expected_status = 'met' if vignette['meets_criteria'] else 'not_met'

        if target in hybrid_results:
            h = hybrid_results[target]
            hybrid_status, hybrid_correct = h.status, h.status == expected_status
            hybrid_confidence, hybrid_questions = h.confidence, h.questions_asked
        else:
            hybrid_status, hybrid_correct = 'pruned', (expected_status == 'not_met')
            hybrid_confidence, hybrid_questions = 0.0, 0

        driver.clear_patient(patient_id)

        # Pure LLM evaluation
        pure_predicted, pure_correct, pure_reasoning = evaluate_pure_llm_single(vignette, openai_client)

        logger.info(f"Vignette {vignette['id']} | Hybrid: {hybrid_status} ({hybrid_correct}) | Pure LLM: {pure_predicted} ({pure_correct})")

        results.append(ComparisonResult(
            vignette_id=vignette['id'],
            ground_truth=vignette['ground_truth'],
            difficulty=vignette['difficulty'],
            meets_criteria=vignette['meets_criteria'],
            hybrid_status=hybrid_status,
            hybrid_correct=hybrid_correct,
            hybrid_confidence=hybrid_confidence,
            hybrid_questions=hybrid_questions,
            pure_llm_predicted=pure_predicted,
            pure_llm_correct=pure_correct,
            pure_llm_reasoning=pure_reasoning
        ))

    return results


def calculate_metrics(results: list[ComparisonResult]) -> dict:
    """Calculate comparison metrics from results."""
    total = len(results)

    hybrid_correct = sum(1 for r in results if r.hybrid_correct)
    pure_llm_correct = sum(1 for r in results if r.pure_llm_correct)

    # By difficulty
    by_difficulty = {}
    for r in results:
        if r.difficulty not in by_difficulty:
            by_difficulty[r.difficulty] = {
                'hybrid_correct': 0, 'pure_llm_correct': 0, 'total': 0
            }
        by_difficulty[r.difficulty]['total'] += 1
        if r.hybrid_correct:
            by_difficulty[r.difficulty]['hybrid_correct'] += 1
        if r.pure_llm_correct:
            by_difficulty[r.difficulty]['pure_llm_correct'] += 1

    # Calculate accuracies
    for diff in by_difficulty:
        t = by_difficulty[diff]['total']
        by_difficulty[diff]['hybrid_accuracy'] = by_difficulty[diff]['hybrid_correct'] / t if t > 0 else 0
        by_difficulty[diff]['pure_llm_accuracy'] = by_difficulty[diff]['pure_llm_correct'] / t if t > 0 else 0

    return {
        'total': total,
        'hybrid': {
            'correct': hybrid_correct,
            'accuracy': hybrid_correct / total if total > 0 else 0,
            'avg_questions': sum(r.hybrid_questions for r in results) / total if total > 0 else 0,
            'avg_confidence': sum(r.hybrid_confidence for r in results) / total if total > 0 else 0
        },
        'pure_llm': {
            'correct': pure_llm_correct,
            'accuracy': pure_llm_correct / total if total > 0 else 0
        },
        'by_difficulty': by_difficulty
    }


def save_results(results: list[ComparisonResult], metrics: dict):
    """Save comparison results to JSON file."""
    output_file = RESULTS_DIR / f"{TIMESTAMP}_comparison.json"

    output = {
        'timestamp': TIMESTAMP,
        'metrics': metrics,
        'results': [asdict(r) for r in results]
    }

    with open(output_file, 'w') as f:
        json.dump(output, f, indent=2)

    # Also save as 'latest' for easy notebook access
    latest_file = RESULTS_DIR / 'latest_comparison.json'
    with open(latest_file, 'w') as f:
        json.dump(output, f, indent=2)

    logger.info(f"Results saved to {output_file}")
    print(f"\nResults saved to: {output_file}")
    print(f"Latest results: {latest_file}")

    return output_file


def print_summary(metrics: dict):
    """Print comparison summary to console."""
    from src.utils.formatting import format_header, format_table

    print()
    print(format_header("Comparison Summary"))
    print(f"Total vignettes: {metrics['total']}\n")

    diff = metrics['hybrid']['accuracy'] - metrics['pure_llm']['accuracy']
    rows = [
        ['Hybrid (Prolog+LLM)', f"{metrics['hybrid']['accuracy']:.1%}", f"{metrics['hybrid']['correct']}/{metrics['total']}"],
        ['Pure LLM', f"{metrics['pure_llm']['accuracy']:.1%}", f"{metrics['pure_llm']['correct']}/{metrics['total']}"],
        ['Difference', f"{diff:+.1%}", '']
    ]
    print(format_table(['Approach', 'Accuracy', 'Correct'], rows))

    print()
    print(format_header("By Difficulty"))
    diff_rows = []
    for diff_name in sorted(metrics['by_difficulty'].keys()):
        d = metrics['by_difficulty'][diff_name]
        diff_rows.append([diff_name, f"{d['hybrid_accuracy']:.1%}", f"{d['pure_llm_accuracy']:.1%}"])
    print(format_table(['Difficulty', 'Hybrid', 'Pure LLM'], diff_rows))

    print(f"\nHybrid avg questions: {metrics['hybrid']['avg_questions']:.1f}")
    print(f"Hybrid avg confidence: {metrics['hybrid']['avg_confidence']:.1%}")


def main():
    parser = argparse.ArgumentParser(
        description="Compare Hybrid Prolog+LLM vs Pure LLM diagnostic approaches"
    )
    parser.add_argument(
        '--vignettes', type=Path, required=True,
        help="Path to vignettes JSON file"
    )
    parser.add_argument(
        '--count', type=int, default=None,
        help="Limit number of vignettes to evaluate (default: all)"
    )
    args = parser.parse_args()

    if not args.vignettes.exists():
        print(f"Vignettes file not found: {args.vignettes}")
        return

    # Run comparison
    results = run_comparison(args.vignettes, args.count)

    # Calculate and display metrics
    metrics = calculate_metrics(results)
    print_summary(metrics)

    # Save results
    save_results(results, metrics)

    logger.info("Comparison complete")


if __name__ == '__main__':
    main()
