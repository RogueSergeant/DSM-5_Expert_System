#!/usr/bin/env python
"""
Standalone Batch Experiment Runner

Runs controlled experiments comparing batch vs sequential question answering
for clinical vignette assessment.

Usage:
    python run_batch_experiment.py --num-vignettes 10 --providers ollama openai

Outputs:
    - outputs/batch_experiments/exp_TIMESTAMP/
      - All JSON results per mode
      - comparison_report.json
      - comparison_report.md

Author: Alfie Roberts
Date: January 2026
"""

import argparse
import json
import sys
import datetime
from pathlib import Path

# Add project root to path for imports
project_root = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(project_root))

from src.evaluation.batch_experiment import BatchExperiment
from src.evaluation.experiment_comparator import ExperimentComparator


def prediction_matches(predicted_disorders: list, ground_truth) -> bool:
    """
    Check if prediction matches ground truth, handling comorbid cases.

    Args:
        predicted_disorders: List of predicted disorder IDs
        ground_truth: Single disorder ID (str) or list of disorder IDs (comorbid)

    Returns:
        True if prediction matches ground truth
    """
    if not predicted_disorders:
        return False

    if isinstance(ground_truth, list):
        # Comorbid case: must predict ALL ground truth disorders
        return all(gt in predicted_disorders for gt in ground_truth)
    else:
        # Single disorder case
        return ground_truth in predicted_disorders


def print_vignette_results(results: list, mode_name: str):
    """Print predicted disorder and confidence for each vignette result."""
    print(f"\n  {'─'*50}")
    print(f"  Predictions for {mode_name}:")
    print(f"  {'─'*50}")

    for result in results:
        vignette_id = result.get("vignette_id", "unknown")
        ground_truth = result.get("ground_truth", "unknown")
        # Use predicted_disorders (plural) for comorbid support
        predicted_disorders = result.get("predicted_disorders", [])
        confidence = result.get("confidence", 0.0)
        status = result.get("diagnosis_status", "unknown")

        # Determine if prediction matches ground truth (handles comorbid cases)
        match = "✓" if prediction_matches(predicted_disorders, ground_truth) else "✗"

        # Format predicted disorders as comma-separated string
        pred_str = ','.join(predicted_disorders) if predicted_disorders else 'none'

        # Format confidence as percentage
        conf_pct = f"{confidence * 100:.1f}%"

        print(f"  {match} {vignette_id}: predicted={pred_str} ({conf_pct}), truth={ground_truth}, status={status}")

    # Summary stats (using proper comorbid matching)
    correct = sum(1 for r in results
                  if prediction_matches(r.get("predicted_disorders", []), r.get("ground_truth")))
    total = len(results)
    accuracy = (correct / total * 100) if total > 0 else 0
    avg_conf = sum(r.get("confidence", 0) for r in results) / total if total > 0 else 0

    print(f"  {'─'*50}")
    print(f"  Accuracy: {correct}/{total} ({accuracy:.1f}%), Avg Confidence: {avg_conf * 100:.1f}%")


def main():
    """Main entry point for batch experiment runner."""
    parser = argparse.ArgumentParser(
        description="Run batch vs sequential question-answering experiment"
    )
    parser.add_argument(
        "--num-vignettes",
        type=int,
        default=10,
        help="Number of vignettes to test (default: 10)"
    )
    parser.add_argument(
        "--providers",
        nargs="+",
        default=["ollama", "openai"],
        help="LLM providers to test (default: ollama openai)"
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default="outputs/batch_experiments",
        help="Output directory (default: outputs/batch_experiments)"
    )
    parser.add_argument(
        "--vignettes-file",
        type=str,
        default=None,
        help="Specific vignette file to use (default: load all)"
    )

    args = parser.parse_args()

    # Create output directory
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    output_dir = Path(args.output_dir) / f"exp_{timestamp}"
    output_dir.mkdir(parents=True, exist_ok=True)

    print("="*60)
    print("Batch vs Sequential Experiment")
    print("="*60)
    print(f"Providers: {', '.join(args.providers)}")
    print(f"Vignettes: {args.num_vignettes}")
    print(f"Timestamp: {timestamp}")
    print(f"Output: {output_dir}")
    print("="*60)

    # Run experiment
    experiment = BatchExperiment(
        providers=args.providers,
        num_vignettes=args.num_vignettes,
        vignettes_file=args.vignettes_file
    )

    results = experiment.run_full_experiment()

    # Save raw results
    print(f"\nSaving results to {output_dir}...")

    # Save metadata
    with open(output_dir / "metadata.json", "w") as f:
        json.dump(results["metadata"], f, indent=2)

    # Save per-provider, per-mode results
    for provider, provider_results in results["results"].items():
        for mode, mode_results in provider_results.items():
            filename = f"{provider}_{mode}.json"
            with open(output_dir / filename, "w") as f:
                json.dump(mode_results, f, indent=2)
            print(f"  - {filename}")

    # Generate comparisons
    print(f"\nGenerating comparison reports...")
    comparator = ExperimentComparator()

    all_provider_comparisons = {}

    for provider, provider_results in results["results"].items():
        print(f"\n{'='*60}")
        print(f"SUMMARY: {provider.upper()}")
        print(f"{'='*60}")

        # Print predictions for each mode
        for mode_name, mode_results in provider_results.items():
            print_vignette_results(mode_results, f"{provider}/{mode_name}")

        # Get sequential baseline
        sequential = provider_results["sequential"]

        # Compare each batch mode
        batch_modes = {
            k: v for k, v in provider_results.items()
            if k != "sequential"
        }

        comparisons = comparator.compare_all_modes(sequential, batch_modes)
        all_provider_comparisons[provider] = comparisons

        # Print comparison summary for this provider
        print(f"\n{'='*60}")
        print(f"COMPARISON: {provider.upper()}")
        print(f"{'='*60}")
        for mode_name, comparison in sorted(comparisons.items()):
            agreement = comparison["agreement"]["overall_percent"]
            speedup = comparison["performance"]["speedup_ratio"]
            print(f"\n{mode_name.upper()} vs Sequential:")
            print(f"  Agreement: {agreement:.1f}%")
            print(f"  Speedup: {speedup:.2f}x")

            if agreement >= 95.0:
                print(f"  Status: ✅ Production-ready")
            elif agreement >= 90.0:
                print(f"  Status: ⚠️ Conditional use")
            else:
                print(f"  Status: ❌ Below threshold")

    # Save comparison JSON
    comparison_output = {
        "metadata": results["metadata"],
        "comparisons": all_provider_comparisons
    }

    with open(output_dir / "comparison_report.json", "w") as f:
        json.dump(comparison_output, f, indent=2)
    print(f"\nSaved: comparison_report.json")

    # Generate markdown report for each provider
    for provider, comparisons in all_provider_comparisons.items():
        report = comparator.generate_comparison_report(comparisons)

        report_file = output_dir / f"comparison_report_{provider}.md"
        with open(report_file, "w") as f:
            f.write(report)
        print(f"Saved: comparison_report_{provider}.md")

    print(f"\n{'='*60}")
    print("EXPERIMENT COMPLETE")
    print(f"{'='*60}")
    print(f"\nResults saved to: {output_dir}")
    print(f"\nNext steps:")
    print(f"1. Review comparison reports in {output_dir}")
    print(f"2. Update docs/BATCH_EXPERIMENT.md with findings")
    print(f"3. If viable, integrate batch mode into benchmark.py")


if __name__ == "__main__":
    main()
