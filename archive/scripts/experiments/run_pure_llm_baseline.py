"""
Pure LLM Baseline - Direct diagnosis without symbolic reasoning.

This script tests LLM zero-shot diagnosis capability by asking the model
to directly predict the disorder from clinical vignettes, bypassing
the hybrid Prolog reasoning system.

Usage:
    python run_pure_llm_baseline.py --provider openai --num-vignettes 20
    python run_pure_llm_baseline.py --provider anthropic --all
    python run_pure_llm_baseline.py --provider ollama --model qwen3:14b

Author: Alfie Roberts
Date: January 2026
"""

import argparse
import json
import sys
import time
from pathlib import Path
from datetime import datetime
from typing import List, Dict
from tqdm import tqdm

# Add project root to path for imports
project_root = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(project_root))

from src.evaluation.benchmark import load_vignettes
from src.extraction.providers import get_provider


DISORDERS = ["mdd", "gad", "adhd", "ptsd", "asd"]

ZERO_SHOT_PROMPT = """You are an expert clinical psychiatrist. Based on the following patient presentation, determine which mental health disorder best matches the clinical picture.

<patient_presentation>
{clinical_text}
</patient_presentation>

<available_diagnoses>
- MDD: Major Depressive Disorder
- GAD: Generalized Anxiety Disorder
- ADHD: Attention-Deficit/Hyperactivity Disorder
- PTSD: Posttraumatic Stress Disorder
- ASD: Autism Spectrum Disorder
</available_diagnoses>

<instructions>
1. Analyze the symptoms, duration, and functional impairment described
2. Consider DSM-5-TR diagnostic criteria for each disorder
3. Select the SINGLE most likely primary diagnosis
4. If multiple disorders seem present (comorbidity), choose the PRIMARY one
</instructions>

Respond with ONLY the disorder abbreviation (MDD, GAD, ADHD, PTSD, or ASD). No explanation."""


CHAIN_OF_THOUGHT_PROMPT = """You are an expert clinical psychiatrist performing a structured diagnostic assessment using DSM-5-TR criteria.

<patient_presentation>
{clinical_text}
</patient_presentation>

<task>
Systematically evaluate the patient against DSM-5-TR criteria for each of these disorders:
- MDD: Major Depressive Disorder
- GAD: Generalized Anxiety Disorder
- ADHD: Attention-Deficit/Hyperactivity Disorder
- PTSD: Posttraumatic Stress Disorder
- ASD: Autism Spectrum Disorder
</task>

<analysis_steps>
For each disorder, consider:
1. Are the core/essential symptoms present?
2. Is the symptom count threshold met?
3. Is the duration requirement satisfied?
4. Are exclusion criteria cleared (not better explained by substance/medical condition)?
5. Is there clinically significant distress or impairment?
</analysis_steps>

After your analysis, state your final diagnosis on a new line in this exact format:
DIAGNOSIS: [abbreviation]

Where [abbreviation] is one of: MDD, GAD, ADHD, PTSD, ASD"""


def run_baseline(
    provider_name: str,
    vignettes: List[Dict],
    prompt_type: str = "zero_shot",
    model: str = None
) -> List[Dict]:
    """
    Run pure LLM baseline on vignettes.

    Args:
        provider_name: LLM provider to use
        vignettes: List of vignette dicts
        prompt_type: "zero_shot" or "chain_of_thought"
        model: Optional model override for Ollama

    Returns:
        List of result dicts
    """
    provider = get_provider(provider_name, model=model)
    prompt_template = ZERO_SHOT_PROMPT if prompt_type == "zero_shot" else CHAIN_OF_THOUGHT_PROMPT

    results = []

    for vignette in tqdm(vignettes, desc=f"{provider_name} ({prompt_type})"):
        start_time = time.time()

        prompt = prompt_template.format(clinical_text=vignette["clinical_text"])

        try:
            result = provider.extract(
                dsm5_text=prompt,
                disorder_id="baseline",
                template_guide="",
                custom_prompt=prompt,
                custom_system_prompt="You are a clinical psychiatrist. Be concise."
            )

            if result.success:
                response = result.content.strip().upper()

                # Extract diagnosis from response
                if prompt_type == "chain_of_thought":
                    # Look for "DIAGNOSIS: XXX" pattern
                    if "DIAGNOSIS:" in response:
                        predicted = response.split("DIAGNOSIS:")[-1].strip().split()[0]
                    else:
                        # Fallback: look for disorder abbreviation at end
                        predicted = response.split()[-1] if response else "UNKNOWN"
                else:
                    # Zero-shot: response should be just the abbreviation
                    predicted = response.split()[0] if response else "UNKNOWN"

                # Normalize prediction
                predicted = predicted.strip(".,;:!?")
                if predicted not in ["MDD", "GAD", "ADHD", "PTSD", "ASD"]:
                    # Try to find a valid diagnosis in the response
                    for disorder in ["MDD", "GAD", "ADHD", "PTSD", "ASD"]:
                        if disorder in response:
                            predicted = disorder
                            break
                    else:
                        predicted = "UNKNOWN"

                duration = time.time() - start_time

                # Handle comorbid ground truth
                ground_truth = vignette["ground_truth"]
                if isinstance(ground_truth, list):
                    # For comorbid cases, check if prediction matches any
                    correct = predicted.lower() in [gt.lower() for gt in ground_truth]
                    ground_truth_str = "+".join(sorted(ground_truth))
                else:
                    correct = predicted.lower() == ground_truth.lower()
                    ground_truth_str = ground_truth

                results.append({
                    "vignette_id": vignette["id"],
                    "ground_truth": ground_truth_str,
                    "predicted": predicted,
                    "correct": correct,
                    "difficulty": vignette.get("difficulty", "UNKNOWN"),
                    "duration_seconds": round(duration, 2),
                    "raw_response": result.content[:500],  # Truncate for storage
                    "input_tokens": result.input_tokens,
                    "output_tokens": result.output_tokens
                })
            else:
                results.append({
                    "vignette_id": vignette["id"],
                    "ground_truth": vignette["ground_truth"],
                    "predicted": "ERROR",
                    "correct": False,
                    "difficulty": vignette.get("difficulty", "UNKNOWN"),
                    "duration_seconds": time.time() - start_time,
                    "error": result.error
                })

        except Exception as e:
            results.append({
                "vignette_id": vignette["id"],
                "ground_truth": vignette["ground_truth"],
                "predicted": "ERROR",
                "correct": False,
                "difficulty": vignette.get("difficulty", "UNKNOWN"),
                "duration_seconds": time.time() - start_time,
                "error": str(e)
            })

    return results


def calculate_metrics(results: List[Dict]) -> Dict:
    """Calculate accuracy and other metrics from results."""
    total = len(results)
    correct = sum(1 for r in results if r.get("correct", False))
    errors = sum(1 for r in results if r.get("predicted") == "ERROR")

    # By difficulty
    by_difficulty = {}
    for diff in ["CLEAR", "MODERATE", "AMBIGUOUS"]:
        diff_results = [r for r in results if r.get("difficulty") == diff]
        if diff_results:
            diff_correct = sum(1 for r in diff_results if r.get("correct", False))
            by_difficulty[diff] = {
                "total": len(diff_results),
                "correct": diff_correct,
                "accuracy": round(diff_correct / len(diff_results) * 100, 1)
            }

    # By disorder (ground truth)
    by_disorder = {}
    for disorder in DISORDERS:
        disorder_results = [r for r in results
                          if disorder in r.get("ground_truth", "").lower()]
        if disorder_results:
            disorder_correct = sum(1 for r in disorder_results if r.get("correct", False))
            by_disorder[disorder.upper()] = {
                "total": len(disorder_results),
                "correct": disorder_correct,
                "accuracy": round(disorder_correct / len(disorder_results) * 100, 1)
            }

    # Timing
    durations = [r.get("duration_seconds", 0) for r in results if "duration_seconds" in r]
    avg_duration = sum(durations) / len(durations) if durations else 0
    total_duration = sum(durations)

    # Tokens
    input_tokens = sum(r.get("input_tokens", 0) for r in results)
    output_tokens = sum(r.get("output_tokens", 0) for r in results)

    return {
        "total": total,
        "correct": correct,
        "errors": errors,
        "accuracy": round(correct / total * 100, 1) if total > 0 else 0,
        "by_difficulty": by_difficulty,
        "by_disorder": by_disorder,
        "avg_duration_seconds": round(avg_duration, 2),
        "total_duration_seconds": round(total_duration, 2),
        "total_input_tokens": input_tokens,
        "total_output_tokens": output_tokens
    }


def main():
    parser = argparse.ArgumentParser(description="Run pure LLM baseline evaluation")
    parser.add_argument("--provider", type=str, default="openai",
                       choices=["openai", "anthropic", "ollama"],
                       help="LLM provider to use")
    parser.add_argument("--model", type=str, default=None,
                       help="Model override (mainly for Ollama)")
    parser.add_argument("--num-vignettes", type=int, default=None,
                       help="Number of vignettes to test (default: all)")
    parser.add_argument("--prompt-type", type=str, default="zero_shot",
                       choices=["zero_shot", "chain_of_thought"],
                       help="Prompt strategy to use")
    parser.add_argument("--output-dir", type=str, default="outputs/baselines",
                       help="Output directory for results")
    parser.add_argument("--all", action="store_true",
                       help="Run all vignettes")

    args = parser.parse_args()

    # Load vignettes
    vignettes = load_vignettes()

    if args.num_vignettes and not args.all:
        vignettes = vignettes[:args.num_vignettes]

    print(f"\nPure LLM Baseline Evaluation")
    print(f"=" * 50)
    print(f"Provider: {args.provider}")
    print(f"Model: {args.model or 'default'}")
    print(f"Prompt Type: {args.prompt_type}")
    print(f"Vignettes: {len(vignettes)}")
    print(f"=" * 50)

    # Run baseline
    results = run_baseline(
        provider_name=args.provider,
        vignettes=vignettes,
        prompt_type=args.prompt_type,
        model=args.model
    )

    # Calculate metrics
    metrics = calculate_metrics(results)

    # Print summary
    print(f"\nResults Summary")
    print(f"-" * 40)
    print(f"Accuracy: {metrics['accuracy']}% ({metrics['correct']}/{metrics['total']})")
    print(f"Errors: {metrics['errors']}")
    print(f"Avg Time: {metrics['avg_duration_seconds']}s per vignette")
    print(f"Total Time: {metrics['total_duration_seconds']}s ({metrics['total_duration_seconds']/60:.1f} min)")

    print(f"\nBy Difficulty:")
    for diff, diff_metrics in metrics["by_difficulty"].items():
        print(f"  {diff}: {diff_metrics['accuracy']}% ({diff_metrics['correct']}/{diff_metrics['total']})")

    print(f"\nBy Disorder:")
    for disorder, disorder_metrics in metrics["by_disorder"].items():
        print(f"  {disorder}: {disorder_metrics['accuracy']}% ({disorder_metrics['correct']}/{disorder_metrics['total']})")

    # Save results
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    model_suffix = f"_{args.model.replace(':', '-')}" if args.model else ""
    output_file = output_dir / f"pure_llm_{args.provider}{model_suffix}_{args.prompt_type}_{timestamp}.json"

    output_data = {
        "metadata": {
            "provider": args.provider,
            "model": args.model,
            "prompt_type": args.prompt_type,
            "num_vignettes": len(vignettes),
            "timestamp": timestamp
        },
        "metrics": metrics,
        "results": results
    }

    with open(output_file, "w") as f:
        json.dump(output_data, f, indent=2)

    print(f"\nResults saved to: {output_file}")


if __name__ == "__main__":
    main()
