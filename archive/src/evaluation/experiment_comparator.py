"""
Experiment Comparator - Calculates metrics comparing batch vs sequential results.

This module implements metrics calculation and statistical comparison between
different question-answering modes (sequential baseline vs various batch sizes).

Author: Alfie Roberts
Date: January 2026
"""

from typing import List, Dict, Tuple
from collections import Counter


class ExperimentComparator:
    """
    Compares experimental results across different modes.

    Calculates agreement, performance, and reliability metrics to determine
    which batch size (if any) maintains acceptable accuracy while improving speed.
    """

    def compare_all_modes(
        self,
        sequential_results: List[Dict],
        batch_results_dict: Dict[str, List[Dict]]
    ) -> Dict:
        """
        Compare all batch modes against sequential baseline.

        Args:
            sequential_results: Results from sequential mode
            batch_results_dict: {mode_name: results_list} for all batch modes

        Returns:
            {
                mode_name: {
                    "agreement": {...},
                    "performance": {...},
                    "reliability": {...}
                },
                ...
            }
        """
        comparisons = {}

        for mode_name, batch_results in batch_results_dict.items():
            comparisons[mode_name] = self.compare_results(
                sequential_results,
                batch_results,
                mode_name
            )

        return comparisons

    def compare_results(
        self,
        sequential_results: List[Dict],
        batch_results: List[Dict],
        mode_name: str = "batch"
    ) -> Dict:
        """
        Compare batch results against sequential baseline.

        Args:
            sequential_results: Baseline sequential results
            batch_results: Batch mode results
            mode_name: Name of batch mode for reporting

        Returns:
            {
                "agreement": {...},
                "performance": {...},
                "reliability": {...}
            }
        """
        metrics = {
            "mode": mode_name,
            "agreement": self._calculate_agreement(
                sequential_results, batch_results
            ),
            "performance": self._calculate_performance(
                sequential_results, batch_results
            ),
            "reliability": self._calculate_reliability(batch_results)
        }

        return metrics

    def _calculate_agreement(
        self,
        seq_results: List[Dict],
        batch_results: List[Dict]
    ) -> Dict:
        """
        Calculate answer agreement between modes.

        Agreement metrics:
        1. Overall percentage: (matching answers) / (total questions)
        2. Exact match vignettes: How many vignettes had 100% agreement
        3. Per-question breakdown: Which questions diverged most

        Args:
            seq_results: Sequential results
            batch_results: Batch results

        Returns:
            Agreement metrics dict
        """
        total_questions = 0
        matching_answers = 0
        exact_match_count = 0

        question_disagreements = {}  # question_text -> count

        for seq, batch in zip(seq_results, batch_results):
            assert seq["vignette_id"] == batch["vignette_id"], \
                f"Vignette mismatch: {seq['vignette_id']} vs {batch['vignette_id']}"

            seq_answers = seq["answers"]
            batch_answers = batch["answers"]

            # Find common questions (both modes asked)
            common_questions = set(seq_answers.keys()) & set(batch_answers.keys())

            vignette_matches = 0
            vignette_total = len(common_questions)

            for q in common_questions:
                total_questions += 1
                if seq_answers[q] == batch_answers[q]:
                    matching_answers += 1
                    vignette_matches += 1
                else:
                    # Record disagreement
                    question_disagreements[q] = \
                        question_disagreements.get(q, 0) + 1

            # Check if this vignette had perfect agreement
            if vignette_total > 0 and vignette_matches == vignette_total:
                exact_match_count += 1

        overall_percent = (matching_answers / total_questions * 100) \
                          if total_questions > 0 else 0

        # Top disagreements
        top_disagreements = sorted(
            question_disagreements.items(),
            key=lambda x: x[1],
            reverse=True
        )[:10]  # Top 10

        return {
            "overall_percent": round(overall_percent, 2),
            "exact_match_vignettes": exact_match_count,
            "total_vignettes": len(seq_results),
            "matching_answers": matching_answers,
            "total_questions": total_questions,
            "disagreement_count": len(question_disagreements),
            "top_disagreements": [
                {"question": q[:80], "count": cnt}
                for q, cnt in top_disagreements
            ]
        }

    def _calculate_performance(
        self,
        seq_results: List[Dict],
        batch_results: List[Dict]
    ) -> Dict:
        """
        Calculate time and throughput metrics.

        Args:
            seq_results: Sequential results
            batch_results: Batch results

        Returns:
            Performance metrics dict
        """
        seq_total_time = sum(r["duration_seconds"] for r in seq_results)
        batch_total_time = sum(r["duration_seconds"] for r in batch_results)

        seq_total_questions = sum(r["num_questions"] for r in seq_results)
        batch_total_questions = sum(r["num_questions"] for r in batch_results)

        seq_total_calls = sum(r["num_llm_calls"] for r in seq_results)
        batch_total_calls = sum(r["num_llm_calls"] for r in batch_results)

        speedup = seq_total_time / batch_total_time if batch_total_time > 0 else 0

        return {
            "time_sequential_total": round(seq_total_time, 2),
            "time_batch_total": round(batch_total_time, 2),
            "speedup_ratio": round(speedup, 2),
            "avg_time_per_vignette_sequential": \
                round(seq_total_time / len(seq_results), 2),
            "avg_time_per_vignette_batch": \
                round(batch_total_time / len(batch_results), 2),
            "questions_sequential": seq_total_questions,
            "questions_batch": batch_total_questions,
            "llm_calls_sequential": seq_total_calls,
            "llm_calls_batch": batch_total_calls,
            "call_reduction_ratio": round(
                seq_total_calls / batch_total_calls, 2
            ) if batch_total_calls > 0 else 0
        }

    def _calculate_reliability(self, batch_results: List[Dict]) -> Dict:
        """
        Calculate reliability metrics (parse success, etc).

        Args:
            batch_results: Batch results

        Returns:
            Reliability metrics dict
        """
        total_vignettes = len(batch_results)

        # Count successful parses (no fallback to sequential)
        # This would require tracking in batch_results, for now estimate

        return {
            "total_vignettes": total_vignettes,
            "parse_success_rate": 100.0  # Placeholder - would need tracking
        }

    def generate_summary_table(self, comparison: Dict) -> str:
        """
        Generate human-readable summary table.

        Args:
            comparison: Comparison results dict

        Returns:
            Formatted markdown table string
        """
        agreement = comparison["agreement"]
        performance = comparison["performance"]

        table = f"""
## {comparison['mode'].upper()} vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: {agreement['overall_percent']:.1f}%
- **Exact Match Vignettes**: {agreement['exact_match_vignettes']}/{agreement['total_vignettes']}
- **Matching Answers**: {agreement['matching_answers']}/{agreement['total_questions']}

### Performance Metrics
- **Speedup Ratio**: {performance['speedup_ratio']:.2f}x
- **Time Sequential**: {performance['time_sequential_total']:.1f}s ({performance['time_sequential_total']/60:.1f} min)
- **Time Batch**: {performance['time_batch_total']:.1f}s ({performance['time_batch_total']/60:.1f} min)
- **LLM Call Reduction**: {performance['call_reduction_ratio']:.2f}x ({performance['llm_calls_sequential']} → {performance['llm_calls_batch']} calls)

### Question Efficiency
- **Sequential**: {performance['questions_sequential']} questions
- **Batch**: {performance['questions_batch']} questions

"""

        # Add top disagreements if any
        if agreement['top_disagreements']:
            table += "\n### Top Disagreement Patterns\n"
            for i, item in enumerate(agreement['top_disagreements'][:5], 1):
                table += f"{i}. {item['question']}... ({item['count']} disagreements)\n"

        return table

    def generate_comparison_report(
        self,
        all_comparisons: Dict[str, Dict]
    ) -> str:
        """
        Generate comprehensive comparison report across all modes.

        Args:
            all_comparisons: {mode_name: comparison_dict}

        Returns:
            Formatted markdown report
        """
        report = "# Batch vs Sequential Experiment - Comparison Report\n\n"

        # Summary table
        report += "## Summary Table\n\n"
        report += "| Mode | Agreement | Speedup | LLM Calls | Questions |\n"
        report += "|------|-----------|---------|-----------|------------|\n"

        for mode_name, comparison in sorted(all_comparisons.items()):
            agreement = comparison["agreement"]["overall_percent"]
            speedup = comparison["performance"]["speedup_ratio"]
            calls = comparison["performance"]["llm_calls_batch"]
            questions = comparison["performance"]["questions_batch"]

            report += f"| {mode_name} | {agreement:.1f}% | {speedup:.2f}x | {calls} | {questions} |\n"

        # Detailed sections for each mode
        for mode_name, comparison in sorted(all_comparisons.items()):
            report += "\n" + "="*60 + "\n"
            report += self.generate_summary_table(comparison)

        # Recommendations
        report += "\n" + "="*60 + "\n"
        report += self._generate_recommendations(all_comparisons)

        return report

    def _generate_recommendations(self, all_comparisons: Dict[str, Dict]) -> str:
        """
        Generate recommendations based on results.

        Args:
            all_comparisons: All comparison results

        Returns:
            Recommendations text
        """
        recommendations = "\n## Recommendations\n\n"

        # Find best mode by agreement ≥95% and highest speedup
        viable_modes = []
        for mode_name, comparison in all_comparisons.items():
            agreement = comparison["agreement"]["overall_percent"]
            speedup = comparison["performance"]["speedup_ratio"]

            if agreement >= 95.0:
                viable_modes.append((mode_name, agreement, speedup))

        if viable_modes:
            # Sort by speedup (highest first)
            viable_modes.sort(key=lambda x: x[2], reverse=True)
            best_mode, best_agreement, best_speedup = viable_modes[0]

            recommendations += f"""
**Production-Ready Mode Found** ✅

- **Recommended**: `{best_mode}`
- **Agreement**: {best_agreement:.1f}% (≥95% threshold met)
- **Speedup**: {best_speedup:.2f}x
- **Use Case**: Production benchmarking of large vignette sets

"""
        else:
            # Find mode with highest agreement even if <95%
            best_mode = max(
                all_comparisons.items(),
                key=lambda x: x[1]["agreement"]["overall_percent"]
            )
            mode_name, comparison = best_mode
            agreement = comparison["agreement"]["overall_percent"]

            recommendations += f"""
**No Production-Ready Mode Found** ⚠️

- **Best Agreement**: `{mode_name}` at {agreement:.1f}% (below 95% threshold)
- **Recommendation**: Continue using sequential mode for validation work
- **Alternative**: Use batch modes for exploratory work only

**Next Steps**:
- Implement Plan B: Try smaller batch sizes, hybrid approach, or structured output API
- Investigate disagreement patterns to improve prompt engineering

"""

        return recommendations
