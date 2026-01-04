# Batch vs Sequential Experiment - Comparison Report

## Summary Table

| Mode | Agreement | Speedup | LLM Calls | Questions |
|------|-----------|---------|-----------|------------|
| batch_10 | 65.1% | 1.36x | 129 | 1239 |
| batch_5 | 62.7% | 1.49x | 249 | 1235 |
| batch_all | 59.7% | 3.57x | 10 | 1330 |

============================================================

## BATCH_10 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 65.1%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 667/1024

### Performance Metrics
- **Speedup Ratio**: 1.36x
- **Time Sequential**: 2094.5s (34.9 min)
- **Time Batch**: 1544.2s (25.7 min)
- **LLM Call Reduction**: 8.68x (1120 → 129 calls)

### Question Efficiency
- **Sequential**: 1120 questions
- **Batch**: 1239 questions


### Top Disagreement Patterns
1. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (10 disagreements)
2. Exaggerated startle response (child ≤6)... (9 disagreements)
3. Persistent reduction in expression of positive emotions (child ≤6)... (9 disagreements)
4. Witnessing, in person, the event(s) as it occurred to others, especially primary... (8 disagreements)
5. Often loses things necessary for tasks or activities (e.g., school materials, pe... (8 disagreements)

============================================================

## BATCH_5 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 62.7%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 632/1008

### Performance Metrics
- **Speedup Ratio**: 1.49x
- **Time Sequential**: 2094.5s (34.9 min)
- **Time Batch**: 1401.6s (23.4 min)
- **LLM Call Reduction**: 4.50x (1120 → 249 calls)

### Question Efficiency
- **Sequential**: 1120 questions
- **Batch**: 1235 questions


### Top Disagreement Patterns
1. Exaggerated startle response (child ≤6)... (10 disagreements)
2. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (10 disagreements)
3. Intense or prolonged psychological distress at exposure to internal or external ... (9 disagreements)
4. Often loses things necessary for tasks or activities (e.g., school materials, pe... (8 disagreements)
5. Persistent reduction in expression of positive emotions (child ≤6)... (8 disagreements)

============================================================

## BATCH_ALL vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 59.7%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 630/1056

### Performance Metrics
- **Speedup Ratio**: 3.57x
- **Time Sequential**: 2094.5s (34.9 min)
- **Time Batch**: 586.0s (9.8 min)
- **LLM Call Reduction**: 112.00x (1120 → 10 calls)

### Question Efficiency
- **Sequential**: 1120 questions
- **Batch**: 1330 questions


### Top Disagreement Patterns
1. Witnessing, in person, the event(s) as it occurred to others, especially primary... (9 disagreements)
2. Substantially increased frequency of negative emotional states (e.g., fear, guil... (9 disagreements)
3. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (9 disagreements)
4. Hypervigilance (child ≤6)... (9 disagreements)
5. Markedly diminished interest or participation in significant activities, includi... (9 disagreements)

============================================================

## Recommendations


**No Production-Ready Mode Found** ⚠️

- **Best Agreement**: `batch_10` at 65.1% (below 95% threshold)
- **Recommendation**: Continue using sequential mode for validation work
- **Alternative**: Use batch modes for exploratory work only

**Next Steps**:
- Implement Plan B: Try smaller batch sizes, hybrid approach, or structured output API
- Investigate disagreement patterns to improve prompt engineering

