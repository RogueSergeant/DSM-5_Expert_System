# Batch vs Sequential Experiment - Comparison Report

## Summary Table

| Mode | Agreement | Speedup | LLM Calls | Questions |
|------|-----------|---------|-----------|------------|
| batch_10 | 57.8% | 0.59x | 128 | 1220 |
| batch_5 | 56.8% | 1.42x | 246 | 1209 |
| batch_all | 41.5% | 3.44x | 10 | 850 |

============================================================

## BATCH_10 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 57.8%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 592/1025

### Performance Metrics
- **Speedup Ratio**: 0.59x
- **Time Sequential**: 1765.4s (29.4 min)
- **Time Batch**: 2978.0s (49.6 min)
- **LLM Call Reduction**: 8.52x (1090 → 128 calls)

### Question Efficiency
- **Sequential**: 1090 questions
- **Batch**: 1220 questions


### Top Disagreement Patterns
1. Exaggerated startle response (child ≤6)... (10 disagreements)
2. Often has difficulty waiting his or her turn (e.g., while waiting in line).... (9 disagreements)
3. Often unable to play or engage in leisure activities quietly.... (9 disagreements)
4. Persistent reduction in expression of positive emotions (child ≤6)... (9 disagreements)
5. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (9 disagreements)

============================================================

## BATCH_5 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 56.8%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 579/1020

### Performance Metrics
- **Speedup Ratio**: 1.42x
- **Time Sequential**: 1765.4s (29.4 min)
- **Time Batch**: 1247.1s (20.8 min)
- **LLM Call Reduction**: 4.43x (1090 → 246 calls)

### Question Efficiency
- **Sequential**: 1090 questions
- **Batch**: 1209 questions


### Top Disagreement Patterns
1. Exaggerated startle response (child ≤6)... (10 disagreements)
2. Often has difficulty waiting his or her turn (e.g., while waiting in line).... (9 disagreements)
3. Deficits in nonverbal communicative behaviors used for social interaction, rangi... (9 disagreements)
4. Often unable to play or engage in leisure activities quietly.... (9 disagreements)
5. Problems with concentration (child ≤6)... (9 disagreements)

============================================================

## BATCH_ALL vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 41.5%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 301/725

### Performance Metrics
- **Speedup Ratio**: 3.44x
- **Time Sequential**: 1765.4s (29.4 min)
- **Time Batch**: 513.5s (8.6 min)
- **LLM Call Reduction**: 109.00x (1090 → 10 calls)

### Question Efficiency
- **Sequential**: 1090 questions
- **Batch**: 850 questions


### Top Disagreement Patterns
1. Often has difficulty waiting his or her turn (e.g., while waiting in line).... (10 disagreements)
2. Irritable behavior and angry outbursts (with little or no provocation) typically... (10 disagreements)
3. Often unable to play or engage in leisure activities quietly.... (9 disagreements)
4. Learning that the traumatic event(s) occurred to a close family member or close ... (9 disagreements)
5. Substantially increased frequency of negative emotional states (e.g., fear, guil... (9 disagreements)

============================================================

## Recommendations


**No Production-Ready Mode Found** ⚠️

- **Best Agreement**: `batch_10` at 57.8% (below 95% threshold)
- **Recommendation**: Continue using sequential mode for validation work
- **Alternative**: Use batch modes for exploratory work only

**Next Steps**:
- Implement Plan B: Try smaller batch sizes, hybrid approach, or structured output API
- Investigate disagreement patterns to improve prompt engineering

