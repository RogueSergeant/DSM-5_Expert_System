# Batch vs Sequential Experiment - Comparison Report

## Summary Table

| Mode | Agreement | Speedup | LLM Calls | Questions |
|------|-----------|---------|-----------|------------|
| batch_10 | 70.7% | 1.11x | 96 | 923 |
| batch_5 | 69.5% | 1.48x | 188 | 919 |
| batch_all | 68.9% | 3.34x | 10 | 1110 |

============================================================

## BATCH_10 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 70.7%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 555/785

### Performance Metrics
- **Speedup Ratio**: 1.11x
- **Time Sequential**: 1630.8s (27.2 min)
- **Time Batch**: 1467.8s (24.5 min)
- **LLM Call Reduction**: 8.80x (845 → 96 calls)

### Question Efficiency
- **Sequential**: 845 questions
- **Batch**: 923 questions


### Top Disagreement Patterns
1. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (8 disagreements)
2. Exaggerated startle response... (8 disagreements)
3. Often leaves seat in situations when remaining seated is expected (e.g., leaves ... (8 disagreements)
4. Directly experiencing the traumatic event(s)... (7 disagreements)
5. Learning that the traumatic event(s) occurred to a close family member or close ... (7 disagreements)

============================================================

## BATCH_5 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 69.5%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 537/773

### Performance Metrics
- **Speedup Ratio**: 1.48x
- **Time Sequential**: 1630.8s (27.2 min)
- **Time Batch**: 1103.5s (18.4 min)
- **LLM Call Reduction**: 4.49x (845 → 188 calls)

### Question Efficiency
- **Sequential**: 845 questions
- **Batch**: 919 questions


### Top Disagreement Patterns
1. Often leaves seat in situations when remaining seated is expected (e.g., leaves ... (9 disagreements)
2. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (8 disagreements)
3. Exaggerated startle response... (8 disagreements)
4. Witnessing, in person, the event(s) as it occurred to others... (7 disagreements)
5. Often loses things necessary for tasks or activities (e.g., school materials, pe... (7 disagreements)

============================================================

## BATCH_ALL vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 68.9%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 566/821

### Performance Metrics
- **Speedup Ratio**: 3.34x
- **Time Sequential**: 1630.8s (27.2 min)
- **Time Batch**: 487.8s (8.1 min)
- **LLM Call Reduction**: 84.50x (845 → 10 calls)

### Question Efficiency
- **Sequential**: 845 questions
- **Batch**: 1110 questions


### Top Disagreement Patterns
1. Do you adhere to this criterion: Symptoms cause clinically significant impairmen... (9 disagreements)
2. Do you adhere to this criterion: Severity level for restricted, repetitive behav... (8 disagreements)
3. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (8 disagreements)
4. Do you adhere to this criterion: Severity level for social communication impairm... (8 disagreements)
5. Is it true that: Several symptoms were present prior to age 12?... (7 disagreements)

============================================================

## Recommendations


**No Production-Ready Mode Found** ⚠️

- **Best Agreement**: `batch_10` at 70.7% (below 95% threshold)
- **Recommendation**: Continue using sequential mode for validation work
- **Alternative**: Use batch modes for exploratory work only

**Next Steps**:
- Implement Plan B: Try smaller batch sizes, hybrid approach, or structured output API
- Investigate disagreement patterns to improve prompt engineering

