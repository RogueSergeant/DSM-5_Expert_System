# Batch vs Sequential Experiment - Comparison Report

## Summary Table

| Mode | Agreement | Speedup | LLM Calls | Questions |
|------|-----------|---------|-----------|------------|
| batch_10 | 68.9% | 1.93x | 102 | 983 |
| batch_5 | 65.7% | 1.57x | 194 | 944 |
| batch_all | 68.0% | 3.35x | 10 | 1110 |

============================================================

## BATCH_10 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 68.9%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 566/821

### Performance Metrics
- **Speedup Ratio**: 1.93x
- **Time Sequential**: 1751.5s (29.2 min)
- **Time Batch**: 909.3s (15.2 min)
- **LLM Call Reduction**: 8.40x (857 → 102 calls)

### Question Efficiency
- **Sequential**: 857 questions
- **Batch**: 983 questions


### Top Disagreement Patterns
1. Often has difficulty waiting his or her turn (e.g., while waiting in line).... (10 disagreements)
2. Do you adhere to this criterion: Severity level for restricted, repetitive behav... (9 disagreements)
3. Often interrupts or intrudes on others (e.g., butts into conversations, games, o... (8 disagreements)
4. Often unable to play or engage in leisure activities quietly.... (8 disagreements)
5. Exaggerated startle response... (8 disagreements)

============================================================

## BATCH_5 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 65.7%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 540/822

### Performance Metrics
- **Speedup Ratio**: 1.57x
- **Time Sequential**: 1751.5s (29.2 min)
- **Time Batch**: 1112.3s (18.5 min)
- **LLM Call Reduction**: 4.42x (857 → 194 calls)

### Question Efficiency
- **Sequential**: 857 questions
- **Batch**: 944 questions


### Top Disagreement Patterns
1. Often has difficulty waiting his or her turn (e.g., while waiting in line).... (10 disagreements)
2. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (9 disagreements)
3. Often unable to play or engage in leisure activities quietly.... (9 disagreements)
4. Often interrupts or intrudes on others (e.g., butts into conversations, games, o... (8 disagreements)
5. Exaggerated startle response... (8 disagreements)

============================================================

## BATCH_ALL vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 68.0%
- **Exact Match Vignettes**: 0/10
- **Matching Answers**: 566/833

### Performance Metrics
- **Speedup Ratio**: 3.35x
- **Time Sequential**: 1751.5s (29.2 min)
- **Time Batch**: 523.4s (8.7 min)
- **LLM Call Reduction**: 85.70x (857 → 10 calls)

### Question Efficiency
- **Sequential**: 857 questions
- **Batch**: 1110 questions


### Top Disagreement Patterns
1. Do you adhere to this criterion: Severity level for restricted, repetitive behav... (10 disagreements)
2. Do you adhere to this criterion: Severity level for social communication impairm... (10 disagreements)
3. Often unable to play or engage in leisure activities quietly.... (9 disagreements)
4. Often has difficulty waiting his or her turn (e.g., while waiting in line).... (8 disagreements)
5. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (7 disagreements)

============================================================

## Recommendations


**No Production-Ready Mode Found** ⚠️

- **Best Agreement**: `batch_10` at 68.9% (below 95% threshold)
- **Recommendation**: Continue using sequential mode for validation work
- **Alternative**: Use batch modes for exploratory work only

**Next Steps**:
- Implement Plan B: Try smaller batch sizes, hybrid approach, or structured output API
- Investigate disagreement patterns to improve prompt engineering

