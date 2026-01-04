# Batch vs Sequential Experiment - Comparison Report

## Summary Table

| Mode | Agreement | Speedup | LLM Calls | Questions |
|------|-----------|---------|-----------|------------|
| batch_2 | 74.7% | 2.63x | 50 | 100 |
| batch_5 | 70.6% | 3.98x | 20 | 100 |
| batch_all | 30.7% | 3.83x | 2 | 170 |

============================================================

## BATCH_2 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 74.7%
- **Exact Match Vignettes**: 0/2
- **Matching Answers**: 62/83

### Performance Metrics
- **Speedup Ratio**: 2.63x
- **Time Sequential**: 389.7s (6.5 min)
- **Time Batch**: 148.4s (2.5 min)
- **LLM Call Reduction**: 4.56x (228 → 50 calls)

### Question Efficiency
- **Sequential**: 228 questions
- **Batch**: 100 questions


### Top Disagreement Patterns
1. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (2 disagreements)
2. Learning that the traumatic event(s) occurred to a parent or caregiving figure (... (2 disagreements)
3. Often does not seem to listen when spoken to directly (e.g., mind seems elsewher... (2 disagreements)
4. Persistent, distorted cognitions about the cause or consequences of the traumati... (2 disagreements)
5. Avoidance of or efforts to avoid distressing memories, thoughts, or feelings abo... (1 disagreements)

============================================================

## BATCH_5 vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 70.6%
- **Exact Match Vignettes**: 0/2
- **Matching Answers**: 60/85

### Performance Metrics
- **Speedup Ratio**: 3.98x
- **Time Sequential**: 389.7s (6.5 min)
- **Time Batch**: 98.0s (1.6 min)
- **LLM Call Reduction**: 11.40x (228 → 20 calls)

### Question Efficiency
- **Sequential**: 228 questions
- **Batch**: 100 questions


### Top Disagreement Patterns
1. Witnessing, in person, the event(s) as it occurred to others, especially primary... (2 disagreements)
2. Do you adhere to this criterion: For learning about events (Criterion A3), the e... (2 disagreements)
3. Learning that the traumatic event(s) occurred to a parent or caregiving figure (... (2 disagreements)
4. Often does not seem to listen when spoken to directly (e.g., mind seems elsewher... (2 disagreements)
5. Directly experiencing the traumatic event(s) (child ≤6 years)... (2 disagreements)

============================================================

## BATCH_ALL vs Sequential Baseline

### Agreement Metrics
- **Overall Agreement**: 30.7%
- **Exact Match Vignettes**: 0/2
- **Matching Answers**: 46/150

### Performance Metrics
- **Speedup Ratio**: 3.83x
- **Time Sequential**: 389.7s (6.5 min)
- **Time Batch**: 101.8s (1.7 min)
- **LLM Call Reduction**: 114.00x (228 → 2 calls)

### Question Efficiency
- **Sequential**: 228 questions
- **Batch**: 170 questions


### Top Disagreement Patterns
1. Is often forgetful in daily activities (e.g., doing chores, running errands; for... (2 disagreements)
2. Exaggerated startle response (child ≤6)... (2 disagreements)
3. Witnessing, in person, the event(s) as it occurred to others... (2 disagreements)
4. Avoidance of or efforts to avoid external reminders (people, places, conversatio... (2 disagreements)
5. Avoidance of or efforts to avoid people, conversations, or interpersonal situati... (2 disagreements)

============================================================

## Recommendations


**No Production-Ready Mode Found** ⚠️

- **Best Agreement**: `batch_2` at 74.7% (below 95% threshold)
- **Recommendation**: Continue using sequential mode for validation work
- **Alternative**: Use batch modes for exploratory work only

**Next Steps**:
- Implement Plan B: Try smaller batch sizes, hybrid approach, or structured output API
- Investigate disagreement patterns to improve prompt engineering

