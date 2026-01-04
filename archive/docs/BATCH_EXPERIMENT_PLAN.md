# Batch vs Sequential Question-Answering Experiment Plan

> **Status**: Implementation in progress
> **Date**: January 2026
> **Author**: Alfie Roberts

## Executive Summary

**Problem**: Benchmarking 111 vignettes takes 2.5+ hours due to sequential LLM calls (1 question per API call)

**Hypothesis**: Batch question-answering (multiple questions per call) can achieve ≥95% accuracy while reducing time by 3-6x

**Approach**: Controlled experiment comparing 4 modes (sequential baseline, small batches 2-3, medium batches 5-10, all-at-once) on 10 vignettes with Ollama + OpenAI providers

**Success Criteria**:
- Agreement ≥95% with sequential baseline
- Speedup ratio ≥3x
- Parse success rate ≥90%

**Scope**: Pure experimentation - NO notebook integration until results are validated

---

## 1. Original System Architecture

### Current Sequential Implementation

**File**: [benchmark.py:122-148](../src/evaluation/benchmark.py#L122-L148)

```python
while questions_asked < max_questions:
    # 1. A* search selects next best question
    next_q_id = search.get_next_best_question(manager.state.active_candidates)

    # 2. Get question text from Prolog KB
    q_text = manager.get_symptom_description(next_q_id)

    # 3. ONE LLM CALL per question
    ans_str = sim.answer(q_text)  # ClinicalAnalyser.answer()

    # 4. Update Prolog state
    manager.answer_question(next_q_id, ans_str)
    questions_asked += 1
```

**ClinicalAnalyser.answer()** ([benchmark.py:33-87](../src/evaluation/benchmark.py#L33-L87)):
- Takes 1 question as input
- Builds prompt: vignette + question + "Answer YES/NO/UNKNOWN"
- Makes single `provider.extract()` call
- Returns single answer string

**Performance Bottleneck**:
- ~20 questions per vignette
- 3-15 seconds per LLM call
- 111 vignettes × 20 questions × 5 sec = **2.5+ hours**

---

## 2. Proposed Batch Architecture

### New BatchClinicalAnalyser

**File**: `src/evaluation/batch_analyser.py`

**Key Method**:
```python
def answer_batch(
    questions: List[str],
    batch_size: Optional[int] = None
) -> Dict[str, str]:
    """
    Answer multiple questions in batches.

    Args:
        questions: List of criterion questions
        batch_size: None = all-at-once, int = chunk size

    Returns:
        {question_text: "YES"/"NO"/"UNKNOWN"}
    """
```

**Prompt Strategy**:
```python
TASK: Answer ALL {len(questions)} questions based on the vignette.

QUESTIONS:
1. Does patient have depressed mood most of the day?
2. Does patient have markedly diminished interest?
...
20. Is it true that symptoms are NOT due to substance use?

OUTPUT FORMAT (JSON ONLY):
{
  "1": "YES",
  "2": "NO",
  ...
  "20": "UNKNOWN"
}
```

---

## 3. What We're Testing

### Experimental Conditions

| Mode | Description | A* Search? | Questions Asked | Expected Time | Expected Accuracy |
|------|-------------|------------|----------------|---------------|-------------------|
| **Sequential** (baseline) | 1 question per call | ✅ Yes (after each answer) | ~20 (adaptive) | 100s | 100% (by definition) |
| **Batch Small** | 2-3 questions per call | ✅ Yes (after each batch) | ~22 (adaptive) | 50s | **?** (to be measured) |
| **Batch Medium** | 5-10 questions per call | ✅ Yes (after each batch) | ~25 (adaptive) | 35s | **?** (to be measured) |
| **Batch All-at-Once** | All questions in 1 call | ❌ No (cannot adapt) | ~30 (fixed) | 20s | **?** (to be measured) |

**Key Variables**:
- **Independent**: Batch size (1, 2-3, 5-10, all)
- **Dependent**: Answer agreement %, total time, parse success rate, question efficiency
- **Controlled**: Same vignettes, same providers, same prompts (except batch formatting)

**Rationale for Multiple Batch Sizes**:
- **Small batches (2-3)**: Maintains A* search adaptivity, minimal speedup, highest expected accuracy
- **Medium batches (5-10)**: Still adaptive but reduces iterations, moderate speedup/accuracy trade-off
- **All-at-once**: No adaptivity (must ask all questions upfront), maximum speedup but likely lower accuracy

**Critical Distinction**:
- **Adaptive modes** (sequential, small, medium): Use A* search to select best N questions → batch → update state → select next best N
- **Non-adaptive mode** (all-at-once): Generate all possible questions upfront → single batch → no adaptation possible

**Research Questions**:
1. Does batch processing with A* search (small/medium) maintain ≥95% agreement with sequential baseline?
2. What is the optimal batch size balancing speed, accuracy, and adaptivity?
3. How much accuracy is lost when disabling A* search (all-at-once mode)?
4. Do different providers (Ollama vs OpenAI) handle batch prompts differently?
5. Does batching affect question efficiency (total questions asked)?

---

## 4. Implementation Plan

### Phase 1: Core Batch Functionality

**4.1. BatchClinicalAnalyser** (`src/evaluation/batch_analyser.py`):
- Extends existing `ClinicalAnalyser`
- `answer_batch()` - main batch processing method
- JSON prompt building and parsing
- Fallback error handling

**4.2. BatchExperiment** (`src/evaluation/batch_experiment.py`):
- Experiment orchestrator
- Adaptive batching with A* search (small/medium)
- Non-adaptive all-at-once mode
- Per-vignette processing logic

**4.3. ExperimentComparator** (`src/evaluation/experiment_comparator.py`):
- Agreement metrics
- Performance metrics (speedup, time)
- Statistical tests

**4.4. Standalone Runner** (`run_batch_experiment.py`):
- CLI script for running experiments
- Saves results to `outputs/batch_experiments/`

### Phase 2: Documentation

**docs/BATCH_EXPERIMENT.md** - Results documentation (updated after experiment)

**docs/BATCH_EXPERIMENT_PLAN.md** - This plan

### Phase 3: Execution

```bash
python run_batch_experiment.py \
  --num-vignettes 10 \
  --providers ollama openai \
  --batch-sizes 2 5 all
```

**Expected Runtime**: 20-40 minutes

---

## 5. Success Criteria & Decision Tree

### If Agreement ≥95% and Speedup ≥3x:
✅ **Production-ready** - Document as recommended approach

### If Agreement 90-95%:
⚠️ **Conditional use** - Batch for exploratory work, sequential for validation

### If Agreement <90%:
❌ **Not viable** - Implement Plan B (hybrid approach, structured output API, few-shot examples)

---

## 6. Deliverables

**Code**:
- `src/evaluation/batch_analyser.py` - Batch processing logic
- `src/evaluation/batch_experiment.py` - Experiment orchestrator
- `src/evaluation/experiment_comparator.py` - Metrics calculator
- `run_batch_experiment.py` - Standalone CLI runner

**Documentation**:
- `docs/BATCH_EXPERIMENT.md` - Results and recommendations (updated post-experiment)
- `docs/BATCH_EXPERIMENT_PLAN.md` - This plan
- `outputs/batch_experiments/exp_TIMESTAMP/` - Raw data and reports

**Tests** (optional):
- `tests/evaluation/test_batch_analyser.py` - Unit tests

---

## 7. Expected Outcomes

### Optimistic (Agreement ≥95%, Speedup 4-5x):
- Batch mode becomes standard for large benchmarks
- 111 vignettes: 2.5 hours → 30-40 minutes
- Provider-specific batch size recommendations

### Realistic (Agreement 92-94%, Speedup 3-4x):
- Small batches for high-accuracy needs
- Medium batches for exploratory work
- Accuracy/speed trade-off table

### Pessimistic (Agreement <90%):
- Current format not viable
- Implement Plan B alternatives
- Document lessons learned

---

## Conclusion

This experiment tests batch question-answering as an optimisation for clinical vignette benchmarking. The modular architecture ensures safe iteration without breaking production code. Results will be thoroughly documented before any integration into the main workflow.
