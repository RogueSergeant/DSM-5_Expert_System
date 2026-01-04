# Batch vs Sequential Question-Answering Experiment

> **Status**: Results pending - experiment not yet run
> **Date**: January 2026
> **Author**: Alfie Roberts

## 1. Motivation

### Problem

The current benchmark implementation for clinical vignette assessment has a significant performance bottleneck:

- **Sequential processing**: Each question requires a separate LLM API call
- **~20 questions per vignette**: Average diagnostic assessment
- **3-15 seconds per LLM call**: Depending on provider and model
- **111 total vignettes**: Full benchmark dataset

**Total time**: ~20 questions × 111 vignettes × 5 sec/call = **2.5+ hours**

This makes iterative development and large-scale validation prohibitively slow.

### Hypothesis

Batch question-answering (multiple questions per LLM call) can:
1. Reduce total benchmark time by 3-6x
2. Maintain ≥95% agreement with sequential baseline
3. Preserve question efficiency through adaptive A* search (for small/medium batches)

---

## 2. Methodology

### Experimental Design

**Controlled experiment** comparing 4 modes:

| Mode | Description | A* Search? | Expected Speedup |
|------|-------------|------------|------------------|
| Sequential (baseline) | 1 question per call | ✅ Yes | 1x |
| Small batches (2-3) | 2-3 questions per call | ✅ Yes | ~2x |
| Medium batches (5-10) | 5-10 questions per call | ✅ Yes | ~4x |
| All-at-once | All questions in 1 call | ❌ No | ~6x |

**Test Set**:
- 10 vignettes (subset for quick validation)
- 2 providers (Ollama + OpenAI)
- Same vignettes across all conditions

**Key Variables**:
- **Independent**: Batch size
- **Dependent**: Answer agreement %, total time, parse success rate
- **Controlled**: Vignettes, providers, prompts (except batch formatting)

### Critical Distinction: Adaptive vs Non-Adaptive

**Adaptive modes** (sequential, small, medium):
- Use A* search to select next best N questions
- Update diagnostic state after each batch
- Can prune impossible diagnoses
- Similar question count to sequential

**Non-adaptive mode** (all-at-once):
- Generate all possible questions upfront
- Single batch call, no adaptation
- More questions asked, but only 1 API call

---

## 3. Implementation Details

### Batch Prompt Engineering

**Input**: List of N questions

**Prompt Format**:
```
You are an expert clinical psychiatrist...

PATIENT VIGNETTE:
"[clinical text]"

TASK: Answer ALL N questions below.

QUESTIONS:
1. Does patient have depressed mood most of the day?
2. Does patient have markedly diminished interest?
...
N. Is it true that symptoms are NOT due to substance use?

OUTPUT FORMAT (JSON ONLY):
{
  "1": "YES",
  "2": "NO",
  ...
  "N": "UNKNOWN"
}
```

**Provider-Specific Optimizations**:
- **Ollama**: Explicit formatting instructions, no markdown
- **OpenAI**: Concise system prompt, rely on model reliability
- **Anthropic**: (if tested) Similar to OpenAI

### Parsing Strategy

1. **Primary**: JSON parsing with markdown cleanup
2. **Fallback**: Line-by-line regex parsing (`N: ANSWER`)
3. **Error handling**: Fallback to sequential for failed batches

### Files Implemented

- `src/evaluation/batch_analyser.py` - BatchClinicalAnalyser class
- `src/evaluation/batch_experiment.py` - Experiment orchestrator
- `src/evaluation/experiment_comparator.py` - Metrics calculator
- `run_batch_experiment.py` - Standalone CLI runner

---

## 4. Results

> **Note**: This section will be updated after running the experiment.

### 4.1 Ollama Results

**To be filled after experiment**

| Mode | Agreement | Speedup | LLM Calls | Questions |
|------|-----------|---------|-----------|-----------|
| Sequential | 100% (baseline) | 1.0x | ? | ? |
| Small (2-3) | ?% | ?x | ? | ? |
| Medium (5-10) | ?% | ?x | ? | ? |
| All-at-once | ?% | ?x | ? | ? |

### 4.2 OpenAI Results

**To be filled after experiment**

| Mode | Agreement | Speedup | LLM Calls | Questions |
|------|-----------|---------|-----------|-----------|
| Sequential | 100% (baseline) | 1.0x | ? | ? |
| Small (2-3) | ?% | ?x | ? | ? |
| Medium (5-10) | ?% | ?x | ? | ? |
| All-at-once | ?% | ?x | ? | ? |

### 4.3 Comparison

**To be filled after experiment**

---

## 5. Key Findings

> **Note**: This section will be updated after analyzing results.

**Findings to analyse**:
1. Which batch size (if any) meets ≥95% agreement threshold?
2. What is the optimal speed/accuracy trade-off?
3. Do different providers handle batch prompts differently?
4. What types of questions show most disagreement in batch mode?
5. Does batching affect question efficiency (total questions asked)?

---

## 6. Recommendations

> **Note**: This section will be updated based on results.

**Decision criteria**:
- If agreement ≥95% and speedup ≥3x → **Production-ready**
- If agreement 90-95% → **Conditional use** (exploratory work only)
- If agreement <90% → **Not viable** (implement Plan B)

---

## 7. Limitations & Future Work

### Limitations

1. **Sample size**: 10 vignettes (subset of full 111)
2. **Providers tested**: Ollama + OpenAI only (Anthropic not included)
3. **Batch sizes**: Limited to 2, 5, and all (could test 3, 7, 10, etc.)
4. **No few-shot examples**: Prompt uses zero-shot approach

### Future Work

If initial results are promising:
1. **Scale to full dataset**: Test on all 111 vignettes
2. **Test additional providers**: Anthropic Claude
3. **Fine-tune batch sizes**: Test 3, 7, 10 question batches
4. **Prompt engineering**: Add few-shot examples, test alternative formats
5. **Structured output API**: Test OpenAI's response_format with JSON schema
6. **Hybrid approaches**: Batch symptoms, sequential for verification questions

If results are not viable:
1. **Plan B - Smaller batches**: Test batch_size=1-2 instead of 5
2. **Plan B - Hybrid**: Batch core symptoms, sequential for exclusions
3. **Plan B - Self-review**: LLM reviews its own uncertain answers
4. **Plan B - Provider-specific optimization**: Tailored prompts per provider

---

## Appendix: Running the Experiment

### Prerequisites

- Python 3.10+
- Virtual environment activated
- Ollama running locally (for Ollama tests)
- OpenAI API key in .env (for OpenAI tests)

### Commands

```bash
# Run experiment on 10 vignettes
python run_batch_experiment.py \
  --num-vignettes 10 \
  --providers ollama openai

# Results saved to:
# outputs/batch_experiments/exp_TIMESTAMP/
```

### Output Files

- `metadata.json` - Experiment configuration
- `{provider}_sequential.json` - Baseline results per provider
- `{provider}_batch_2.json` - Small batch results
- `{provider}_batch_5.json` - Medium batch results
- `{provider}_batch_all.json` - All-at-once results
- `comparison_report.json` - Aggregated metrics
- `comparison_report_{provider}.md` - Human-readable summary

---

## References

- Experiment plan: [BATCH_EXPERIMENT_PLAN.md](./BATCH_EXPERIMENT_PLAN.md)
- Implementation: [src/evaluation/](../src/evaluation/)
- Original benchmark: [benchmark.py](../src/evaluation/benchmark.py)
