# Evaluation System

Evaluates the diagnostic driver against generated vignettes to measure accuracy and efficiency.

## Overview

The evaluation system runs the Prolog-based diagnostic driver against clinical vignettes and compares predicted diagnoses to ground truth. It uses **differential diagnosis mode** - evaluating all disorders simultaneously rather than one at a time.

## Differential Diagnosis Mode

The driver performs realistic differential diagnosis:

1. **Start**: All 5 disorders (MDD, GAD, ADHD, PTSD, ASD) are active candidates
2. **Question loop**: Ask questions across ALL active candidates (deduplicated)
3. **Pruning**: Prolog's `disorder_pruned/2` automatically eliminates candidates as answers rule them out
4. **Termination**: Loop ends when all candidates are resolved (met/not_met) or pruned

### Question Ordering

Questions are sorted by:
1. **Priority** (type-based): symptoms → exclusions → duration → onset → subjective → settings
2. **Item ID** (alphabetical within priority)

This means questions are asked in order: adhd_* → asd_* → gad_* → mdd_* → ptsd_*

### Pruning Rules (from Prolog schema.pl)

A disorder is pruned when:
- All symptoms in a required category are explicitly absent
- A category requires all symptoms but any is absent
- An exclusion criterion is confirmed (e.g., substance-induced)
- Patient's age is outside the disorder's valid range (e.g., ADHD onset must be <12)

### Performance

**Achieved Results (50 vignettes, preextracted mode):**
- 100% accuracy (59/59 evaluations)
- 9.5 seconds total runtime
- ~0.19 seconds per vignette

This performance is enabled by the `next_question/2` optimisation described below.

## Answer Modes

| Mode | Description | Use Case |
|------|-------------|----------|
| `preextracted` | Looks up answers from vignette's pre-computed dict | Fast automated baseline testing |
| `interactive` | Clinician reads vignette and answers via prompts | Real clinical use, validation |
| `llm` | GPT-5-mini infers answers from clinical text only | Realistic system evaluation |

### Mode Details

**Pre-extracted (default)**
- Uses the `answers` dict generated alongside each vignette
- Ground truth answers - tests Prolog reasoning in isolation
- Very fast (no LLM calls)

**Interactive**
- Displays the clinical vignette text once
- Prompts for each symptom/exclusion/etc.
- Clinician interprets the narrative and provides answers
- For validating the system with human judgment

**LLM-led**
- GPT-5-mini reads ONLY the `clinical_text` narrative
- No access to pre-extracted answers - must infer from text
- Tests the full pipeline: LLM interpretation → Prolog reasoning
- More realistic evaluation of end-to-end system

## Tier B: Subjective Criteria LLM (--subjective-model)

The `--subjective-model` flag enables optional LLM processing for subjective criteria only, while keeping objective criteria handled by the base mode.

### What Are Subjective Criteria?

Subjective criteria require clinical judgment rather than objective counting:

| Assessment Type | Example Criterion | Disorder |
|-----------------|-------------------|----------|
| clinical_significance | "Symptoms cause clinically significant distress or impairment" | MDD, GAD, PTSD, ASD |
| excessiveness | "Anxiety and worry are excessive/disproportionate" | GAD |
| functional_impairment | "Clear evidence symptoms interfere with functioning" | ADHD |
| severity | "Severity level for social communication impairments" | ASD |
| quality | "Exposure involves death, serious injury, or sexual violence" | PTSD |

### How It Works

When `--subjective-model` is set to `claude` or `openai`:

1. **Objective criteria** (symptoms, exclusions, duration, onset, settings) → handled by base mode (preextracted or interactive)
2. **Subjective criteria** → routed to LLM for clinical judgment

```
Question loop:
    item = get_next_question()

    if item.type == 'subjective':
        → LLM assesses criterion with confidence score
        → (if interactive) Clinician can accept/override
    else:
        → Base answer mode handles it
```

### Provider Options

| Provider | Model | Prompt Format | Best For |
|----------|-------|---------------|----------|
| `claude` | Claude Sonnet 4.5 | XML | Highest quality clinical judgment |
| `openai` | GPT-5-mini | Markdown | Faster, lower cost |
| `none` | (disabled) | - | Default behaviour (no LLM for subjective) |

### Interactive Override

When combined with `--mode interactive`, the clinician sees:

```
────────────────────────────────────────────────────────────
SUBJECTIVE CRITERION: Symptoms cause clinically significant
distress or impairment in social, occupational, or other
important areas of functioning.
LLM Assessment: MET (confidence: 85%)
Evidence: "significant distress at work and relationship strain"
────────────────────────────────────────────────────────────
[a]ccept LLM / [o]verride / [u]nclear: _
```

- **Accept**: Use LLM's assessment (with its confidence score)
- **Override**: Clinician provides their own assessment (confidence 1.0)
- **Unclear**: Mark as unclear (confidence 0.5)

### Confidence Propagation

LLM confidence scores flow through to the final diagnosis:

1. LLM returns: `{"status": "met", "confidence": 0.85, "evidence": "..."}`
2. Stored in Prolog: `subjective_assessment(patient_001, mdd_subj_01, met, 0.85)`
3. Affects `calculate_enhanced_confidence/4` in final diagnosis
4. Lower LLM confidence → lower overall diagnosis confidence

### Usage Examples

```bash
# Pre-extracted + Claude for subjective (recommended)
python -m src.evaluation.evaluate \
    --vignettes data/vignettes/*.json \
    --mode preextracted \
    --subjective-model claude

# Interactive with Claude suggestions and override
python -m src.evaluation.evaluate \
    --vignettes data/vignettes/*.json \
    --mode interactive \
    --subjective-model claude

# Pre-extracted + OpenAI for subjective (faster)
python -m src.evaluation.evaluate \
    --vignettes data/vignettes/*.json \
    --mode preextracted \
    --subjective-model openai
```

## Usage

```bash
# Pre-extracted mode (fast baseline)
python -m src.evaluation.evaluate --vignettes data/vignettes/vignettes_*.json

# LLM mode (realistic evaluation)
python -m src.evaluation.evaluate --vignettes data/vignettes/vignettes_*.json --mode llm

# Interactive mode (clinician validation)
python -m src.evaluation.evaluate --vignettes data/vignettes/vignettes_*.json --mode interactive

# Filter by disorder or difficulty
python -m src.evaluation.evaluate --vignettes data/vignettes/*.json --disorder mdd
python -m src.evaluation.evaluate --vignettes data/vignettes/*.json --difficulty CLEAR

# Verbose output
python -m src.evaluation.evaluate --vignettes data/vignettes/*.json -v
```

## Output

Terminal output includes:
- Overall accuracy (correct/total)
- Average questions asked per vignette
- Breakdown by difficulty (CLEAR, MODERATE, AMBIGUOUS, COMORBID)
- Breakdown by disorder (MDD, GAD, ADHD, PTSD, ASD)
- Status distribution (met, not_met, incomplete)

Logs are written to `logs/evaluation/{timestamp}.log`.

## Metrics

| Metric | Target | Description |
|--------|--------|-------------|
| Accuracy (CLEAR) | >90% | Correct diagnosis on clear cases |
| Accuracy (overall) | >80% | Correct diagnosis across all difficulties |
| Avg questions | <15 | Efficiency of pruning |

## Architecture

```
src/evaluation/
├── __init__.py           # Module exports
├── evaluate.py           # CLI, orchestration, metrics
├── answer_modes.py       # Three answer mode factories
└── generate_vignettes.py # Vignette generation
```

### Key Functions

**evaluate.py**
- `evaluate_on_vignettes(path, mode, ...)` - Main entry point
- `evaluate_vignette(driver, vignette, mode)` - Single vignette evaluation
- `load_vignettes(path, disorder, difficulty)` - Load and filter vignettes
- `print_metrics(results)` - Display and log results

**answer_modes.py**
- `create_preextracted_answer_fn(answers)` - Dict lookup mode
- `create_interactive_answer_fn(clinical_text)` - Terminal prompt mode
- `create_llm_answer_fn(clinical_text)` - GPT-5-mini inference mode
- `create_subjective_llm_answer_fn(clinical_text, provider)` - LLM for subjective criteria only
- `create_hybrid_answer_fn(base_fn, llm_fn, interactive_override)` - Routes subjective to LLM

All return the same interface: `(DiagnosticItem) -> (status, evidence, confidence, value)`

## Design Decisions

### Why Three Modes?

1. **Pre-extracted** isolates Prolog reasoning from LLM interpretation
2. **LLM** tests the realistic pipeline where symptoms must be inferred
3. **Interactive** allows human validation and clinician override

### Why JSON for LLM Responses?

LLM returns structured JSON rather than free text:
```json
{"status": "present", "confidence": 0.9, "evidence": "quote", "value": null}
```

Benefits:
- Reliable parsing (no regex fragility)
- Explicit confidence scores
- Evidence traceability

### LLM Only Sees Clinical Text

In LLM mode, the answer callback receives ONLY the narrative `clinical_text` - not the pre-extracted answers. This ensures we're testing the system's ability to interpret clinical notes, not just route pre-computed answers.

## Performance Optimisation: `next_question/2`

### The Problem (Before)

The original implementation had a critical performance bottleneck. Each iteration of the diagnosis loop performed ~42 Prolog queries:

| Query | Count per iteration |
|-------|---------------------|
| `get_active_candidates()` | 2 (redundant calls) |
| `get_missing_items()` per disorder | 5 queries |
| `get_question_text()` per item | 20-50 queries |
| `full_diagnosis()` per disorder | 10 queries |
| `disorder()` info | 5 queries |

**Total: ~42 queries per question × 137 questions = ~5,800 queries per vignette**

The biggest bottleneck was `get_question_text()` - called separately for EACH missing item to retrieve its description. With 4-10 items per disorder across 5 candidates, this alone created 20-50 queries per iteration.

**Result**: Evaluation was frozen/infinite due to the sheer number of round-trips to Prolog.

### The Solution (After)

Instead of Python orchestrating multiple queries, I moved all the logic to a single Prolog predicate:

```prolog
%% next_question(+PatientID, -Item)
%% Returns the single next question to ask, with description included.
next_question(PatientID, Item) :-
    findall(
        q{priority: Priority, id: ID, disorder: DID, type: Type, category: Cat, description: Desc},
        (
            disorder(DID, _, _),
            \+ disorder_pruned(PatientID, DID),
            missing_item_with_priority(PatientID, DID, Type, ID, Cat, Desc, Priority)
        ),
        AllItems
    ),
    AllItems \= [],
    predsort(compare_questions, AllItems, Sorted),
    dedupe_by_id(Sorted, Deduped),
    Deduped = [First|_],
    Item = First.
```

**Key insight**: Prolog already has all the data. Instead of Python making 42 queries to assemble the next question, Prolog does it all internally and returns ONE item.

### What Changed

#### Added to `schema.pl`

| Predicate | Purpose |
|-----------|---------|
| `missing_item_with_priority/7` | Returns missing item with description and priority embedded |
| `compare_questions/3` | Sorts by priority, then alphabetically by ID |
| `dedupe_by_id/2` | Removes duplicate item IDs (same symptom may appear in multiple disorders) |
| `next_question/2` | Main entry point - returns single next question |

#### Modified in `driver.py`

| Method | Change |
|--------|--------|
| `get_next_question()` | **ADDED** - Single Prolog call returns next question |
| `run_differential_diagnosis()` | **SIMPLIFIED** - Now just calls `get_next_question()` in a loop |
| `assert_answer()` | **FIXED** - Now handles unclear/None values for duration, onset, settings |
| `get_all_missing_items()` | **DELETED** - Replaced by `get_next_question()` |

#### Bug Fixes

The optimisation also exposed a critical bug: when answers were unclear (e.g., no onset age provided), `assert_answer()` wasn't asserting anything. This caused infinite loops - the same question was asked repeatedly because it was never marked as "answered".

**Fixes applied:**
- **Duration**: If value is None, assert 0 days (doesn't meet duration requirement)
- **Onset**: If value is None, assert age 999 (adult onset - fails early onset checks)
- **Settings**: If evidence is empty, assert `setting(none)` to mark as answered

### Performance Comparison

| Metric | Before | After |
|--------|--------|-------|
| Queries per question | ~42 | **2** |
| Total queries per vignette | ~5,800 | **~280** |
| 50 vignettes runtime | Frozen/infinite | **9.5 seconds** |
| Accuracy | N/A | **100%** |

### Results (50 Vignettes, Preextracted Mode)

```
EVALUATION RESULTS
Overall Accuracy: 100.0% (59/59)
Average Questions: 136.6

By Difficulty:
  AMBIGUOUS   : 100.0% (6/6), avg 115.8 questions
  CLEAR       : 100.0% (22/22), avg 139.0 questions
  COMORBID    : 100.0% (18/18), avg 139.0 questions
  MODERATE    : 100.0% (13/13), avg 139.0 questions

By Disorder:
  adhd        : 100.0% (12/12)
  asd         : 100.0% (9/9)
  gad         : 100.0% (12/12)
  mdd         : 100.0% (15/15)
  ptsd        : 100.0% (11/11)

Predicted Statuses:
  met         : 56
  not_met     : 2
  pruned      : 1

Runtime: 9.5 seconds (vs frozen before)
```

### Architecture After Optimisation

```
Python (driver.py)                    Prolog (schema.pl)
─────────────────                     ──────────────────
run_differential_diagnosis()
    │
    └─► get_next_question()  ────►  next_question(PatientID, Item)
            │                              │
            │                              ├─► findall missing items from ALL disorders
            │                              ├─► Include descriptions directly
            │                              ├─► Sort by priority, then ID
            │                              ├─► Deduplicate by ID
            │                              └─► Return FIRST item
            │
            ◄──────────────────────────────┘
            │
    └─► assert_answer()      ────►  assert patient fact
            │
    └─► Loop until no more questions
```

### Why This Works

1. **Data locality**: Prolog has all disorder definitions, symptoms, and descriptions in memory. Querying them internally is nearly instant.

2. **Single round-trip**: Instead of Python ↔ Prolog chattering 42 times, I make 2 calls per question: one to get the question, one to assert the answer.

3. **Prolog does what Prolog does best**: Pattern matching, backtracking, and aggregation are native to Prolog. Doing this in Python required manual loops and multiple queries.

### Future Optimisation Opportunities

The current implementation still makes multiple queries at the end to build final results. This could be further optimised with a batch `all_diagnosis_results/2` predicate, but current performance (~0.19s per vignette) is acceptable.
