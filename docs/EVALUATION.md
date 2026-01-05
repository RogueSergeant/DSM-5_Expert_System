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

### Performance Consideration

Differential diagnosis requires more Prolog queries per question than single-disorder mode:
- `get_active_candidates()` - 1 query
- `get_missing_items()` per candidate - 6 queries
- `get_diagnosis()` per candidate for resolution check - 6 queries

With 137 total questions across all disorders, this results in ~1900 queries per vignette. Optimisation is planned for future work.

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

All three return the same interface: `(DiagnosticItem) -> (status, evidence, confidence, value)`

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
