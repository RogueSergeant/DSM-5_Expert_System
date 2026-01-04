# Legacy System Documentation

This document summarises the original over-engineered implementation and explains the rationale for simplification.

## Original Architecture (Pre-Simplification)

The system was designed as a **Category 3 Hybrid Approach** combining Prolog symbolic reasoning with LLM-assisted extraction and assessment. The architecture followed a three-tier model:

### Layer 1: Knowledge Base Construction (LLM → Prolog)
- DSM-5-TR text extracted via LLM into Prolog predicates
- Three provider implementations: OpenAI, Anthropic, Ollama
- Validation pipeline: syntax checking + schema compliance

### Layer 2: Three-Tier Diagnostic Reasoning
- **Tier A (Prolog)**: Objective criteria — symptom counts, duration, onset, exclusions
- **Tier B (LLM)**: Subjective criteria — clinical significance, excessive worry
- **Tier C (Prolog)**: Integration — combine results with confidence propagation

### Layer 3: Diagnostic Pathway Search (A*)
- Heuristic-guided question selection to minimise questions asked
- Candidate pruning based on exclusions and core symptom absence
- Claimed 40-60% reduction in questions vs exhaustive approach

## What Was Built

| Component | Lines | Purpose |
|-----------|-------|---------|
| `schema.pl` | 1,100 | Core Prolog inference engine |
| Gold standards | ~600 | 5 disorders hand-curated (MDD, GAD, ADHD, PTSD, ASD) |
| `SessionManager` | 387 | Python↔Prolog orchestration with 8-branch question dispatch |
| `DiagnosticSearch` | 193 | A* search (simplified to greedy heuristic) |
| 3 LLM providers | ~1,000 | OpenAI, Anthropic, Ollama extraction |
| Batch experiment framework | ~900 | Multi-mode comparison testing |
| Evaluation/benchmark | ~400 | Clinical vignette testing |
| Visualisation | ~200 | Diagnostic flowchart generation |
| **Total** | **~5,800 Python + ~1,700 Prolog** | |

## Why It Was Over-Engineered

### 1. Schema Expansion Beyond Spec

The specification asked for:
```prolog
symptom/3, duration/2, exclusion/2, specifier/3, subjective_criterion/2
% Target: 50-80 rules across 5 disorders
```

What was built:
```prolog
symptom/4, symptom_category/5, duration_requirement/3, onset_requirement/3,
exclusion_criterion/4, subjective_criterion/4, age_adjusted_count/4,
setting_requirement/2, criterion_check/5, full_diagnosis/3, ...
% Actual: 1,100 lines
```

The schema grew to handle edge cases (ADHD age-adjusted counts, PTSD preschool variant, multi-setting requirements) that added complexity without proportional benefit.

### 2. The "Tier B" Problem

The spec described Tier B as:
> "For criteria requiring interpretation ('clinically significant impairment', 'excessive worry'), the system either: (a) presents questions for clinician judgment, or (b) provides LLM-generated suggestions with confidence scores."

What was actually built:
- LLM extracts knowledge base (offline, one-time) ✓
- LLM answers questions from vignettes (for benchmarking) — **this is testing, not diagnosis**
- No real-time LLM subjective assessment during actual diagnosis

The `ClinicalAnalyser` class simulates a patient answering questions from a vignette. It doesn't provide clinical judgment on subjective criteria — it just parses text and returns YES/NO/UNKNOWN.

### 3. Search Complexity Without Payoff

The A* search was specified but the implementation:
- Reduced to a greedy heuristic (not true A*)
- Used fragile string-matching (`"_exc_" in question_id`)
- Had 8 different question type branches
- Information gain calculation was simplistic (count presence, not mutual information)

The `SessionManager.answer_question()` method has cascading if/elif branches:
```python
if "_duration_check" in question_id:
elif "_exc_" in question_id:
elif "_subj_" in question_id:
elif "_onset_age_check" in question_id:
elif "_onset_event_check" in question_id:
elif "_settings_check" in question_id:
elif "_age_threshold_check" in question_id:
elif "_age_range_check" in question_id:
else:  # Standard symptom
```

This complexity exists because Python is managing what Prolog should handle declaratively.

### 4. Evaluation Framework Scope Creep

The spec required:
- 30-50 synthetic vignettes
- Basic accuracy metrics

What was built:
- Batch experiment framework with 4 modes (sequential, batch-5, batch-10, all-at-once)
- Multi-provider comparison
- Experiment orchestration with tqdm progress bars
- JSON + Markdown report generation
- ~900 lines of evaluation code

### 5. Documentation Proliferation

14 markdown files totalling ~110KB documenting subsystems that themselves were unnecessary:
- `BATCH_EXPERIMENT.md`, `BATCH_EXPERIMENT_PLAN.md`, `BATCH_EXPERIMENT_FINDINGS.md`
- `PROVIDER_EVALUATION.md`, `PROVIDER_DECISION.md`
- `EXTRACTION_BENCHMARKS.md`, `FLOWCHART_GUIDE.md`

## What Actually Matters

The core of a Prolog diagnostic system is simple:

```prolog
% 1. Define disorder criteria (knowledge base)
disorder(mdd, 'Major Depressive Disorder', depressive_disorders).
symptom(mdd, mdd_a1, core, 'Depressed mood').
symptom_category(mdd, core, [mdd_a1, mdd_a2], 1, at_least_one_of).
duration_requirement(mdd, 2, weeks).

% 2. Assert patient facts
patient_symptom(pt1, mdd_a1, present, 'Reports feeling sad').

% 3. Query for diagnosis
?- diagnosis_candidate(pt1, mdd, Confidence).
```

Everything else is orchestration.

## The New Approach

### Principles
1. **Prolog does the reasoning** — don't replicate logic in Python
2. **Thin Python wrapper** — assert facts, run queries, display results
3. **One LLM use case** — knowledge extraction (Layer 1), not patient simulation
4. **Simple evaluation** — does it diagnose correctly? How many questions?

### What Stays
- `src/prolog/` — the actual diagnostic system
- `src/reasoning/engine.py` — thin pyswip wrapper
- One extraction provider — for KB construction
- `data/` — source text and test vignettes
- Core documentation

### What's Archived
- Batch experiment framework
- Multi-provider extraction comparison
- Visualisation system
- Debug scripts tied to complex implementation
- Most documentation

## DSM-5-TR Reference Notes

The system uses DSM-5-TR (2022), not DSM-5 (2013). Key differences:

| Disorder | DSM-5-TR Change | Impact |
|----------|-----------------|--------|
| ASD | Criterion A clarified to "all of" | Minor wording |
| ADHD | Text updates only | None |
| MDD | Mixed features specifier clarified | None for core |
| GAD | Text updates only | None |
| PTSD | None for adults | None |

## Archived Components

The following were moved to `archive/`:

```
archive/
├── src/
│   ├── evaluation/
│   │   ├── batch_experiment.py      # 900-line experiment framework
│   │   ├── batch_analyser.py        # Batch question answering
│   │   └── experiment_comparator.py # Comparison metrics
│   └── reasoning/
│       └── viz.py                   # Flowchart visualisation
├── scripts/
│   └── experiments/
│       ├── run_batch_experiment.py
│       └── run_pure_llm_baseline.py
├── outputs/
│   └── batch_experiments/           # All experiment outputs
├── docs/                            # Legacy documentation
└── tests/                           # Tests for complex system
```

## Lessons Learned

1. **Start simple, add complexity only when needed** — the spec asked for 50-80 rules, not 1,100 lines
2. **Don't build evaluation infrastructure before the core works** — batch experiments preceded working diagnosis
3. **Prolog's strength is declarative logic** — Python shouldn't manage question types with string matching
4. **LLMs are tools, not magic** — using an LLM to answer vignette questions doesn't constitute "Tier B subjective assessment"
5. **Documentation follows implementation** — 14 docs for a system that doesn't work correctly is wasted effort
