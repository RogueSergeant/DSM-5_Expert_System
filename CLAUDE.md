# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Always Remember

It is 2026
Write in British English only
Avoid using =s to break up print statements

## Project Overview

Hybrid Category 3 AI system combining Prolog-based symbolic reasoning (Tier A: objective criteria) with LLM-assisted judgment (Tier B: subjective criteria) for mental health diagnostic assessment using DSM-5-TR (2022).

**Target Disorders**: MDD, GAD, ADHD, PTSD, ASD

**DSM Version**: DSM-5-TR (2022), not DSM-5 (2013). See `docs/DSM_VERSION_NOTES.md` for differences.

## Architecture

**Three-Tier Hybrid Design:**
- **Tier A (Prolog)**: Objective criteria - symptom counts, duration, onset, exclusions → Transparent reasoning chains
- **Tier B (LLM)**: Subjective criteria - clinical significance, excessive worry → Confidence scores (0.0-1.0)
- **Tier C (Prolog)**: Integration - combines A+B with confidence propagation → Final diagnosis

**Key Directories:**
```
src/prolog/
├── schema.pl              # Diagnostic inference engine (predicates + rules)
├── gold_standard/         # Hand-curated disorders (mdd.pl, gad.pl, adhd.pl, ptsd.pl, asd.pl)
│   ├── loader.pl         # Loads all gold standard files
│   └── README.md         # Complete template guide for creating disorder files
└── extracted/            # LLM-extracted disorders (production output)

src/extraction/           # LLM extraction pipeline (DSM text → Prolog)
├── run_extraction.py    # CLI entry point
├── providers/           # OpenAI, Anthropic, Ollama implementations
└── evaluate.py          # Syntax + schema validation

src/reasoning/            # Python↔Prolog interface
├── engine.py            # PrologEngine wrapper (pyswip)
├── utils.py             # KB exploration
└── viz.py               # Diagnostic flowchart generation

src/search/              # Diagnostic pathway optimisation
├── search.py            # A* search for optimal question sequences
└── manager.py           # SessionManager with pruning logic

src/evaluation/          # Benchmarking framework
└── benchmark.py         # Clinical vignette testing

scripts/                 # Standalone scripts
├── debug/              # Debugging utilities
└── experiments/        # Batch experiment runners

tests/                   # Test scripts
specs/                   # Coursework specification PDFs
data/dsm5_text/          # DSM-5-TR source text
data/vignettes/          # Clinical test cases
docs/                    # Comprehensive documentation (see README.md)
```

## Prerequisites

- Python 3.10+
- SWI-Prolog: `brew install swi-prolog` (macOS) - required for pyswip
- API keys: OpenAI/Anthropic (optional, only for LLM extraction)

## Setup

```bash
python -m venv .venv
source .venv/bin/activate  # Windows: .venv\Scripts\activate
pip install -r requirements.txt
cp .env.example .env  # Add API keys if using LLM extraction
```

## Common Commands

**LLM Extraction** (DSM text → Prolog):
```bash
# Recommended: Anthropic Claude (best quality)
python -m src.extraction.run_extraction --disorder ptsd --provider anthropic --thinking-budget 15000 --production

# Fastest: OpenAI GPT-5 (excellent quality, faster)
python -m src.extraction.run_extraction --disorder mdd --provider openai --reasoning-effort high --production

# Local/offline: Ollama (requires validation)
python -m src.extraction.run_extraction --disorder gad --provider ollama --model gpt-oss:20b --think high --save

# Compare all providers (saves to outputs/extractions/)
python -m src.extraction.run_extraction --disorder ptsd --all --save

# List available Ollama models
python -m src.extraction.run_extraction --list-models
```

**Prolog Interactive Testing**:
```bash
cd src/prolog
swipl -g "[schema], ['gold_standard/loader']"

# In Prolog REPL:
?- disorder(X, Name, Category).          # List all disorders
?- symptom(mdd, S, Cat, Desc).           # List MDD symptoms
?- validate_disorder(mdd, Issues).       # Validate disorder definition
?- full_diagnosis(pt001, mdd, Result).   # Run full diagnosis
```

**Debugging & Evaluation**:
```bash
python scripts/debug/diagnose_prolog_failure.py  # Diagnose criteria failures
python scripts/debug/debug_assertions.py         # Check duration/subjective assertions
python -m src.evaluation.benchmark               # Run clinical vignette benchmarks
```

**Experiments**:
```bash
python scripts/experiments/run_batch_experiment.py --num-vignettes 10  # Batch vs sequential
python scripts/experiments/run_pure_llm_baseline.py --provider openai  # Pure LLM baseline
```

**Testing**:
```bash
pytest                    # All tests (if tests/ exists)
pytest -v                 # Verbose output
pytest -k "pattern"       # Pattern matching
```

## Prolog Schema Overview

**Knowledge Base Predicates** (define disorder criteria):
- `disorder/3` - disorder(ID, FullName, Category)
- `symptom/4` - symptom(DisorderID, SymptomID, Category, Description)
- `symptom_category/5` - symptom groupings with count requirements
  - RequirementType: `at_least`, `exactly`, `all`, `at_least_one_of`
- `duration_requirement/3` - temporal constraints (e.g., "2 weeks")
- `onset_requirement/3` - onset timing (before_age, after_event, any)
- `exclusion_criterion/4` - what must NOT be present (substance, medical, other_disorder)
- `subjective_criterion/4` - criteria requiring clinical judgment (clinical_significance, excessiveness, etc.)

**Diagnostic Inference Predicates** (reason over patient data):
- `full_diagnosis/3` - comprehensive diagnosis with status tracking (met/not_met/missing_data)
- `criterion_check/5` - individual criterion status with details
- `collect_missing_data/3` - identify data gaps
- `generate_follow_up_questions/3` - suggest next questions

**Disorder-Specific Predicates** (optional):
- `age_adjusted_count/4` - different thresholds by age (ADHD: 6 symptoms <17, 5 symptoms ≥17)
- `setting_requirement/2` - symptoms required across multiple settings (ADHD: 2+ settings)

See `src/prolog/gold_standard/README.md` for complete predicate reference and templates.

## LLM Provider Configuration

| Provider | Parameter | Options | Best For |
|----------|-----------|---------|----------|
| **Anthropic** | `--thinking-budget` | 0 (off), 1024-20000 | Gold standard (highest quality) |
| **OpenAI** | `--reasoning-effort` | none/minimal/low/medium/high/xhigh | Batch processing (fastest, excellent quality) |
| **Ollama** | `--think` | low/medium/high | Local/offline (requires validation) |

**Key Findings** (see `docs/PROVIDER_EVALUATION.md`):
- Anthropic: Most complete extractions, best for gold standards
- OpenAI: Fastest (67s vs 81s), excellent quality, lower cost
- Ollama: Free but may miss critical criteria (e.g., PTSD Criterion A)

## Working with Disorders

**Adding New Disorders**:
1. Add DSM-5-TR source text: `data/dsm5_text/{DISORDER}.txt`
2. **Option A - LLM Extraction**: `python -m src.extraction.run_extraction --disorder {id} --provider anthropic --production`
3. **Option B - Manual**: Create `src/prolog/gold_standard/{id}.pl` following `gold_standard/README.md` template
4. Register in `gold_standard/loader.pl` if gold standard

**Validation Pipeline**:
- Prolog syntax check (SWI-Prolog subprocess)
- Schema compliance via `validate_disorder/2`
- Symptom/exclusion counts
- Comparison with gold standard (if available)

**Output Locations**:
- `--save`: `outputs/extractions/{disorder}_{provider}_{timestamp}.pl` (with metadata)
- `--production`: `src/prolog/extracted/{disorder}.pl` (overwrites existing)

## Important Architecture Details

**Patient Data vs Knowledge Base**:
- Knowledge Base (static): `disorder/3`, `symptom/4`, etc. defined in gold_standard/*.pl
- Patient Facts (dynamic): `patient_symptom/4`, `patient_duration/3` asserted at runtime by PrologEngine

**Diagnostic Flow**:
1. Load schema + disorder files
2. Assert patient facts for current case
3. Query `full_diagnosis/3` → triggers inference rules
4. Rules check symptom counts, duration, onset, exclusions, subjective criteria
5. Return diagnosis with confidence score and explanation

**Search & Pruning** (src/search/):
- A* search finds optimal question sequences (reduces questions 40-60%)
- Pruning eliminates candidates via exclusions or missing core symptoms
- SessionManager tracks active candidates and answered questions
