# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Always Remember

It is 2025

## Project Overview

Hybrid diagnostic decision support system combining Prolog-based symbolic reasoning with LLM-assisted knowledge extraction. The system uses DSM-5-TR (2022) diagnostic criteria encoded in Prolog to perform explainable diagnostic reasoning.

**DSM Version Note**: The reference standard is DSM-5-TR (2022), not DSM-5 (2013). See `data/DSM_VERSION_NOTES.md` for version differences and their impact on each disorder.

## Data Flow

```
data/dsm5_text/{DISORDER}.txt  →  LLM Extraction  →  src/prolog/extracted/{disorder}.pl
                                       ↓
                    gold_standard/README.md (template guide)
                    src/prolog/schema.pl (predicate reference)
                                       ↓
                              Syntax validation via SWI-Prolog
                              Schema validation via validate_disorder/2
```

## Architecture

```
src/prolog/
├── schema.pl              # Core diagnostic inference engine (knowledge base + rules)
├── gold_standard/         # Hand-curated disorder definitions (mdd.pl, gad.pl, adhd.pl, ptsd.pl)
│   ├── loader.pl         # Loads all gold standard files
│   └── README.md         # Template guide for .pl file structure
└── extracted/            # LLM-extracted disorder definitions (production output)

src/extraction/           # LLM extraction pipeline
├── base.py              # ExtractionProvider ABC, ExtractionResult dataclass
├── config.py            # Environment config, API key management
├── evaluate.py          # Prolog syntax validation via SWI-Prolog subprocess
├── run_extraction.py    # CLI entry point
└── providers/           # LLM provider implementations
    ├── openai_provider.py    # GPT-5 with reasoning_effort
    ├── anthropic_provider.py # Claude with extended thinking
    └── ollama_provider.py    # Local models with think parameter

data/dsm5_text/          # Source DSM-5-TR criteria text files ({DISORDER}.txt)
outputs/extractions/     # Timestamped extraction results with metadata
notebooks/main.ipynb     # Interactive experiments with pyswip
```

## Prerequisites

- Python 3.10+
- SWI-Prolog (`brew install swi-prolog` on macOS) - required for pyswip and syntax validation
- API keys for OpenAI/Anthropic (or local Ollama)

## Key Commands

```bash
# Environment setup
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
cp .env.example .env  # Add API keys

# Run LLM extraction
python -m src.extraction.run_extraction --disorder ptsd --provider ollama --model gpt-oss:20b --think high
python -m src.extraction.run_extraction --disorder ptsd --provider anthropic --thinking-budget 15000
python -m src.extraction.run_extraction --disorder ptsd --provider openai --reasoning-effort high

# Save extraction to production location
python -m src.extraction.run_extraction --disorder ptsd --provider anthropic --thinking-budget 15000 --production

# Test all providers at once
python -m src.extraction.run_extraction --disorder ptsd --all --save

# List available Ollama models
python -m src.extraction.run_extraction --list-models

# Prolog REPL (run from src/prolog/ directory)
cd src/prolog && swipl -g "[schema], ['gold_standard/loader']"
# In Prolog REPL:
# ?- disorder(X, Name, Category).          % List all disorders
# ?- symptom(mdd, S, Cat, Desc).           % List symptoms
# ?- validate_disorder(mdd, Issues).       % Validate a disorder
# ?- full_diagnosis(pt001, mdd, Result).   % Run full diagnosis

# Run tests
pytest                          # All tests
pytest -v                       # Verbose output
pytest tests/test_foo.py        # Single file
pytest -k "test_extraction"     # Pattern matching
```

## Prolog Predicate Structure

The schema defines these key predicates for disorder knowledge:
- `disorder/3` - disorder(ID, FullName, Category)
- `symptom/4` - symptom(DisorderID, SymptomID, Category, Description)
- `symptom_category/5` - groupings with count requirements (at_least, exactly, all, at_least_one_of)
- `duration_requirement/3`, `onset_requirement/3` - temporal constraints
- `exclusion_criterion/4` - what must NOT be present
- `subjective_criterion/4` - criteria requiring clinical judgment

Diagnostic inference predicates:
- `full_diagnosis/3` - comprehensive diagnosis with status tracking (met/not_met/missing_data)
- `criterion_check/5` - individual criterion status with details
- `collect_missing_data/3`, `generate_follow_up_questions/3` - gap analysis

## LLM Provider Configuration

| Provider | Thinking Parameter | Models |
|----------|-------------------|--------|
| OpenAI | `--reasoning-effort` (none/minimal/low/medium/high/xhigh) | gpt-5.2, gpt-5.1 |
| Anthropic | `--thinking-budget` (0 to disable, 1024+ to enable) | claude-sonnet-4-5, claude-opus-4-5 |
| Ollama | `--think` (low/medium/high) | gpt-oss:20b, deepseek-r1:*, qwq:32b |

## Adding New Disorders

1. Create DSM-5-TR source: `data/dsm5_text/{DISORDER}.txt`
2. Extract with LLM: `python -m src.extraction.run_extraction --disorder {id} --provider anthropic --production`
3. Or manually create: `src/prolog/gold_standard/{disorder_id}.pl` following the template guide
4. Register in `gold_standard/loader.pl` if gold standard

## Extraction Validation

Extractions are validated via:
1. Prolog syntax check (loads without errors)
2. Schema compliance via `validate_disorder/2`
3. Counts: symptoms defined, exclusions defined
4. Comparison with gold standard if available

Outputs saved to `outputs/extractions/` (with --save) or `src/prolog/extracted/` (with --production).
