# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Always Remember

It is 2026
Write in British English only
Avoid using =s to break up print statements
Write efficient, concise code - no unnecessary verbosity or fluff

## Logging Standards

All modules should log to `logs/{module_name}/` with timestamped files.

**Setup pattern:**
```python
import logging
from datetime import datetime
from pathlib import Path

LOG_DIR = Path(__file__).parent.parent.parent / 'logs' / 'module_name'
LOG_DIR.mkdir(parents=True, exist_ok=True)
LOG_FILE = LOG_DIR / f"{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter('%(asctime)s | %(levelname)s | %(message)s'))
logger.addHandler(file_handler)
```

**Requirements:**
- Logs go to files, not terminal (use `print()` for user-facing output)
- Format: `%(asctime)s | %(levelname)s | %(message)s`
- Use pipe `|` as delimiter for easy parsing
- Log file path printed to terminal at start: `print(f"Log file: {LOG_FILE}")`
- DEBUG for detailed tracing, INFO for key events, WARNING/ERROR for issues

## Project Overview

Hybrid Category 3 AI system combining Prolog-based symbolic reasoning (Tier A: objective criteria) with LLM-assisted judgment (Tier B: subjective criteria) for mental health diagnostic assessment using DSM-5-TR (2022).

**Target Disorders**: MDD, GAD, ADHD, PTSD, ASD

**DSM Version**: DSM-5-TR (2022), not DSM-5 (2013). See `docs/DSM_VERSION_NOTES.md` for differences.

## Architecture

**Three-Tier Hybrid Design:**
- **Tier A (Prolog)**: Objective criteria - symptom counts, duration, onset, exclusions → Transparent reasoning chains
- **Tier B (LLM)**: Subjective criteria - clinical significance, excessive worry → Confidence scores (0.0-1.0) with clinician override
- **Tier C (Prolog)**: Integration - combines A+B with confidence propagation → Final diagnosis

**Key Directories:**
```
src/
├── prolog/                    # Core Diagnostic System
│   ├── schema.pl             # Inference engine (~1,100 lines)
│   ├── gold_standard/        # Hand-curated disorders (5)
│   │   ├── mdd.pl, gad.pl, adhd.pl, ptsd.pl, asd.pl
│   │   └── loader.pl
│   └── extracted/            # LLM-extracted disorders
│
├── extraction/                # Layer 1: KB Construction
│   ├── run_extraction.py     # CLI entry point
│   ├── providers/            # OpenAI, Anthropic, Ollama
│   └── evaluate.py           # Validation pipeline
│
├── evaluation/                # Vignette Generation & Testing
│   ├── generate_vignettes.py # CLI for synthetic case generation
│   ├── evaluate.py           # CLI for vignette evaluation
│   └── answer_modes.py       # Answer mode factories (preextracted/interactive/llm)
│
├── diagnosis/                 # Diagnostic Driver
│   └── driver.py             # Orchestrates Prolog reasoning with answer callbacks
│
└── reasoning/                 # Python↔Prolog Interface
    └── engine.py             # Thin pyswip wrapper

data/
├── dsm5_text/                # DSM-5-TR source text
└── vignettes/                # Generated clinical test cases

logs/
├── vignettes/                # Vignette generation logs
└── evaluation/               # Evaluation run logs

docs/
├── DSM_VERSION_NOTES.md      # DSM-5 vs DSM-5-TR differences
├── EXTRACTION_BENCHMARKS.md  # LLM provider comparison
├── IMPLEMENTATION_PLAN.md    # Simplified rebuild plan
├── VIGNETTE_GENERATION.md    # Vignette generation architecture
├── EVALUATION.md             # Evaluation system and answer modes
└── LEGACY_SYSTEM.md          # Old architecture (archived)

archive/                       # Previous over-engineered implementation
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

**Testing** (pytest):
```bash
# Run all tests
pytest tests/ -v

# Run single test class
pytest tests/test_prolog_schema.py::TestSchemaLoading -v

# Run single test
pytest tests/test_prolog_schema.py::TestSchemaLoading::test_schema_loads -v

# Run with output shown
pytest tests/ -v -s
```

**LLM Extraction** (DSM text → Prolog):
```bash
# Recommended: Anthropic Claude (best quality)
python -m src.extraction.run_extraction --disorder ptsd --provider anthropic --thinking-budget 15000 --production

# Fastest: OpenAI GPT-5 (excellent quality, faster)
python -m src.extraction.run_extraction --disorder mdd --provider openai --reasoning-effort high --production

# Local/offline: Ollama (requires validation)
python -m src.extraction.run_extraction --disorder gad --provider ollama --model gpt-oss:20b --think high --save

# List available Ollama models
python -m src.extraction.run_extraction --list-models
```

**Vignette Generation** (synthetic clinical cases):
```bash
# Generate 50 vignettes (default)
python -m src.evaluation.generate_vignettes --count 50

# Generate 10 vignettes for quick testing
python -m src.evaluation.generate_vignettes --count 10

# Custom output directory
python -m src.evaluation.generate_vignettes --count 20 --output data/vignettes/custom/
```

Output: `data/vignettes/vignettes_{timestamp}.json`
Logs: `logs/vignettes/{timestamp}.log`

See `docs/VIGNETTE_GENERATION.md` for architecture and configuration details.

**Vignette Evaluation** (test diagnostic accuracy):
```bash
# Pre-extracted mode (fast baseline)
python -m src.evaluation.evaluate --vignettes data/vignettes/*.json

# LLM mode (GPT-5-mini infers from clinical text)
python -m src.evaluation.evaluate --vignettes data/vignettes/*.json --mode llm

# Interactive mode (clinician answers)
python -m src.evaluation.evaluate --vignettes data/vignettes/*.json --mode interactive

# Filter by disorder or difficulty
python -m src.evaluation.evaluate --vignettes data/vignettes/*.json --disorder mdd --difficulty CLEAR

# Tier B: Route subjective criteria to LLM (claude or openai)
python -m src.evaluation.evaluate --vignettes data/vignettes/*.json --mode preextracted --subjective-model claude
```

Logs: `logs/evaluation/{timestamp}.log`

See `docs/EVALUATION.md` for answer modes and metrics.

**Interactive Diagnosis** (single disorder, CLI prompts):
```bash
# Run interactive diagnosis for a specific disorder
python -m src.diagnosis.driver mdd
python -m src.diagnosis.driver gad
```

**Prolog Interactive Testing**:
```bash
cd src/prolog
swipl -g "[schema], ['gold_standard/loader']"

# In Prolog REPL:
?- disorder(X, Name, Category).          % List all disorders
?- symptom(mdd, S, Cat, Desc).           % List MDD symptoms
?- validate_disorder(mdd, Issues).       % Validate disorder definition
?- full_diagnosis(pt001, mdd, Result).   % Run full diagnosis
```

**Python↔Prolog** (via PrologEngine):
```python
from pathlib import Path
from src.reasoning.engine import PrologEngine

engine = PrologEngine(Path('src/prolog'))
engine.load_file('schema.pl')
engine.load_file('gold_standard/loader.pl')

# Query all disorders
results = engine.query("disorder(X, Name, Category)")

# Assert patient facts
engine.assert_fact("patient_symptom(pt001, mdd_a1, present, 'Reports sadness')")

# Run diagnosis
result = engine.query_one("full_diagnosis(pt001, mdd, Result)")
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

**Key Findings** (see `docs/EXTRACTION_BENCHMARKS.md`):
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

**Differential Diagnosis Mode** (used by evaluation):
1. All 5 disorders start as active candidates
2. `next_question/2` returns single next question (sorted by priority, deduplicated across disorders)
3. Answer callback provides: `(status, evidence, confidence, value)`
4. `disorder_pruned/2` automatically eliminates candidates as answers rule them out
5. Loop ends when all candidates resolved (met/not_met) or pruned

**Key Design Principles** (see `docs/IMPLEMENTATION_PLAN.md`):
1. Prolog does the reasoning — Python is just glue
2. Questions come from the KB — Not hardcoded in Python
3. Pruning is declarative — Prolog rules, not Python if/else
4. Simple ordering beats complex search — Clinical priority is good enough
5. Test on real vignettes — Not synthetic batch experiments
