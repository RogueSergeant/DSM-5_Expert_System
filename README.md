# Hybrid Diagnostic Decision Support System

A Category 3 hybrid AI system combining Prolog-based symbolic reasoning with LLM-assisted knowledge extraction for mental health diagnostic assessment using DSM-5-TR (2022) criteria.

## Overview

This system implements a three-tier architecture that separates objective diagnostic criteria (Tier A: Prolog) from subjective clinical judgment (Tier B: LLM), with integrated confidence scoring (Tier C: Prolog). The result is a diagnostic system that is both **explainable** (transparent reasoning chains) and **nuanced** (handles clinical judgment with uncertainty).

**Target Disorders:** MDD, GAD, ADHD, PTSD, ASD

## Architecture

```
┌──────────────────────────────────────────┐
│  Tier A: Objective (Prolog)              │
│  • Symptom counts (≥5 of 9)              │
│  • Duration (≥2 weeks)                   │
│  • Onset (before age 12)                 │
│  • Exclusions (not substance-induced)    │
│  → Transparent reasoning chains          │
└──────────────────────────────────────────┘
                   ↓
┌──────────────────────────────────────────┐
│  Tier B: Subjective (LLM)                │
│  • Clinical significance                 │
│  • Excessive worry/distress              │
│  • Functional impairment                 │
│  → Confidence scores + clinician override│
└──────────────────────────────────────────┘
                   ↓
┌──────────────────────────────────────────┐
│  Tier C: Integration (Prolog)            │
│  • Combine Tier A + Tier B               │
│  • Calculate final confidence            │
│  • Generate explanation                  │
│  → diagnosis_candidate(pt001, mdd, 0.92) │
└──────────────────────────────────────────┘
```

## Quick Start

### Prerequisites

- Python 3.10+
- SWI-Prolog (`brew install swi-prolog` on macOS)
- API keys for OpenAI/Anthropic (optional, for LLM features)

### Installation

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
cp .env.example .env  # Add API keys if needed
```

### Test the Knowledge Base

```bash
cd src/prolog
swipl -g "[schema], ['gold_standard/loader']"

# In Prolog REPL:
?- disorder(X, Name, Category).          % List all disorders
?- symptom(mdd, S, Cat, Desc).           % List MDD symptoms
?- validate_disorder(mdd, Issues).       % Validate KB
?- full_diagnosis(pt001, mdd, Result).   % Run diagnosis
```

### Extract Diagnostic Criteria (Layer 1)

```bash
# Using Anthropic Claude (recommended for quality)
python -m src.extraction.run_extraction \
  --disorder ptsd \
  --provider anthropic \
  --thinking-budget 15000 \
  --production

# Using OpenAI GPT-5 (fastest)
python -m src.extraction.run_extraction \
  --disorder mdd \
  --provider openai \
  --reasoning-effort high \
  --production
```

## Project Structure

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
└── reasoning/                 # Python↔Prolog Interface
    └── engine.py             # Thin pyswip wrapper

data/
├── dsm5_text/                # DSM-5-TR source text
└── vignettes/                # Clinical test cases

docs/
├── DSM_VERSION_NOTES.md      # DSM-5 vs DSM-5-TR differences
├── EXTRACTION_BENCHMARKS.md  # LLM provider comparison
├── IMPLEMENTATION_PLAN.md    # Simplified rebuild plan
└── LEGACY_SYSTEM.md          # Old architecture (archived)

archive/                       # Previous over-engineered implementation
```

## Key Features

### Prolog Knowledge Base
- DSM-5-TR criteria encoded as logical predicates
- Validates symptom counts, duration, onset, exclusions
- Transparent inference with full audit trail
- 5 disorders: MDD, GAD, ADHD, PTSD, ASD

### LLM Knowledge Extraction
- Converts DSM-5 text → structured Prolog rules
- Supports Anthropic Claude, OpenAI GPT-5, Ollama
- Validation pipeline ensures correctness
- See [Extraction Benchmarks](docs/EXTRACTION_BENCHMARKS.md)

### Tier B Subjective Assessment
- LLM evaluates subjective criteria with confidence scores
- Clinician can accept, modify, or override suggestions
- Handles "clinically significant", "excessive worry", etc.

## Documentation

- [DSM Version Notes](docs/DSM_VERSION_NOTES.md) - DSM-5 vs DSM-5-TR
- [Extraction Benchmarks](docs/EXTRACTION_BENCHMARKS.md) - Provider comparison
- [Implementation Plan](docs/IMPLEMENTATION_PLAN.md) - Simplified architecture
- [Legacy System](docs/LEGACY_SYSTEM.md) - Why we simplified

## Academic Context

Developed for **Foundations of AI** (MSc Applied AI) as a demonstration of hybrid AI combining symbolic and stochastic methods.

**Deadline:** 5th January 2026

## Technical Stack

| Component | Technology |
|-----------|------------|
| Symbolic Reasoning | SWI-Prolog |
| Python Interface | pyswip |
| LLM Integration | Anthropic/OpenAI/Ollama APIs |

---

**Note:** This is a research prototype for academic purposes. Not intended for clinical use without appropriate oversight and validation.
