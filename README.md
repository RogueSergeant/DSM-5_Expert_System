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
│   ├── schema.pl             # Inference engine (~1,119 lines)
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
├── diagnosis/                 # Diagnostic Driver
│   └── driver.py             # Orchestrates Prolog reasoning
│
├── evaluation/                # Vignette Generation & Testing
│   ├── generate_vignettes.py # Synthetic case generation
│   ├── evaluate.py           # Vignette evaluation
│   ├── compare_llm.py        # Hybrid vs Pure LLM comparison
│   └── answer_modes.py       # Answer mode factories
│
├── reasoning/                 # Python↔Prolog Interface
│   └── engine.py             # Thin pyswip wrapper
│
└── utils/                     # Formatting & Explanation
    ├── formatting.py         # ANSI colours, tables
    └── explain.py            # Proof tree formatter

data/
├── dsm5_text/                # DSM-5-TR source text
├── vignettes/                # Generated clinical test cases
└── results/                  # Evaluation outputs and figures

tests/                         # 80 tests across 5 files

docs/
├── DSM_VERSION_NOTES.md      # DSM-5 vs DSM-5-TR differences
├── EXTRACTION_BENCHMARKS.md  # LLM provider comparison
├── IMPLEMENTATION_PLAN.md    # Simplified rebuild plan
├── VIGNETTE_GENERATION.md    # Vignette generation architecture
├── EVALUATION.md             # Evaluation system and answer modes
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

### Evaluation Pipeline
- Synthetic clinical vignette generation with ground truth
- Multiple answer modes: preextracted, interactive, LLM, hybrid
- Comparative analysis: Hybrid vs Pure LLM approaches
- Visualisation of evaluation metrics

## Current Results

| Metric | Value |
|--------|-------|
| Diagnostic accuracy | 100% (53/53 vignette evaluations) |
| Hybrid vs Pure LLM | +22% advantage (100% vs 78%) |
| Average questions | 133.4 per vignette |
| Test coverage | 80 tests, all passing |

## Documentation

- [DSM Version Notes](docs/DSM_VERSION_NOTES.md) - DSM-5 vs DSM-5-TR differences
- [Extraction Benchmarks](docs/EXTRACTION_BENCHMARKS.md) - LLM provider comparison
- [Vignette Generation](docs/VIGNETTE_GENERATION.md) - Synthetic case generation
- [Evaluation](docs/EVALUATION.md) - Evaluation system and answer modes
- [Implementation Plan](docs/IMPLEMENTATION_PLAN.md) - Simplified architecture
- [Legacy System](docs/LEGACY_SYSTEM.md) - Previous architecture (archived)

## Academic Context

Developed for **Foundations of AI** (MSc Applied AI, 7COSC013W.1) as a demonstration of hybrid AI combining symbolic and stochastic methods.

## Technical Stack

| Component | Technology |
|-----------|------------|
| Symbolic Reasoning | SWI-Prolog |
| Python Interface | pyswip |
| LLM Integration | Anthropic/OpenAI/Ollama APIs |

---

**Note:** This is a research prototype for academic purposes. Not intended for clinical use without appropriate oversight and validation.
