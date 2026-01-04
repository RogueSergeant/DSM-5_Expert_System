# Hybrid Diagnostic Decision Support System

A Category 3 hybrid AI system combining Prolog-based symbolic reasoning with LLM-assisted knowledge extraction for mental health diagnostic assessment using DSM-5-TR (2022) criteria.

## Overview

This system implements a novel three-tier architecture that separates objective diagnostic criteria (Tier A: Prolog) from subjective clinical judgment (Tier B: LLM), with integrated confidence scoring (Tier C: Prolog). The result is a diagnostic system that is both **explainable** (transparent reasoning chains) and **nuanced** (handles clinical judgment with uncertainty).

**Target Disorders:**
- Major Depressive Disorder (MDD)
- Generalized Anxiety Disorder (GAD)
- Attention-Deficit/Hyperactivity Disorder (ADHD)
- Posttraumatic Stress Disorder (PTSD)
- Autism Spectrum Disorder (ASD)

## Key Features

### 🔗 Hybrid Architecture
- **Symbolic (Prolog):** Encodes DSM-5 diagnostic rules as logical predicates for objective criteria (symptom counts, duration, onset, exclusions)
- **Stochastic (LLM):** Handles subjective assessments requiring clinical judgment (e.g., "clinically significant distress", "excessive worry")
- **Integration:** Combines both approaches with confidence propagation for final diagnosis

### 🔍 Explainable Reasoning
- Complete audit trail of which criteria were met/not met
- Transparent Prolog inference chains for objective criteria
- LLM confidence scores for subjective assessments
- Missing data tracking for incomplete evaluations

### 🤖 Automated Knowledge Extraction
- LLM pipeline converts DSM-5 text → structured Prolog rules
- Supports multiple providers: Anthropic Claude, OpenAI GPT-5, Ollama (local)
- Validation pipeline ensures syntactic and semantic correctness
- See [Provider Evaluation](docs/PROVIDER_EVALUATION.md) for benchmarks

### 🎯 Diagnostic Pathway Optimisation
- A* search finds optimal question sequences
- Pruning eliminates ruled-out diagnoses early
- Verification mode boosts high-value questions
- Reduces questions from 40-50 (exhaustive) to 15-25 (optimised)

### 📊 Comprehensive Testing
- Clinical vignette benchmarks
- Gold standard validation
- Provider comparison (Anthropic vs OpenAI vs Ollama)
- Extraction quality metrics

## Quick Start

### Prerequisites

- Python 3.10+
- SWI-Prolog (install via `brew install swi-prolog` on macOS)
- API keys for OpenAI/Anthropic (optional - for extraction only)

### Installation

```bash
# Clone repository
git clone <repository-url>
cd "Foundations of AI"

# Create virtual environment
python -m venv .venv
source .venv/bin/activate  # Windows: .venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Configure API keys (optional)
cp .env.example .env
# Edit .env to add OPENAI_API_KEY and/or ANTHROPIC_API_KEY
```

### Test the Knowledge Base

```bash
# Interactive Prolog REPL
cd src/prolog
swipl -g "[schema], ['gold_standard/loader']"

# In Prolog:
# ?- disorder(X, Name, Category).  % List all disorders
# ?- symptom(mdd, S, Cat, Desc).   % List MDD symptoms
# ?- validate_disorder(mdd, Issues). % Validate knowledge base
```

### Run Diagnostic Search

```bash
# Test pruning logic
python debug_pruning.py

# Test ADHD diagnosis with age-adjusted counts
python debug_adhd.py
```

### Extract Diagnostic Criteria

```bash
# Using Anthropic Claude (recommended)
python -m src.extraction.run_extraction \
  --disorder ptsd \
  --provider anthropic \
  --thinking-budget 15000 \
  --save

# Using OpenAI GPT-5 (fastest)
python -m src.extraction.run_extraction \
  --disorder mdd \
  --provider openai \
  --reasoning-effort high \
  --save

# Using Ollama (local/offline)
python -m src.extraction.run_extraction \
  --disorder gad \
  --provider ollama \
  --model gpt-oss:20b \
  --think high \
  --save
```

### Run Evaluation Benchmark

```bash
python -m src.evaluation.benchmark
```

## Project Structure

```
├── src/
│   ├── prolog/                    # Knowledge Base
│   │   ├── schema.pl             # Core diagnostic inference engine
│   │   ├── gold_standard/        # Hand-curated disorder definitions
│   │   │   ├── mdd.pl           # Major Depressive Disorder
│   │   │   ├── gad.pl           # Generalized Anxiety Disorder
│   │   │   ├── adhd.pl          # ADHD
│   │   │   └── ptsd.pl          # PTSD
│   │   └── extracted/            # LLM-extracted disorders
│   ├── reasoning/                 # Prolog interface & utilities
│   │   ├── engine.py            # PrologEngine wrapper
│   │   ├── utils.py             # KB exploration tools
│   │   └── viz.py               # Diagnostic flowcharts
│   ├── extraction/                # LLM extraction pipeline
│   │   ├── run_extraction.py    # CLI entry point
│   │   └── providers/           # OpenAI, Anthropic, Ollama
│   ├── search/                    # Diagnostic pathway optimisation
│   │   ├── manager.py           # SessionManager with pruning
│   │   └── search.py            # A* search implementation
│   └── evaluation/                # Benchmarking
│       └── benchmark.py         # Vignette evaluation
│
├── data/
│   ├── dsm5_text/                # DSM-5-TR source text
│   └── vignettes/                # Clinical test cases
│
├── docs/                          # Documentation
│   ├── ARCHITECTURE.md           # System design
│   ├── GETTING_STARTED.md        # Setup guide
│   ├── API_REFERENCE.md          # Prolog predicate reference
│   ├── PROVIDER_EVALUATION.md    # LLM benchmarks
│   └── EXTRACTION_BENCHMARKS.md  # Detailed test results
│
└── outputs/
    └── extractions/              # Timestamped extraction results
```

## Documentation

- **[Getting Started](docs/GETTING_STARTED.md)** - Installation and first steps
- **[Architecture](docs/ARCHITECTURE.md)** - System design and three-tier reasoning
- **[API Reference](docs/API_REFERENCE.md)** - Prolog predicate documentation
- **[Provider Evaluation](docs/PROVIDER_EVALUATION.md)** - LLM comparison (Anthropic vs OpenAI vs Ollama)
- **[Extraction Benchmarks](docs/EXTRACTION_BENCHMARKS.md)** - Detailed test results
- **[DSM Version Notes](docs/DSM_VERSION_NOTES.md)** - DSM-5 vs DSM-5-TR differences

## Key Results

### LLM Extraction Quality

| Provider | Completeness | Speed | Cost | Recommendation |
|----------|--------------|-------|------|----------------|
| **Anthropic Claude Sonnet 4.5** | ✅ Best | Medium (81s) | $0.16 | Gold standard creation |
| **OpenAI GPT-5.2** | ✅ Excellent | **Fastest** (67s) | $0.35 | Batch processing |
| **Ollama gpt-oss:20b** | ⚠️ Incomplete* | Slowest (506s) | Free | Local/offline (with validation) |

*Ollama missing critical Criterion A for PTSD at all thinking levels

See [Provider Evaluation](docs/PROVIDER_EVALUATION.md) for full benchmarks.

### Diagnostic Search Efficiency

- **Question Reduction:** 40-60% vs exhaustive search
- **Pruning:** Eliminates candidates early based on exclusions/core symptoms
- **Verification Mode:** Boosts onset/duration questions when symptoms met

## Three-Tier Architecture

```
┌──────────────────────────────────────────┐
│  Tier A: Objective (Prolog)             │
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
│  → Confidence scores (0.0-1.0)           │
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

## Design Rationale

### Why Prolog for Tier A?

- DSM-5 criteria are inherently rule-based and logical
- Natural expression of diagnostic decision trees
- Built-in backtracking for criterion checking
- Complete audit trail for regulatory/clinical review
- Validated against gold standard

### Why LLM for Tier B?

- Subjective criteria require nuanced clinical judgment
- Cannot be captured by keyword matching or rigid rules
- LLMs excel at "excessiveness", "clinical significance" assessment
- Confidence scores quantify uncertainty
- Allows clinician override

### Why Separate Tiers?

- **Explainability:** Tier A provides transparent reasoning
- **Validation:** Tier A can be validated against gold standard
- **Reliability:** Tier A is deterministic (no hallucination risk)
- **Efficiency:** Tier B only called for 1-2 subjective criteria per disorder

## Academic Context

This project was developed for the **Foundations of AI** module (MSc Applied AI, University of Bath) as a demonstration of hybrid AI systems combining symbolic and stochastic methods.

**Key Learning Outcomes:**
- Hybrid AI architecture design (Category 3)
- Knowledge representation with logic programming (Prolog)
- LLM integration for knowledge extraction
- Search optimisation (A* for diagnostic pathways)
- Explainable AI for high-stakes domains (healthcare)
- Benchmark design and evaluation

**Deadline:** 5th January 2026

## Technical Stack

| Component | Technology | Purpose |
|-----------|------------|---------|
| Symbolic Reasoning | SWI-Prolog | Diagnostic inference engine |
| Python Interface | pyswip | Prolog ↔ Python bridge |
| LLM Integration | Anthropic/OpenAI/Ollama APIs | Knowledge extraction & subjective assessment |
| Search | A* algorithm | Pathway optimisation |
| Visualization | matplotlib, networkx | Diagnostic flowcharts |
| Testing | pytest, clinical vignettes | Validation & benchmarking |

## License

This is an academic project developed for educational purposes.

## Contributing

This is a coursework project and is not accepting external contributions.

## Acknowledgments

- **DSM-5-TR (2022)** - American Psychiatric Association
- **SWI-Prolog** - Jan Wielemaker et al.
- **pyswip** - Yuce Tekol
- **Anthropic Claude** - For LLM extraction and subjective assessment
- **OpenAI GPT-5** - For comparative benchmarks

## Contact

For questions about this project, please refer to the [documentation](docs/README.md) or review the codebase.

---

**Note:** This system is a research prototype for academic purposes only. It is not intended for clinical use and should not be used for real patient diagnosis without appropriate clinical oversight and validation.
