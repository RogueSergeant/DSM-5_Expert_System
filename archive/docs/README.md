# Documentation Index

Welcome to the Hybrid Diagnostic Decision Support System documentation.

This system combines Prolog-based symbolic reasoning with LLM-assisted knowledge extraction to implement DSM-5-TR (2022) diagnostic criteria for mental health assessment.

## Quick Links

| Document | Description |
|----------|-------------|
| [Getting Started](GETTING_STARTED.md) | Installation, setup, and first steps |
| [Architecture](ARCHITECTURE.md) | System design, data flow, and module overview |
| [API Reference](API_REFERENCE.md) | Prolog predicate reference and usage examples |

## Guides

| Guide | Content |
|-------|---------|
| [Provider Evaluation](PROVIDER_EVALUATION.md) | LLM provider comparison (Anthropic, OpenAI, Ollama) |
| [Extraction Benchmarks](EXTRACTION_BENCHMARKS.md) | Detailed benchmark results and analysis |
| [DSM Version Notes](DSM_VERSION_NOTES.md) | DSM-5 vs DSM-5-TR differences by disorder |

## Archived Materials

| Archive | Description |
|---------|-------------|
| [notebooks/](notebooks/) | Jupyter notebooks (deprecated - see [`src/reasoning/`](../src/reasoning/)) |

## Module Documentation

| Module | README |
|--------|--------|
| Extraction Pipeline | [src/extraction/README.md](../src/extraction/README.md) |
| Gold Standard KB | [src/prolog/gold_standard/README.md](../src/prolog/gold_standard/README.md) |
| Reasoning Engine | [src/reasoning/](../src/reasoning/) |
| Search System | [src/search/](../src/search/) |
| Evaluation | [src/evaluation/](../src/evaluation/) |

## Project Structure

```
docs/                   # Documentation (you are here)
├── README.md          # This index
├── ARCHITECTURE.md    # System design
├── GETTING_STARTED.md # Quick start guide
├── API_REFERENCE.md   # Prolog API docs
├── PROVIDER_EVALUATION.md    # LLM comparison
├── EXTRACTION_BENCHMARKS.md  # Benchmark details
└── DSM_VERSION_NOTES.md      # DSM-5-TR changes

src/
├── prolog/            # Diagnostic knowledge base
│   ├── schema.pl     # Core inference engine
│   ├── gold_standard/ # Hand-curated disorders
│   └── extracted/    # LLM-extracted disorders
├── reasoning/         # Prolog engine & utilities
├── extraction/        # LLM extraction pipeline
├── search/            # Diagnostic pathway search
└── evaluation/        # Benchmark & testing

data/
├── dsm5_text/         # DSM-5-TR source text
└── vignettes/         # Test cases

outputs/
└── extractions/       # Timestamped extraction results
```

## Academic Context

This project was developed for the **Foundations of AI** module (MSc Applied AI, University of Bath) as a demonstration of hybrid AI systems combining symbolic and stochastic methods.

**Deadline:** 5th January 2026

## Quick Start

```bash
# Install dependencies
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt

# Test Prolog KB
cd src/prolog && swipl -g "[schema], ['gold_standard/loader']"

# Run extraction
python -m src.extraction.run_extraction --disorder mdd --provider anthropic

# Run diagnostic search
python -m src.search.manager

# Run evaluation benchmark
python -m src.evaluation.benchmark
```

For detailed instructions, see [Getting Started](GETTING_STARTED.md).
