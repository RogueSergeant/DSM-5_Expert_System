# Getting Started

This guide will help you set up and run the Hybrid Diagnostic Decision Support System.

## Prerequisites

Before you begin, ensure you have the following installed:

- **Python 3.10+** - Required for all Python modules
- **SWI-Prolog** - Required for pyswip and Prolog validation
  - macOS: `brew install swi-prolog`
  - Ubuntu/Debian: `sudo apt-get install swi-prolog`
  - Windows: Download from [SWI-Prolog website](https://www.swi-prolog.org/Download.html)
- **Git** - For cloning the repository

### API Keys (Optional)

You'll need API keys if you want to use external LLM providers for extraction:

- **OpenAI API Key** - For GPT-5 models (requires `OPENAI_API_KEY` environment variable)
- **Anthropic API Key** - For Claude models (requires `ANTHROPIC_API_KEY` environment variable)
- **Ollama** - For local models (no API key required, but requires Ollama installation)

## Installation

### 1. Clone the Repository

```bash
git clone <repository-url>
cd "Foundations of AI"
```

### 2. Create Virtual Environment

```bash
python -m venv .venv
source .venv/bin/activate  # On Windows: .venv\Scripts\activate
```

### 3. Install Dependencies

```bash
pip install -r requirements.txt
```

### 4. Configure API Keys (Optional)

If using external LLM providers:

```bash
cp .env.example .env
# Edit .env and add your API keys:
# OPENAI_API_KEY=your_openai_key_here
# ANTHROPIC_API_KEY=your_anthropic_key_here
```

### 5. Verify Installation

Test that SWI-Prolog is accessible:

```bash
swipl --version
```

Test that pyswip can load:

```bash
python -c "from pyswip import Prolog; print('pyswip OK')"
```

## Testing the Knowledge Base

### Interactive Prolog REPL

Load the Prolog knowledge base interactively:

```bash
cd src/prolog
swipl -g "[schema], ['gold_standard/loader']"
```

This will load:
- `schema.pl` - Core diagnostic inference engine
- `gold_standard/loader.pl` - All hand-curated disorders (MDD, GAD, ADHD, PTSD)

### Example Queries

Once in the Prolog REPL, try these queries:

```prolog
% List all disorders
?- disorder(X, Name, Category).

% List MDD symptoms
?- symptom(mdd, S, Cat, Desc).

% Validate MDD knowledge base
?- validate_disorder(mdd, Issues).

% Check symptom count requirement
?- symptom_category(mdd, CatID, Symptoms, Count, Type).

% Exit
?- halt.
```

### Python Interface

Test the Python Prolog interface:

```bash
python -m src.reasoning.engine
```

This runs a demo showing:
- Loading Prolog files
- Querying disorders and symptoms
- Asserting and retracting patient facts

Or explore programmatically:

```python
from pathlib import Path
from src.reasoning import PrologEngine, explore_disorder, list_disorders

# Initialize engine
engine = PrologEngine(Path('src/prolog'))
engine.load_file('schema.pl')
engine.load_file('gold_standard/loader.pl')

# List all disorders
disorders = list_disorders(engine)
print(f"Loaded {len(disorders)} disorders: {disorders}")

# Explore MDD
mdd_data = explore_disorder(engine, 'mdd')
print(f"MDD has {len(mdd_data['symptoms'])} symptoms")
print(f"Duration requirement: {mdd_data['duration']}")
```

## Running Your First Extraction

Extract DSM-5 diagnostic criteria from text using an LLM:

### Using Anthropic Claude (Recommended)

```bash
python -m src.extraction.run_extraction \
  --disorder mdd \
  --provider anthropic \
  --model claude-sonnet-4-5 \
  --thinking-budget 10000 \
  --save
```

This will:
1. Read `data/dsm5_text/MDD.txt`
2. Send to Claude with 10,000 token thinking budget
3. Parse Prolog output
4. Validate syntax and schema
5. Save to `outputs/extractions/mdd_anthropic_<timestamp>.pl`

### Using OpenAI GPT-5

```bash
python -m src.extraction.run_extraction \
  --disorder gad \
  --provider openai \
  --model gpt-5.2 \
  --reasoning-effort high \
  --save
```

### Using Ollama (Local)

First, ensure Ollama is running and you have a model installed:

```bash
ollama list  # List installed models
ollama pull gpt-oss:20b  # Pull a model if needed
```

Then run extraction:

```bash
python -m src.extraction.run_extraction \
  --disorder ptsd \
  --provider ollama \
  --model gpt-oss:20b \
  --think high \
  --save
```

### Test All Providers

Compare all providers for a disorder:

```bash
python -m src.extraction.run_extraction \
  --disorder adhd \
  --all \
  --save
```

This tests Anthropic, OpenAI, and Ollama sequentially and saves all results.

### Save to Production

To save directly to the production location (`src/prolog/extracted/`):

```bash
python -m src.extraction.run_extraction \
  --disorder asd \
  --provider anthropic \
  --production
```

## Running Diagnostic Search

The diagnostic search system finds optimal question sequences to reach a diagnosis:

### Debug Pruning Logic

Test exclusion and core symptom pruning:

```bash
python debug_pruning.py
```

This script:
- Simulates answering questions
- Tests exclusion pruning (e.g., manic episode → rules out MDD)
- Tests core symptom pruning (e.g., denying all GAD core symptoms)

### Debug ADHD Diagnosis

Test age-adjusted symptom counts and onset criteria:

```bash
python debug_adhd.py
```

This script:
- Sets patient age to 29 (adult)
- Asserts 5 inattention + 5 hyperactivity symptoms
- Tests age-adjusted count requirement (5 instead of 6 for adults)
- Verifies onset before age 12
- Checks all diagnostic criteria

### SessionManager Usage

Programmatic usage:

```python
from src.search.manager import SessionManager

# Initialize session
manager = SessionManager()
manager.start_new_session()

# Set patient data
manager.set_patient_data(age=29, gender='female')

# Answer questions
manager.answer_question('mdd_a1', 'YES')  # Depressed mood
manager.answer_question('mdd_a2', 'YES')  # Anhedonia
manager.answer_question('mdd_a4', 'YES')  # Sleep disturbance
# ... continue answering questions

# Check active candidates (auto-pruned based on answers)
print(f"Active candidates: {manager.state.active_candidates}")
```

## Running Evaluation Benchmark

Evaluate diagnostic accuracy on clinical vignettes:

```bash
python -m src.evaluation.benchmark
```

This will:
1. Load vignettes from `data/vignettes/`
2. Extract symptoms using LLM (ClinicalAnalyzer)
3. Run diagnostic search
4. Compare against ground truth diagnoses
5. Report accuracy metrics

### Custom Vignette

Test a single vignette:

```python
from src.evaluation.benchmark import ClinicalAnalyzer, run_diagnostic_benchmark

vignette = """
34-year-old female presenting with persistent low mood for 3 weeks.
Reports feeling sad and hopeless most days, lost interest in guitar
and socializing. Difficulty sleeping, waking at 4am. Constantly fatigued.
Feels worthless. No suicidal ideation. No substance use. No prior mania.
"""

analyzer = ClinicalAnalyzer()
result = analyzer.diagnose(vignette)
print(f"Diagnosis: {result.disorder_id}")
print(f"Confidence: {result.confidence}")
print(f"Explanation: {result.explanation}")
```

## Generating Visualizations

Create diagnostic flowcharts:

```bash
python -m src.reasoning.viz
```

This generates flowchart PNGs for all disorders in `docs/images/`.

Or programmatically:

```python
from pathlib import Path
from src.reasoning import PrologEngine, explore_disorder, visualise_diagnostic_flowchart

engine = PrologEngine(Path('src/prolog'))
engine.load_file('schema.pl')
engine.load_file('gold_standard/loader.pl')

mdd_data = explore_disorder(engine, 'mdd')
visualise_diagnostic_flowchart(
    disorder_id='mdd',
    disorder_data=mdd_data,
    save_to_file=Path('docs/images/mdd_flowchart.png')
)
```

## Next Steps

Now that you have the system running:

1. **Read the Architecture** - Understand the three-tier hybrid design: [Architecture](ARCHITECTURE.md)
2. **Explore the API** - Learn Prolog predicates and query patterns: [API Reference](API_REFERENCE.md)
3. **Compare Providers** - See LLM extraction benchmark results: [Provider Evaluation](PROVIDER_EVALUATION.md)
4. **Review Code** - Explore `src/` modules for implementation details

## Troubleshooting

### pyswip Import Error

**Error:** `ImportError: No module named 'pyswip'`

**Solution:** Ensure SWI-Prolog is installed and accessible:
```bash
swipl --version
pip install pyswip
```

### SWI-Prolog Not Found

**Error:** `Could not find SWI-Prolog`

**Solution:** Install SWI-Prolog and ensure it's in your PATH:
```bash
# macOS
brew install swi-prolog

# Linux
sudo apt-get install swi-prolog

# Verify
which swipl
```

### API Key Not Found

**Error:** `Anthropic API key not found`

**Solution:** Set environment variable or add to `.env`:
```bash
export ANTHROPIC_API_KEY='your-key-here'
# Or edit .env file
```

### Ollama Connection Error

**Error:** `Could not connect to Ollama`

**Solution:** Ensure Ollama is running:
```bash
# Start Ollama
ollama serve

# In another terminal, verify connection
ollama list
```

### Prolog Syntax Error

**Error:** `Syntax error in loaded Prolog file`

**Solution:** Validate the .pl file syntax:
```bash
cd src/prolog
swipl -g "consult('gold_standard/mdd.pl'), halt"
```

If errors persist, check the file against `gold_standard/README.md` template guide.

## Getting Help

- **Documentation:** See [docs/](README.md) for comprehensive guides
- **Issues:** Report bugs or request features on GitHub Issues
- **CLAUDE.md:** Reference guide for developers working with Claude Code
