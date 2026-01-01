# DSM-5 Criteria Extraction Module

## Purpose
Automates extraction of DSM-5 diagnostic criteria from text into structured Prolog (.pl) files using LLMs. This is the "stochastic" component of the hybrid diagnostic system.

## Architecture
```
data/dsm5_text/<disorder>.txt     -->  [LLM Extraction]  -->  src/prolog/gold_standard/<disorder>.pl
                                            ^
                                            |
                              src/prolog/gold_standard/README.md (template guide)
                              src/prolog/schema.pl (predicate reference)
```

## Quick Start

### 1. Install dependencies
```bash
pip install -r requirements.txt
```

### 2. Configure API keys
```bash
cp .env.example .env
# Edit .env with your API keys
```

### 3. Run extraction
```bash
# Using Ollama (local) with thinking
python -m src.extraction.run_extraction --disorder ptsd --provider ollama --model gpt-oss:20b --think high

# Using OpenAI GPT-5 with reasoning
python -m src.extraction.run_extraction --disorder ptsd --provider openai --model gpt-5.2 --reasoning-effort high

# Using Claude with extended thinking
python -m src.extraction.run_extraction --disorder ptsd --provider anthropic --model claude-sonnet-4-5 --thinking-budget 10000

# Test all providers
python -m src.extraction.run_extraction --disorder ptsd --all --save
```

### 4. List available Ollama models
```bash
python -m src.extraction.run_extraction --list-models
```

## Module Structure

```
src/extraction/
├── __init__.py           # Package exports
├── base.py               # Base classes and utilities
├── config.py             # Configuration management
├── evaluate.py           # .pl file validation
├── run_extraction.py     # CLI entry point
├── README.md             # This file
└── providers/
    ├── __init__.py       # Provider factory
    ├── openai_provider.py    # OpenAI GPT-5 with reasoning_effort
    ├── anthropic_provider.py # Claude with extended thinking
    └── ollama_provider.py    # Ollama with think parameter
```

## Providers

| Provider | SDK | API Key Required | Thinking Parameter |
|----------|-----|------------------|-------------------|
| OpenAI | `openai>=1.0.0` | `OPENAI_API_KEY` | `reasoning_effort` |
| Anthropic | `anthropic>=0.40.0` | `ANTHROPIC_API_KEY` | `thinking` (budget_tokens) |
| Ollama | `ollama>=0.4.0` | None (local) | `think` |

## Models & Thinking Parameters

### OpenAI GPT-5 Series (December 2025)

| Model | Description | Reasoning Effort Levels |
|-------|-------------|------------------------|
| gpt-5.2 | Latest flagship with enhanced reasoning | none, minimal, low, medium, high, xhigh |
| gpt-5.1 | Previous generation, stable | none, minimal, low, medium, high |
| gpt-5 | Original GPT-5 release | minimal, low, medium, high |

**Reasoning Effort Parameter:**
```python
from src.extraction.providers import OpenAIProvider
provider = OpenAIProvider(
    model="gpt-5.2",
    reasoning_effort="high",  # none, minimal, low, medium, high, xhigh
)
```

CLI usage:
```bash
python -m src.extraction.run_extraction --provider openai --reasoning-effort high
```

### Anthropic Claude (December 2025)

| Model | Description | Thinking Budget Range |
|-------|-------------|----------------------|
| claude-sonnet-4-5 | Balanced speed/quality (recommended) | 1024 - 128000 |
| claude-opus-4-5 | Most capable, complex tasks | 1024 - 128000 |
| claude-haiku-3-5 | Fastest, simple extractions | 1024 - 128000 |

**Extended Thinking Parameter:**
```python
from src.extraction.providers import AnthropicProvider
provider = AnthropicProvider(
    model="claude-sonnet-4-5",
    thinking_budget=10000,  # 0 to disable, 1024+ to enable
)
```

Budget guidelines:
- 1024-5000: Light thinking for straightforward tasks
- 5000-15000: Moderate thinking (recommended for extraction)
- 15000-50000: Deep thinking for complex criteria
- 50000+: Maximum thinking for difficult edge cases

CLI usage:
```bash
python -m src.extraction.run_extraction --provider anthropic --thinking-budget 15000
```

### Ollama Local Models

| Model | Parameters | Think Support |
|-------|------------|---------------|
| gpt-oss:20b | 20B | "low", "medium", "high" |
| deepseek-r1:7b/14b/32b | 7-32B | True/False |
| qwq:32b | 32B | True/False |
| llama3.1:8b/70b | 8-70B | Not supported |

**Think Parameter:**
```python
from src.extraction.providers import OllamaProvider
provider = OllamaProvider(
    model="gpt-oss:20b",
    think="high",  # None, True, "low", "medium", "high"
)
```

CLI usage:
```bash
python -m src.extraction.run_extraction --provider ollama --model gpt-oss:20b --think high
```

## Models Tested

| Model | Provider | Parameters | Thinking | Status | Notes |
|-------|----------|------------|----------|--------|-------|
| gpt-oss:20b | Ollama | 20B | high | Testing | Initial test for PTSD |
| | | | | | |

## Prompt Strategy

The prompt provides:
1. **Template guide**: `gold_standard/README.md` - explains predicate structure and requirements
2. **Source text**: Raw DSM-5 criteria for the target disorder
3. **Schema reference** (optional): `schema.pl` for predicate signatures

### Prompt Template
```
You are an expert in both clinical psychology (DSM-5 diagnostic criteria) and Prolog programming.

Your task is to extract the diagnostic criteria from the DSM-5 text provided and convert it into a structured Prolog file following the template guide.

## Template Guide (how to structure the .pl file):
{template_guide}

## DSM-5 Text to Extract From:
{dsm5_text}

## Instructions:
1. Create a complete Prolog file for the disorder with ID: `{disorder_id}`
2. Follow the template guide exactly for predicate structure
3. Include ALL multifile declarations at the top
4. Extract ALL symptoms, grouping them by category
5. Include ALL exclusion criteria
6. Include duration and onset requirements
7. Add subjective criteria for clinical significance
8. Add relevant specifiers and differential features

Output ONLY the Prolog code, no explanations.
```

## Evaluation Criteria

| Criterion | Description | How to Test |
|-----------|-------------|-------------|
| Syntactic validity | .pl file loads without errors | `swipl -g "[schema], ['gold_standard/<disorder>']"` |
| Completeness | All criteria (A-F) captured | Compare against DSM-5 text |
| Accuracy | Symptoms/exclusions match DSM-5 | Manual review |
| Schema compliance | Correct predicate structure | `validate_disorder(<id>, Issues)` |

The `evaluate.py` module automates these checks:
- Syntax validation via SWI-Prolog
- Schema compliance via `validate_disorder/2`
- Comparison with gold standards (MDD, GAD, ADHD)

## Provider Comparison

See [Provider Evaluation](../../docs/PROVIDER_EVALUATION.md) for comprehensive LLM provider comparison (Anthropic vs OpenAI vs Ollama).

See [Extraction Benchmarks](../../docs/EXTRACTION_BENCHMARKS.md) for detailed test results, including:
- Performance metrics (latency, throughput, token efficiency)
- Quality benchmarks (criterion coverage, structural quality)
- Cost analysis
- Provider-specific issues and recommendations

## Test Log

Comprehensive test results have been moved to [Extraction Benchmarks](../../docs/EXTRACTION_BENCHMARKS.md). Summary of key findings:

- **Anthropic Claude Sonnet 4.5**: Best overall quality, most complete extractions
- **OpenAI GPT-5.2**: Best speed/quality ratio, 16x faster than Ollama
- **Ollama gpt-oss:20b**: Free but incomplete (missing Criterion A for PTSD at all thinking levels)

---

## Output Files

Extracted files are saved to `outputs/extractions/` with:
- `{disorder}_{provider}_{model}_{timestamp}.pl` - The extracted Prolog file
- `{disorder}_{provider}_{model}_{timestamp}.json` - Metadata and validation results

## CLI Reference

```bash
python -m src.extraction.run_extraction [OPTIONS]

Required:
  --disorder TEXT         Disorder ID to extract (e.g., ptsd, asd)

Provider Selection:
  --provider {openai,anthropic,ollama}  LLM provider to use
  --model TEXT            Specific model to use (overrides default)
  --all                   Test all available providers

Thinking Parameters:
  --reasoning-effort {none,minimal,low,medium,high,xhigh}
                          OpenAI GPT-5 reasoning effort level
  --thinking-budget INT   Anthropic extended thinking budget (1024+)
  --think {low,medium,high}
                          Ollama thinking level for supported models

Output:
  --save                  Save extracted .pl file to outputs directory
  --no-schema             Don't include schema.pl as reference

Utilities:
  --init-env              Create .env.example template file
  --list-models           List available Ollama models
```

## API Documentation

- [OpenAI Platform](https://platform.openai.com/docs/api-reference/chat)
- [OpenAI Reasoning Guide](https://platform.openai.com/docs/guides/reasoning)
- [Anthropic Docs](https://docs.anthropic.com/en/docs/)
- [Anthropic Extended Thinking](https://docs.anthropic.com/en/docs/build-with-claude/extended-thinking)
- [Ollama API](https://github.com/ollama/ollama/blob/main/docs/api.md)
- [Ollama Python SDK](https://github.com/ollama/ollama-python)
