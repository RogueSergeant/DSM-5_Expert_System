# Extraction Benchmarks

**Test Date:** 2025-12-19
**Test Task:** Extract DSM-5 PTSD diagnostic criteria into structured Prolog format
**Input File:** `data/dsm5_text/PTSD.txt` (18,656-22,990 tokens depending on provider tokenization)

This document provides detailed technical benchmarks for the LLM extraction pipeline.

For a summary and recommendations, see [Provider Evaluation](PROVIDER_EVALUATION.md).

## Test Matrix

### Providers Tested

| Provider | Model | SDK Version | API Endpoint |
|----------|-------|-------------|--------------|
| Anthropic | claude-sonnet-4-5-20251022 | anthropic>=0.40.0 | https://api.anthropic.com/v1/messages |
| OpenAI | gpt-5.2 | openai>=1.0.0 | https://api.openai.com/v1/chat/completions |
| Ollama | gpt-oss:20b | ollama>=0.4.0 | http://localhost:11434/api/chat |

### Thinking Parameters

| Provider | Parameter Name | Low | Medium | High |
|----------|----------------|-----|--------|------|
| Anthropic | `thinking_budget` | 5,000 tokens | 10,000 tokens | 15,000 tokens |
| OpenAI | `reasoning_effort` | `low` | `medium` | `high` |
| Ollama | `think` | `low` | `medium` | `high` |

### Shared Parameters

| Parameter | Value | Rationale |
|-----------|-------|-----------|
| `temperature` | 1.0 | Required by reasoning models (Claude/GPT-5) |
| `max_tokens` | 16,384 | Sufficient for full PTSD extraction |
| `top_p` | Default | Not overridden |

## Performance Benchmarks

### Latency Comparison

#### Low Thinking Level

| Provider | Model | Latency | Tokens/sec (out) | Relative Speed |
|----------|-------|---------|------------------|----------------|
| OpenAI | gpt-5.2 | 67.0s | 61.4 | 1.00x (baseline) |
| Anthropic | claude-sonnet-4-5 | 81.4s | 71.9 | 0.82x |
| Ollama | gpt-oss:20b | 506.1s | 4.8 | 0.13x |

#### High Thinking Level

| Provider | Model | Latency | Tokens/sec (out) | Relative Speed |
|----------|-------|---------|------------------|----------------|
| OpenAI | gpt-5.2 | 83.4s | 58.8 | 1.00x (baseline) |
| Anthropic | claude-sonnet-4-5 | 105.3s | 83.2 | 0.79x |
| Ollama | gpt-oss:20b | 1345.5s | 5.7 | 0.06x |

**Key Finding:** OpenAI is 16.1x faster than Ollama at high thinking level.

### Throughput Analysis

| Provider | Input Tok/s | Output Tok/s | Total Processing Time |
|----------|-------------|--------------|----------------------|
| OpenAI (high) | 223.7 | 58.8 | 83.4s |
| Anthropic (high) | 218.3 | 83.2 | 105.3s |
| Ollama (high) | 14.1 | 5.7 | 1345.5s |

**Analysis:** Cloud providers (OpenAI/Anthropic) have similar throughput. Ollama is 15-40x slower.

### Token Efficiency

| Provider | Thinking Level | Input Tokens | Output Tokens | Output/Input Ratio |
|----------|----------------|--------------|---------------|--------------------|
| Anthropic | low (5k) | 22,990 | 5,857 | 0.25 |
| Anthropic | high (15k) | 22,990 | 8,764 | 0.38 |
| OpenAI | low | 18,656 | 4,115 | 0.22 |
| OpenAI | high | 18,656 | 4,907 | 0.26 |
| Ollama | low | 18,982 | 2,452 | 0.13 |
| Ollama | high | 18,982 | 7,646 | 0.40 |

**Observation:** Higher thinking budgets increase output length, but not proportionally to budget increase.

## Quality Benchmarks

### Criterion Coverage

Full criterion-by-criterion breakdown:

#### Criterion A: Trauma Exposure (4 items)

| Provider | Think | ptsd_a1 | ptsd_a2 | ptsd_a3 | ptsd_a4 | Score |
|----------|-------|---------|---------|---------|---------|-------|
| Anthropic | low | ✅ | ✅ | ✅ | ✅ | **4/4** |
| Anthropic | high | ✅ | ✅ | ✅ | ✅ | **4/4** |
| OpenAI | low | ✅ | ✅ | ✅ | ✅ | **4/4** |
| OpenAI | high | ✅ | ✅ | ✅ | ✅ | **4/4** |
| Ollama | low | ❌ | ❌ | ❌ | ❌ | **0/4** |
| Ollama | medium | ❌ | ❌ | ❌ | ❌ | **0/4** |
| Ollama | high | ❌ | ❌ | ❌ | ❌ | **0/4** |

**Critical Finding:** Ollama systematically omits entire Criterion A at all thinking levels.

#### Criterion B: Intrusion Symptoms (5 items)

| Provider | Think | B1 | B2 | B3 | B4 | B5 | Score |
|----------|-------|----|----|----|----|----|-------|
| Anthropic | low | ✅ | ✅ | ✅ | ✅ | ✅ | 5/5 |
| OpenAI | low | ✅ | ✅ | ✅ | ✅ | ✅ | 5/5 |
| Ollama | low | ✅ | ✅ | ✅ | ✅ | ✅ | 5/5 |

#### Criterion C: Avoidance (2 items)

| Provider | Think | C1 | C2 | Score |
|----------|-------|----|----|-------|
| Anthropic | low | ✅ | ✅ | 2/2 |
| OpenAI | low | ✅ | ✅ | 2/2 |
| Ollama | low | ✅ | ✅ | 2/2 |

#### Criterion D: Negative Cognitions and Mood (7 items)

| Provider | Think | D1 | D2 | D3 | D4 | D5 | D6 | D7 | Score |
|----------|-------|----|----|----|----|----|----|----|----|
| Anthropic | low | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | 7/7 |
| OpenAI | low | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | 7/7 |
| Ollama | low | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | 7/7 |

#### Criterion E: Arousal and Reactivity (6 items)

| Provider | Think | E1 | E2 | E3 | E4 | E5 | E6 | Score |
|----------|-------|----|----|----|----|----|----|-------|
| Anthropic | low | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | 6/6 |
| OpenAI | low | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | 6/6 |
| Ollama | low | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | 6/6 |

### Structural Quality

| Feature | Anthropic | OpenAI | Ollama |
|---------|-----------|--------|--------|
| `disorder/3` declaration | ✅ | ✅ | ✅ |
| `symptom/4` facts | ✅ Complete | ✅ Complete | ⚠️ Missing A |
| `symptom_category/5` logic | ✅ Correct | ✅ Correct | ⚠️ Incomplete |
| `duration_requirement/3` | ✅ | ✅ | ✅ |
| `onset_requirement/3` | ✅ | ✅ | ✅ |
| `exclusion_criterion/4` | ✅ 2 | ✅ 2 | ✅ 2 |
| `subjective_criterion/4` | ✅ | ✅ | ✅ |
| Preschool criteria | ✅ Full | ⚠️ Partial | ❌ None |
| Differential features | ✅ Good | ✅ Excellent | ⚠️ Basic |
| Multifile declarations | ✅ | ✅ | ✅ |
| Syntax validity | ✅ Valid | ✅ Valid | ✅ Valid |

### File Size Metrics

| Provider | Think | File Size | Lines of Code | Bytes/Symptom |
|----------|-------|-----------|---------------|---------------|
| Anthropic | low (5k) | 20.8 KB | ~580 | ~780 |
| Anthropic | high (15k) | 26.5 KB | ~720 | ~995 |
| OpenAI | low | 15.2 KB | ~420 | ~570 |
| OpenAI | high | 18.3 KB | ~510 | ~685 |
| Ollama | low | 5.4 KB | ~150 | ~270 |
| Ollama | high | 4.5 KB | ~125 | ~225 |

**Analysis:** Ollama produces 3-5x smaller files, suggesting missing content beyond Criterion A.

## Cost Analysis

### Per-Extraction Cost

| Provider | Input Cost | Output Cost | Total | Cost/KB Output |
|----------|------------|-------------|-------|----------------|
| Anthropic (5k) | $0.07 | $0.09 | $0.16 | $0.0077 |
| Anthropic (15k) | $0.07 | $0.13 | $0.20 | $0.0075 |
| OpenAI (low) | $0.19 | $0.16 | $0.35 | $0.0230 |
| OpenAI (high) | $0.19 | $0.20 | $0.39 | $0.0213 |
| Ollama (any) | $0.00 | $0.00 | $0.00 | $0.0000 |

*Pricing based on December 2025 rates. Subject to change.*

### Batch Processing Estimates

Extracting all 5 disorders (MDD, GAD, ADHD, PTSD, ASD):

| Provider | Settings | Total Time | Total Cost | Cost per Disorder |
|----------|----------|------------|------------|-------------------|
| Anthropic | budget=15k | ~8 minutes | ~$1.00 | $0.20 |
| OpenAI | high | ~7 minutes | ~$1.95 | $0.39 |
| Ollama | high | ~112 minutes | $0.00 | $0.00 |

**Analysis:** Anthropic offers best cost/quality ratio. Ollama is free but takes 16x longer.

## Technical Implementation

### Validation Pipeline

All extractions passed through 3-stage validation:

```python
# Stage 1: Syntax Validation
swipl_result = subprocess.run(
    ['swipl', '-g', f"consult('{file}'), halt", '-t', 'halt(1)'],
    capture_output=True
)
syntax_valid = (swipl_result.returncode == 0)

# Stage 2: Schema Validation
engine = PrologEngine(Path('src/prolog'))
engine.load_file(extracted_file)
issues = engine.query(f"validate_disorder({disorder_id}, Issues)")

# Stage 3: Manual Review
# Check for completeness against DSM-5 source text
```

### Provider-Specific Issues

#### Anthropic Extended Thinking

**Issue:** Must set `temperature=1.0` when thinking is enabled.

**Solution:**
```python
if thinking_budget and thinking_budget > 0:
    params['temperature'] = 1.0
```

**Minimum Thinking Budget:** 1,024 tokens (enforced by API)

#### OpenAI Reasoning Effort

**Levels:** `none`, `minimal`, `low`, `medium`, `high`, `xhigh` (gpt-5.2 only)

**Recommendation:** Use `medium` or `high` for production. `xhigh` has diminishing returns.

#### Ollama Local Models

**Connection:** Requires Ollama server running on `localhost:11434`

**Model Pull:**
```bash
ollama pull gpt-oss:20b  # 12GB download
```

**Think Parameter Mapping:**
- `low` → Short reasoning chain
- `medium` → Medium reasoning chain
- `high` → Extended reasoning chain (166% slower than low)

## Output File Naming Convention

```
<disorder>_<provider>_<model>_<timestamp>.pl
<disorder>_<provider>_<model>_<timestamp>.json  # metadata
```

**Examples:**
```
ptsd_anthropic_claude-sonnet-4-5_20251219_154020.pl
ptsd_openai_gpt-5.2_20251219_154221.pl
ptsd_ollama_gpt-oss-20b_20251219_153819.pl
```

**Metadata JSON:**
```json
{
  "provider": "anthropic",
  "model": "claude-sonnet-4-5-20251022",
  "timestamp": "2025-12-19T15:40:20",
  "duration_seconds": 81.4,
  "input_tokens": 22990,
  "output_tokens": 5857,
  "thinking_budget": 5000,
  "syntax_valid": true,
  "file_size_bytes": 20832,
  "validation_issues": []
}
```

## Test Logs

Full extraction logs available at:
- **Detailed Report:** [outputs/extractions/FINDINGS_2025-12-19.md](../outputs/extractions/FINDINGS_2025-12-19.md)
- **Prolog Files:** `outputs/extractions/ptsd_*.pl`
- **Metadata:** `outputs/extractions/ptsd_*.json`

### Reproducing Tests

```bash
# Test all providers with low thinking
python -m src.extraction.run_extraction \
  --disorder ptsd \
  --all \
  --save

# Test Anthropic with high thinking
python -m src.extraction.run_extraction \
  --disorder ptsd \
  --provider anthropic \
  --thinking-budget 15000 \
  --save

# Test OpenAI with high reasoning
python -m src.extraction.run_extraction \
  --disorder ptsd \
  --provider openai \
  --reasoning-effort high \
  --save

# Test Ollama thinking levels
for think in low medium high; do
  python -m src.extraction.run_extraction \
    --disorder ptsd \
    --provider ollama \
    --model gpt-oss:20b \
    --think $think \
    --save
done
```

## Known Limitations

### Ollama gpt-oss:20b
- **Critical:** Missing Criterion A for PTSD at all thinking levels
- Systematic model limitation, not fixable via parameter tuning
- Other disorders may have similar gaps - validation required

### All Providers
- No providers extracted all exclusion criteria (got 2, DSM-5 has more)
- Differential features vary in completeness
- Preschool criteria handling inconsistent (only Anthropic got full)

## Future Testing

### Recommended Tests
1. Test on simpler disorders (MDD, GAD) to isolate PTSD-specific issues
2. Test alternative Ollama models (deepseek-r1:70b, qwq:32b)
3. Test smaller cloud models (gpt-4o, claude-haiku-4) for cost optimisation
4. Inter-rater reliability: Run same extraction 3-5 times, measure consistency
5. Test extraction from DSM-5-TR vs DSM-5 to validate version handling

### Metrics to Collect
- Precision/Recall vs gold standard
- Criterion coverage percentage
- Token efficiency (output quality per 1000 tokens)
- Cost per criterion extracted

## Related Documentation

- [Provider Evaluation](PROVIDER_EVALUATION.md) - Summary and recommendations
- [Architecture](ARCHITECTURE.md) - System design
- [Extraction Module](../src/extraction/README.md) - Implementation details
- [API Reference](API_REFERENCE.md) - Prolog schema reference
