# LLM Provider Evaluation

**Last Updated:** 2025-12-19
**Test Task:** Extract DSM-5 PTSD diagnostic criteria into structured Prolog format

This document summarizes the comparison of LLM providers for automated knowledge extraction from DSM-5 text.

## Executive Summary

Three LLM providers were tested for automated extraction of DSM-5 diagnostic criteria into Prolog format. All three produced syntactically valid output, but varied significantly in completeness and comprehensiveness.

**Recommendation:** Anthropic Claude Sonnet 4.5 produced the most complete extraction, including both adult and preschool criteria as separate disorders. OpenAI GPT-5.2 offers a good balance of speed and quality. Ollama gpt-oss:20b is viable for local/offline use but produces less comprehensive output.

## Provider Rankings

### 1. Anthropic Claude Sonnet 4.5 - Best Overall

**Strengths:**
- Most complete output with proper preschool criteria handling
- Consistent quality across thinking levels
- Best structural fidelity to DSM-5
- Moderate speed (81-105s)

**Use Cases:**
- Gold standard creation
- Complex disorders with developmental variants
- When completeness is critical

**Recommended Settings:**
```bash
python -m src.extraction.run_extraction \
  --provider anthropic \
  --model claude-sonnet-4-5 \
  --thinking-budget 10000-15000
```

### 2. OpenAI GPT-5.2 - Best Speed/Quality Ratio

**Strengths:**
- Complete adult criteria with comprehensive differentials
- Fastest provider (67-83s)
- Good documentation in output
- Best for batch processing multiple disorders

**Use Cases:**
- Rapid iteration during development
- Batch processing multiple disorders
- When speed matters

**Recommended Settings:**
```bash
python -m src.extraction.run_extraction \
  --provider openai \
  --model gpt-5.2 \
  --reasoning-effort medium
```

### 3. Ollama gpt-oss:20b - Not Recommended for PTSD

**Strengths:**
- No API costs
- Data stays local (privacy/offline use)
- Zero external dependencies

**Critical Limitation:**
- **Missing Criterion A at ALL thinking levels** (low, medium, high)
- This is a systematic model limitation, not a thinking depth issue
- Slowest provider (506-1345s)

**Use Cases:**
- Offline development (if validated for target disorder)
- Privacy-sensitive use cases
- Cost-constrained environments

**Important:** Requires manual review for completeness. May be viable for other disorders but needs validation.

**Settings (if used):**
```bash
python -m src.extraction.run_extraction \
  --provider ollama \
  --model gpt-oss:20b \
  --think high
```

## Performance Comparison

### Speed (Low Thinking Level)

| Provider | Model | Duration | Throughput |
|----------|-------|----------|------------|
| OpenAI | gpt-5.2 | **67.0s** | Fastest |
| Anthropic | claude-sonnet-4-5 | 81.4s | 21% slower |
| Ollama | gpt-oss:20b | 506.1s | 655% slower |

### Speed (High Thinking Level)

| Provider | Model | Duration | vs OpenAI |
|----------|-------|----------|-----------|
| OpenAI | gpt-5.2 | **83.4s** | 1x |
| Anthropic | claude-sonnet-4-5 | 105.3s | 1.26x |
| Ollama | gpt-oss:20b | 1345.5s | **16.1x** |

**Key Finding:** OpenAI high was 16x faster than Ollama high. Cloud APIs are significantly more efficient than local models for this task.

### Output Quality

| Provider | File Size | Criterion A | Preschool Criteria | Differential Features |
|----------|-----------|-------------|--------------------|-----------------------|
| Anthropic | **20.8 KB** | ✅ 4/4 | ✅ Full | Good |
| OpenAI | 15.2 KB | ✅ 4/4 | ⚠️ Partial | **Excellent** |
| Ollama | 5.4 KB | ❌ Missing | ❌ No | Basic |

### Token Usage

| Provider | Input Tokens | Output Tokens (low) | Output Tokens (high) |
|----------|--------------|---------------------|----------------------|
| Anthropic | 22,990 | 5,857 | 8,764 |
| OpenAI | 18,656 | 4,115 | 4,907 |
| Ollama | 18,982 | 2,452 | 7,646 |

**Observation:** Anthropic uses more input tokens (includes thinking tokens) but produces more comprehensive output.

## Cost Comparison

| Provider | Input Cost | Output Cost | Total (PTSD test) | Relative |
|----------|------------|-------------|-------------------|----------|
| Ollama | $0.00 | $0.00 | **$0.00** | Free |
| Anthropic | ~$0.07 | ~$0.09 | **$0.16** | Cheapest cloud |
| OpenAI | ~$0.19 | ~$0.16 | **$0.35** | 2.2x Anthropic |

*Note: Costs estimated based on December 2025 pricing. Actual costs depend on current rates.*

**Analysis:**
- Anthropic offers best cost/quality ratio for cloud providers
- Ollama is free but requires 16x more time and manual QA

## Criterion Coverage Comparison

### Low Thinking Level

| DSM-5 Criterion | Ollama (low) | OpenAI (low) | Anthropic (5k) |
|-----------------|--------------|--------------|----------------|
| **A: Trauma Exposure** (4 items) | ❌ Missing | ✅ 4/4 | ✅ 4/4 |
| **B: Intrusion** (5 items) | ✅ 5/5 | ✅ 5/5 | ✅ 5/5 |
| **C: Avoidance** (2 items) | ✅ 2/2 | ✅ 2/2 | ✅ 2/2 |
| **D: Negative Cognitions/Mood** (7 items) | ✅ 7/7 | ✅ 7/7 | ✅ 7/7 |
| **E: Arousal/Reactivity** (6 items) | ✅ 6/6 | ✅ 6/6 | ✅ 6/6 |
| **F: Duration** | ✅ Yes | ✅ Yes | ✅ Yes |
| **G: Clinical Significance** | ✅ Yes | ✅ Yes | ✅ Yes |
| **H: Exclusions** | ⚠️ 2 | ⚠️ 2 | ⚠️ 2 |
| **Preschool Criteria** | ❌ No | ⚠️ Partial | ✅ Full |

### High Thinking Level

| DSM-5 Criterion | Ollama (high) | OpenAI (high) | Anthropic (15k) |
|-----------------|---------------|---------------|-----------------|
| **A: Trauma Exposure** (4 items) | ❌ **Still Missing** | ✅ 4/4 | ✅ 4/4 |
| **B: Intrusion** (5 items) | ✅ 5/5 | ✅ 5/5 | ✅ 5/5 |
| **C: Avoidance** (2 items) | ✅ 2/2 | ✅ 2/2 | ✅ 2/2 |
| **D: Negative Cognitions/Mood** (7 items) | ✅ 7/7 | ✅ 7/7 | ✅ 7/7 |
| **E: Arousal/Reactivity** (6 items) | ✅ 6/6 | ✅ 6/6 | ✅ 6/6 |
| **F: Duration** | ✅ Yes | ✅ Yes | ✅ Yes |
| **G: Clinical Significance** | ✅ Yes | ✅ Yes | ✅ Yes |
| **H: Exclusions** | ⚠️ 2 | ⚠️ 2 | ⚠️ 2 |
| **Preschool Criteria** | ❌ No | ⚠️ Partial | ✅ Full |

**Critical Finding:** Ollama's Criterion A gap persists at high thinking level. This is a model limitation, not a thinking parameter issue.

## Key Findings

| Finding | Implication |
|---------|-------------|
| Ollama missing Criterion A is systematic | Model limitation, not thinking depth issue |
| High thinking doesn't always improve quality | Medium may be optimal for Ollama |
| Anthropic preschool handling is superior | Best for complex developmental criteria |
| OpenAI is 16x faster than Ollama high | Cloud APIs significantly more efficient |

## Recommendations by Use Case

### For Gold Standard Creation

**Provider:** Anthropic Claude Sonnet 4.5
**Settings:** `thinking_budget=10000-15000`

**Why:**
- Most complete output
- Correct handling of developmental variants
- Best structural fidelity to DSM-5

**Example:**
```bash
python -m src.extraction.run_extraction \
  --disorder ptsd \
  --provider anthropic \
  --thinking-budget 15000 \
  --production
```

### For Batch Processing

**Provider:** OpenAI GPT-5.2
**Settings:** `reasoning_effort=medium`

**Why:**
- Best speed/quality ratio
- Comprehensive differential features
- Good for processing multiple disorders

**Example:**
```bash
# Extract all disorders
for disorder in mdd gad adhd ptsd asd; do
  python -m src.extraction.run_extraction \
    --disorder $disorder \
    --provider openai \
    --reasoning-effort medium \
    --save
done
```

### For Offline/Local Use

**Provider:** Ollama gpt-oss:20b (with caveats)
**Settings:** `think=high`

**Why:**
- No API costs
- Data stays local
- No external dependencies

**Caveats:**
- Requires manual review for completeness
- May miss critical criteria (e.g., Criterion A for PTSD)
- 16x slower than cloud providers
- Validate on each disorder before trusting output

**Example:**
```bash
python -m src.extraction.run_extraction \
  --disorder mdd \
  --provider ollama \
  --model gpt-oss:20b \
  --think high \
  --save

# Then manually review output against DSM-5 text
```

## Thinking Level Analysis

### Ollama Thinking Levels

| Think Level | Duration | Output Tokens | Criterion A | Improvement |
|-------------|----------|---------------|-------------|-------------|
| low | 506.1s | 2,452 | ❌ Missing | Baseline |
| medium | 626.4s | 3,118 | ❌ Missing | +666 tokens, no fix |
| high | 1345.5s | 7,646 | ❌ Missing | +5,194 tokens, no fix |

**Conclusion:** Increasing thinking level did not fix the Criterion A gap. The issue is a systematic model limitation.

### Anthropic Thinking Budget

| Budget | Duration | Output Tokens | Completeness |
|--------|----------|---------------|--------------|
| 5,000 | 81.4s | 5,857 | Excellent |
| 15,000 | 105.3s | 8,764 | **Best** |

**Conclusion:** Higher budget improves completeness and detail. Recommended: 10,000-15,000 for production use.

### OpenAI Reasoning Effort

| Effort | Duration | Output Tokens | Quality |
|--------|----------|---------------|---------|
| low | 67.0s | 4,115 | Good |
| high | 83.4s | 4,907 | Better |

**Conclusion:** High effort adds minimal time (+16s) with noticeable quality improvement. Recommended: medium-high for production.

## Test Configuration

### Primary Test (Low Thinking)

| Parameter | OpenAI | Anthropic | Ollama |
|-----------|--------|-----------|--------|
| Thinking Level | `reasoning_effort=low` | `thinking_budget=5000` | `think=low` |
| Temperature | 1.0 | 1.0 | 1.0 |
| Max Tokens | 16,384 | 16,384 | 16,384 |

### Follow-up Test (High Thinking)

| Parameter | OpenAI | Anthropic | Ollama |
|-----------|--------|-----------|--------|
| Thinking Level | `reasoning_effort=high` | `thinking_budget=15000` | `think=high` |
| Temperature | 1.0 | 1.0 | 1.0 |
| Max Tokens | 16,384 | 16,384 | 16,384 |

## Validation Notes

All providers produced **syntactically valid** Prolog files that load without errors in SWI-Prolog. However, semantic completeness varied significantly.

**Validation Process:**
1. Syntax check via `swipl -g "consult('file.pl'), halt"`
2. Schema validation via `validate_disorder/2`
3. Manual review against DSM-5 source text

## Future Testing Recommendations

1. **Test with high/xhigh thinking** to compare quality improvement
2. **Test on more complex disorders** (e.g., Schizophrenia, Bipolar)
3. **Evaluate inter-rater reliability** across multiple runs
4. **Test smaller/faster models** for cost optimization (gpt-4o, claude-haiku)
5. **Test alternative Ollama models** (deepseek-r1, qwq) for Criterion A handling

## Related Documentation

- [Extraction Benchmarks](EXTRACTION_BENCHMARKS.md) - Detailed test results and logs
- [Architecture](ARCHITECTURE.md) - System design and LLM integration
- [Getting Started](GETTING_STARTED.md) - Running extractions
- [Extraction Module README](../src/extraction/README.md) - Provider configuration details

## Source Data

Full benchmark logs and extracted Prolog files:
- **Findings Report:** [outputs/extractions/FINDINGS_2025-12-19.md](../outputs/extractions/FINDINGS_2025-12-19.md)
- **Output Files:** [outputs/extractions/](../outputs/extractions/)
