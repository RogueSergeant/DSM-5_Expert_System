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

## Models Tested

| Model | Provider | Parameters | Status | Notes |
|-------|----------|------------|--------|-------|
| gpt-oss:20b | Ollama (local) | 20B | Testing | Initial test for PTSD |
| | | | | |

## Prompt Strategy

The prompt provides:
1. **Template guide**: `gold_standard/README.md` - explains predicate structure and requirements
2. **Source text**: Raw DSM-5 criteria for the target disorder
3. **Schema reference** (optional): `schema.pl` for predicate signatures

### Prompt Template
```
Following the guidance in the README.md file attached, create the content
of the <disorder>.pl file based on the <DISORDER>.txt content from the
DSM-5 I have sent here. Also attached is the schema.pl file for reference.
```

## Evaluation Criteria

| Criterion | Description | How to Test |
|-----------|-------------|-------------|
| Syntactic validity | .pl file loads without errors | `swipl -g "[schema], ['gold_standard/<disorder>']"` |
| Completeness | All criteria (A-F) captured | Compare against DSM-5 text |
| Accuracy | Symptoms/exclusions match DSM-5 | Manual review |
| Schema compliance | Correct predicate structure | `validate_disorder(<id>, Issues)` |

## Test Log

### Test 1: PTSD with gpt-oss:20b
- **Date**: 2025-12-19
- **Model**: gpt-oss:20b (Ollama)
- **Thinking Level**: High
- **Target**: PTSD.txt → ptsd.pl
- **Files attached**: `src/prolog/gold_standard/README.md`
- **Result**: Pending
- **Issues**: Waiting for Ollama API response

---

## Future Work
- Test with different model sizes (7B, 13B, 70B)
- Test with Claude API for comparison
- Develop automated validation pipeline
- Measure extraction accuracy vs hand-coded gold standards (MDD, GAD, ADHD)
