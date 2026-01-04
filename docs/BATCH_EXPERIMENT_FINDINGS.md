# Batch Experiment Findings - Critical Issues Identified

> **Date**: January 3rd-4th, 2026
> **Author**: Alfie Roberts
> **Status**: Under Investigation - Validation Script Bugs Found

## ✅ CRITICAL UPDATE - VALIDATION FIXED (January 4th)

**Validation script bug has been FIXED**. The true sequential baseline accuracy is now known.

### The Bug
The validation script used text-based question mapping which failed because:
- Batch experiment stores questions in `questions[]` array with IDs
- Validation script was using `answers{}` dict keyed by text
- Text-based mapping couldn't handle prefixed questions ("Do you adhere to this criterion:")

### The Fix
Eliminated text mapping entirely - now uses question IDs directly from the `questions[]` array:
```python
# OLD: Text mapping (broken)
for question_text, answer in answers.items():
    question_id = question_id_map.get(question_text)  # Fails on prefixes

# NEW: Direct ID access (works)
for question in questions_list:
    question_id = question['id']  # Direct from array
    manager.answer_question(question_id, answer)
```

### True Sequential Baseline Accuracy

**Overall**: 50% exact matches, 60% partial matches (6/10 vignettes)

**By Disorder Type**:
- **GAD/MDD**: 100% accuracy (5/5 correct)
- **ADHD/PTSD/ASD**: 0% accuracy (0/5 correct - all returned "none")

**Single vs Comorbid**:
- Single-disorder: 55.6% (5/9)
- Comorbid: 0% exact, 100% partial (found MDD, missed PTSD)

### New Issue Discovered

**Systematic failure for ADHD, PTSD, and ASD validation** - all return "none" instead of valid diagnoses. This is NOT a validation script bug but a disorder-specific diagnostic criteria issue.

**Next Steps**:
1. ✅ Validation script fixed
2. ❌ Investigate ADHD/PTSD/ASD diagnostic failures
3. ❌ Fix disorder-specific criteria issues
4. ❌ Re-run validation to achieve ≥90% baseline accuracy
5. ❌ Then reassess batch experiment results

## Executive Summary (Original - See Update Above)

The batch vs sequential experiment revealed **two potential issues**:

1. **Sequential baseline validation showed 70% accuracy** - BUT this is based on buggy validation script (see update)
2. **Batch_all implementation is incomplete** - missing exclusion, subjective, duration, and onset questions ✓ CONFIRMED

**Current Status**: ⚠️ **Validation script must be fixed before drawing conclusions**

---

## Issue 1: Sequential Baseline Accuracy is 50%, Not 100% (CORRECTED)

### The Problem

The batch experiment measured **agreement** between batch modes and sequential baseline, assuming sequential was correct. This assumption is **false**.

### Validation Results (CORRECTED with ID-based mapping)

Running fixed [validate_sequential_diagnoses.py](../validate_sequential_diagnoses.py) against ground truth:

| Metric | Value |
|--------|-------|
| **Total Vignettes** | 10 |
| **Exact Matches** | 5 |
| **Partial Matches** | 6 |
| **Exact Accuracy** | **50.0%** |
| **Partial Accuracy** | **60.0%** |

**Correct Cases (5)**:
- `vig_1349_0_gad`: ✓ GAD (confidence: 0.90)
- `vig_3244_0_mdd`: ✓ MDD (confidence: 0.90)
- `vig_3315_1_gad`: ✓ GAD (confidence: 0.90)
- `vig_20260102_103450_000_mdd`: ✓ MDD (confidence: 0.90)
- `vig_20260102_103508_001_gad`: ✓ GAD (confidence: 0.90)

**Incorrect Cases (5)**:
- `vig_3349_2_adhd`: ✗ Predicted none, truth: ADHD
- `vig_3410_3_ptsd`: ✗ Predicted none, truth: PTSD
- `vig_3442_4_asd`: ✗ Predicted none, truth: ASD
- `vig_20260102_103551_002_adhd`: ✗ Predicted none, truth: ADHD
- `vig_20260102_103634_003_comorbid_ptsd_mdd`: Partial match - found MDD (✓), missed PTSD (✗)

**Pattern**: GAD and MDD diagnoses work perfectly (5/5). ADHD, PTSD, and ASD all return "none" (0/5).

### Why This Matters

The comparison report states:
- "batch_10: 57.8% agreement with sequential"
- "batch_5: 56.8% agreement with sequential"

**But this doesn't tell us**:
1. Whether batch modes are getting the **correct** diagnoses
2. Whether disagreements are batch making **errors** or **corrections**
3. Which mode is closer to ground truth

**Example**: If sequential predicts PTSD incorrectly, and batch_10 predicts GAD correctly, this counts as "disagreement" and lowers batch_10's agreement score - even though batch_10 was right!

---

## Issue 2: Batch_All Implementation is Incomplete

### The Problem

Batch_all mode asks **850 total questions** across 10 vignettes (85 per vignette), whilst sequential asks **1090 questions** (109 per vignette on average).

**Root Cause**: `_get_all_questions_for_vignette()` only queries symptoms via `get_candidate_symptoms()`, which excludes:
- ❌ Exclusion criteria (e.g., "Is it true symptoms are NOT due to substance use?")
- ❌ Subjective criteria (e.g., "Do symptoms cause clinically significant distress?")
- ❌ Duration checks (e.g., "Have symptoms persisted for ≥2 weeks?")
- ❌ Onset checks (e.g., "Did symptoms start before age 12?")

**Code Location**: [batch_experiment.py:482-512](../src/evaluation/batch_experiment.py#L482-L512)

```python
def _get_all_questions_for_vignette(self, vignette: Dict) -> List[str]:
    # ...
    for disorder in manager.state.active_candidates:
        # ONLY gets symptoms - missing other question types!
        symptoms = manager.get_candidate_symptoms(disorder)
        for symptom_id in symptoms:
            q_text = manager.get_symptom_description(symptom_id)
            questions.add(q_text)
    return list(questions)
```

### Impact

Without exclusion/subjective/duration questions, batch_all cannot make valid diagnoses because:
- **DSM-5-TR requires duration criteria** (e.g., MDD ≥2 weeks, GAD ≥6 months)
- **Exclusion criteria** are mandatory (e.g., "not due to substance use")
- **Subjective judgement** is required (e.g., "clinically significant distress")

This explains the low 41.5% agreement - batch_all is fundamentally incomplete.

---

## Current Experiment Results (Context Only)

From [comparison_report_openai.md](../outputs/batch_experiments/exp_20260103_144737/comparison_report_openai.md):

| Mode | Agreement | Speedup | LLM Calls | Questions |
|------|-----------|---------|-----------|-----------|
| Sequential | 100% (baseline) | 1.0x | 1090 | 1090 |
| batch_5 | 56.8% | 1.42x | 246 | 1209 |
| batch_10 | 57.8% | 0.59x | 128 | 1220 |
| batch_all | 41.5% | 3.44x | 10 | 850 |

**Problems with these results**:
1. ❌ Agreement measured against imperfect sequential baseline (70% accuracy)
2. ❌ Batch_all missing critical question types
3. ❌ No validation of final diagnoses against ground truth
4. ❌ batch_10 slower than sequential (negative speedup) - suggests implementation issue

---

## Required Actions

### Immediate (Before Using Results)

1. ✅ **Validate batch modes against ground truth** (not just sequential)
   - Run [validate_sequential_diagnoses.py](../validate_sequential_diagnoses.py) on all batch modes
   - Compare final diagnosis accuracy, not just answer-level agreement

2. ❌ **Fix batch_all implementation** to include all question types:
   - Add exclusion criteria questions
   - Add subjective criteria questions
   - Add duration/onset verification questions
   - Should generate ~109 questions per vignette, not 85

3. ❌ **Investigate batch_10 negative speedup**
   - Why is batch_10 slower than sequential?
   - Likely: batching 10 questions per call has higher per-call latency than 1 question per call
   - May need smaller batch sizes (2-5) for optimal speed

### Future Work

1. **Re-run experiment** with fixed batch_all implementation
2. **Add diagnosis-level metrics**:
   - Diagnosis accuracy (% vignettes with correct disorder)
   - Precision/recall for each disorder
   - F1 scores
3. **Compare all modes to ground truth**, not to each other
4. **Investigate sequential baseline errors** - why is it only 70% accurate?
   - Is A* search pruning correct diagnoses too early?
   - Are LLM answers inconsistent?
   - Are there bugs in Prolog diagnostic logic?

---

## Recommendations

### For This Project (Academic Submission)

**DO NOT** integrate batch processing into main workflow until:
1. Sequential baseline achieves ≥90% accuracy on ground truth
2. Batch_all implementation is complete and validated
3. All modes validated against ground truth (not just agreement with sequential)

**Current Recommendation**: Continue using sequential mode for all benchmarking and analyses.

### For Documentation

1. ✅ Document Ollama decision separately: [PROVIDER_DECISION.md](./PROVIDER_DECISION.md)
2. ✅ Document sequential baseline limitations
3. Document batch experiment as "exploratory work with significant limitations"
4. Include lessons learned:
   - Don't assume sequential is correct without validation
   - Measure final outcome metrics (diagnosis accuracy), not intermediate metrics (answer agreement)
   - Validate completeness of question generation before comparing modes

---

## Lessons Learned

### Methodological

1. **Always validate baselines** - sequential mode seemed correct but was only 70% accurate
2. **Measure what matters** - answer-level agreement ≠ diagnosis-level correctness
3. **Implementation completeness** - batch_all appeared to work but was missing critical questions

### Technical

1. **DSM-5-TR diagnosis is complex** - symptoms alone are insufficient
2. **Question generation must be exhaustive** - can't skip exclusions/subjective/duration
3. **Speedup ≠ batch size** - batch_10 was slower than sequential (latency vs throughput trade-off)

### Process

1. **Validation scripts are essential** - [validate_sequential_diagnoses.py](../validate_sequential_diagnoses.py) revealed both issues
2. **Ground truth is the only real measure** - agreement between two potentially wrong systems proves nothing
3. **Document limitations prominently** - don't bury negative results

---

## Files Generated

- [PROVIDER_DECISION.md](./PROVIDER_DECISION.md) - OpenAI vs Ollama decision
- [validate_sequential_diagnoses.py](../validate_sequential_diagnoses.py) - Validation script
- [openai_sequential_validation.json](../outputs/batch_experiments/exp_20260103_144737/openai_sequential_validation.json) - Validation results
- This document

---

## Next Steps

**Before proceeding with ANY batch integration**:

1. Fix sequential baseline to achieve ≥90% accuracy
2. Fix batch_all to include all question types
3. Re-run validation on all modes
4. Compare diagnosis accuracy (not answer agreement)
5. Only then consider production integration

**For academic submission**:
- Use sequential mode exclusively
- Document batch experiment as exploratory work
- Include validation results showing 70% baseline accuracy
- Discuss limitations and lessons learned
