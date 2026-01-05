# Vignette Generation

Clinical vignette generation for testing the diagnostic system.

## Overview

The vignette generator creates synthetic psychiatric case presentations with ground-truth answers keyed to Prolog symptom IDs. This enables automated evaluation of the diagnostic pipeline.

## Usage

```bash
python -m src.evaluation.generate_vignettes --count 50
```

Options:
- `--count N`: Number of vignettes to generate (default: 50)
- `--output PATH`: Custom output directory (default: `data/vignettes/`)

Output: `data/vignettes/vignettes_{timestamp}.json`

## Case Distribution

| Type | Weight | Description |
|------|--------|-------------|
| CLEAR | 40% | All symptoms clearly present, meets criteria |
| MODERATE | 35% | Minimum required symptoms, subtle presentation |
| AMBIGUOUS | 15% | Borderline case, 50% chance of meeting criteria |
| COMORBID | 10% | Two disorders present simultaneously |

Comorbid pairs: MDD+GAD, MDD+PTSD, GAD+PTSD, ADHD+GAD, ADHD+MDD

## Answer Generation

Answers respect Prolog `symptom_category/5` requirements:

1. **Category sorting**: Stricter requirements processed first (`exactly`/`all` before `at_least`)
2. **Overlap handling**: Tracks which symptoms already selected across categories
3. **Threshold calculation**: Adds only enough symptoms to meet each category's requirement

Example for MDD:
- `core_symptoms` [A1, A2] requires `at_least 1` -> selects A1
- `all_symptoms` [A1-A9] requires `at_least 5` -> A1 already present, adds 4 more

This ensures gateway criteria (like A1/A2 for MDD) are always satisfied.

## Prompt Methodology

Based on clinical vignette research and expert feedback:

### Present Symptoms
- Described phenomenologically (HOW they feel, not just existence)
- Woven into narrative prose, not listed
- Include direct patient quotes

### Absent Symptoms
- Explicitly denied as pertinent negatives
- "Denies feelings of worthlessness" rather than silent omission
- Prevents LLM from inferring absence = not assessed

### Required Sections
1. Opening with chief complaint in patient's words
2. History of present illness (narrative)
3. Past psychiatric history
4. Family history
5. Mental status examination

### Avoided Phrases
- "preserved interest" / "maintained interest" (contradicts anhedonia)
- "consistent with" / "suggestive of" (diagnostic language)
- Any disorder names or diagnostic labels

## Output Format

```json
{
  "id": "vig_000_mdd_moderate",
  "ground_truth": ["mdd"],
  "difficulty": "MODERATE",
  "meets_criteria": true,
  "clinical_text": "42-year-old accountant presents with...",
  "answers": {
    "mdd_a1": "present",
    "mdd_a2": "present",
    "mdd_a3": "absent",
    ...
    "mdd_exc_substance": "cleared",
    "mdd_subj_clinical_significance": "met",
    "duration_days": 21
  }
}
```

## Validation

Ground truth answers can be directly asserted into Prolog to verify:
1. Symptom category counts are satisfied
2. Duration requirements met
3. Exclusions cleared
4. Subjective criteria evaluated

The `meets_criteria` field indicates expected diagnostic outcome.
