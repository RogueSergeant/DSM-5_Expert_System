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

## Architecture

### Ground Truth First

The system generates ground truth **before** the vignette text:

```
Prolog KB → generate_answers() → answers dict → build_prompt() → LLM → vignette text
```

This ensures diagnostic validity is determined by the knowledge base, not LLM interpretation. The vignette becomes a "rendering" of a pre-determined clinical state.

### Verification Layer

Each generated vignette is verified before storage:
- Checks for forbidden diagnostic labels
- Validates duration/timeline presence
- Confirms pertinent negatives included
- Verifies onset age stated (for relevant disorders)
- Checks medical context (labs, substances, medications)

Failed verification triggers automatic retry (up to 2 retries).

## Case Distribution

| Type | Weight | Description |
|------|--------|-------------|
| CLEAR | 40% | All symptoms clearly present, meets criteria |
| MODERATE | 35% | Minimum required symptoms, subtle presentation |
| AMBIGUOUS | 15% | Borderline case, 50% chance of meeting criteria |
| COMORBID | 10% | Two disorders present simultaneously |

Comorbid pairs: MDD+GAD, MDD+PTSD, GAD+PTSD, ADHD+GAD, ADHD+MDD

## Answer Generation

### Category Requirements

Answers respect Prolog `symptom_category/5` requirements:

1. **Category sorting**: Stricter requirements first (`exactly`/`all` before `at_least`)
2. **Overlap handling**: Tracks symptoms already selected across categories
3. **Threshold calculation**: Adds only enough symptoms to meet each category's requirement

Example for MDD:
- `core_symptoms` [A1, A2] requires `at_least 1` → selects A1
- `all_symptoms` [A1-A9] requires `at_least 5` → A1 already present, adds 4 more

This ensures gateway criteria (like A1/A2 for MDD) are always satisfied.

### Comorbid Deduplication

For comorbid cases, symptoms are deduplicated across disorders to prevent:
- Duplicate symptom descriptions in prompt
- Inconsistent symptom portrayals in generated text

## Prompt Design

### Key Principles

Based on clinical vignette research and expert feedback:

| Requirement | Implementation |
|-------------|----------------|
| Phenomenological descriptions | Instruct LLM to describe HOW symptoms feel, not just presence |
| Pertinent negatives | Explicitly deny absent symptoms (scaled 4-8 based on complexity) |
| No diagnostic labels | Forbidden phrases list with verification |
| Exclusion evidence | Labs, substances, medications dynamically based on disorder |
| Onset requirements | Explicit instruction when disorder has before_age onset |
| Functional impairment | Required section for work/relationships/daily activities |
| MSE depth | Category-specific requirements (psychotic, suicidal, cognitive) |

### Avoided Patterns

These phrases create contradictions or leak diagnostic intent:
- "preserved interest" / "maintained interest" (contradicts anhedonia)
- "consistent with" / "suggestive of" (diagnostic language)
- Any DSM disorder names or abbreviations

### Dynamic Prompt Sections

#### Exclusion Criteria Evidence

Generated based on disorder's `exclusion_criterion` predicates:

```python
# If disorder excludes substance causes:
"- Substance use: alcohol frequency, recreational drugs, caffeine"
# If disorder excludes medical causes:
"- Labs: 'TSH normal', 'metabolic panel unremarkable', 'toxicology negative'"
```

#### MSE Requirements

Scaled based on symptom content:

| Symptom Keywords | MSE Focus |
|------------------|-----------|
| hallucination, delusion | Thought content, perceptions, insight |
| suicide, death, self-harm | Suicidal ideation (frequency, plan, intent, protective factors) |
| memory, cognitive, orientation | Detailed cognitive testing |
| Default | Standard appearance, affect, thought process, cognition |

#### Onset Age

For disorders with `onset_requirement(DisorderID, before_age, Age)`:

```
ONSET: Symptoms first appeared around age 10. State this explicitly.
```

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
    "mdd_exc_substance": "cleared",
    "mdd_subj_clinical_significance": "met",
    "duration_days": 21
  },
  "verification": {
    "valid": true,
    "issues": [],
    "warnings": ["No laboratory investigations mentioned"],
    "word_count": 312
  }
}
```

## Ground Truth / Vignette Alignment

### What Ground Truth Tracks

| Field | Meaning | Evidence Required in Vignette |
|-------|---------|------------------------------|
| `symptom_id: present` | Symptom present | Phenomenological description |
| `symptom_id: absent` | Symptom absent | Explicit denial |
| `exclusion_id: cleared` | Exclusion ruled out | Labs, substance history, medication list |
| `subjective_id: met` | Clinical significance met | Functional impairment description |
| `duration_days` | Symptom duration | Clear timeline in HPI |
| `onset_age` | Age at symptom onset | Explicit statement of childhood onset |

### Verification Checks

The `verify_vignette()` function checks:

| Check | Failure Criteria | Retry? |
|-------|------------------|--------|
| Diagnostic labels | Contains "MDD", "ADHD", etc. | Yes |
| Duration | No temporal patterns found | Yes |
| Pertinent negatives | < 2 denial patterns | Yes |
| Onset age | Required but not stated | Yes |
| Word count | < 200 words | Yes |
| Labs/substances/meds | Not mentioned | Warning only |

## Evaluation Considerations

### Testing AI Diagnostic Systems

When using vignettes to evaluate an AI diagnostic system:

1. **Symptom extraction**: Can it identify symptoms from natural language?
2. **Threshold application**: Does it apply correct DSM counts?
3. **Exclusion handling**: Does it require evidence to clear exclusions?
4. **Ambiguity handling**: Does it express appropriate uncertainty?
5. **Comorbidity detection**: Can it identify multiple disorders?

### Potential False Negatives

An AI system may correctly say "insufficient information" when:
- Labs not mentioned (can't rule out medical cause)
- Onset age unclear (can't confirm childhood onset for ADHD)
- Functional impairment vague (can't confirm clinical significance)

These are not necessarily errors - they may indicate appropriate clinical caution.

### Recommended Metrics

```python
# Per-difficulty accuracy
by_difficulty = {
    'CLEAR': {'tp': 0, 'fp': 0, 'fn': 0},
    'MODERATE': {'tp': 0, 'fp': 0, 'fn': 0},
    'AMBIGUOUS': {'tp': 0, 'fp': 0, 'fn': 0},  # Expect lower accuracy
    'COMORBID': {'tp': 0, 'fp': 0, 'fn': 0},
}

# Per-disorder accuracy
by_disorder = {
    'mdd': {'precision': 0.0, 'recall': 0.0, 'f1': 0.0},
    'gad': {...},
    # etc.
}
```

## Configuration

### Model

Uses `gpt-5-mini` for vignette generation. No temperature parameter (model default).

### Retry Behaviour

- Max retries: 2
- Retry delay: 0.5s
- Retry triggers: Verification failures (not API errors)

### Rate Limiting

- Inter-vignette delay: 0.3s
- Adjust for API rate limits if needed

## Lessons Learned

### From Clinical Expert Review

1. **Explicit contradictions vs legitimate ambiguity**: "Preserved interest" directly contradicts anhedonia criterion, unlike "denies being depressed" which is legitimate clinical ambiguity.

2. **Pertinent negatives matter**: Silent omission of symptoms is not the same as explicit denial. AI systems cannot distinguish "not assessed" from "assessed and absent".

3. **Exclusion evidence is essential**: Ground truth marking exclusions as "cleared" is meaningless if the vignette contains no labs, no substance history, no medication list.

4. **Onset requirements vary by disorder**: ADHD requires childhood onset - vignettes must state this explicitly or AI cannot verify the criterion.

5. **MSE depth varies by presentation**: Psychotic disorders need detailed thought content/perceptions. Mood disorders need detailed affect description. Cognitive disorders need formal testing results.

6. **Functional impairment is diagnostic**: DSM Criterion B (clinical significance) requires documented impairment in occupational, social, or other functioning.

### Architecture Decisions

1. **Ground truth before text**: Generating answers first ensures diagnostic validity independent of LLM behaviour.

2. **Verification with retry**: LLMs drift and hallucinate. Verification catches common failures and retries improve yield.

3. **No symptom truncation**: Full DSM symptom descriptions provide context the LLM needs for accurate portrayal.

4. **Dynamic prompt sections**: Different disorders need different evidence (onset, MSE depth, exclusions).
