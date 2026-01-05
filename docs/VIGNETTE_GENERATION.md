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
  "id": "vig_000_mdd_moderate_a1b2c3d4",
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

The `verify_vignette()` function uses LLM-based validation (gpt-5-nano) to check:

| Check | Failure Criteria | Retry? |
|-------|------------------|--------|
| Diagnostic labels | Contains forbidden terms for disorder | Yes |
| Duration | No timeline/duration stated | Yes |
| Pertinent negatives | < 2 denial patterns | Yes |
| Onset age | Required but not stated (ADHD/ASD) | Yes |
| Word count | < 50 words (hard check before LLM) | Yes |
| Medical context | Labs/substances/meds not mentioned | Yes |

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

### Models

| Purpose | Model | Token Limit | Notes |
|---------|-------|-------------|-------|
| Vignette generation | `gpt-5-mini` | 10,000 | High limit for internal reasoning |
| Validation | `gpt-5-nano` | 5,000 | LLM-based validation, replaces regex |

**Why high token limits?**

GPT-5 models use internal reasoning that consumes tokens before generating visible output. With insufficient limits, the model exhausts tokens during reasoning and returns empty content. Symptoms: `finish_reason: length` with `content: ''`.

### Validation

Verification uses `gpt-5-nano` instead of regex patterns. The LLM checks:
- No diagnostic labels present
- Timeline/duration stated
- Pertinent negatives included (≥2)
- Medical context mentioned
- Onset age stated (for ADHD/ASD)

Response format:
```json
{"status": "accepted", "reason": ""}
{"status": "rejected", "reason": "No clear timeline found"}
```

### Logging

Logs saved to `logs/vignettes/{timestamp}.log` with:
- Vignette index, disorder, difficulty
- Full API response metadata (ID, model, finish_reason, token usage)
- Content length and preview
- Validation results

Log path printed at startup for easy access.

### Vignette IDs

Format: `vig_{idx}_{disorder}_{difficulty}_{uuid8}`

Example: `vig_000_mdd_moderate_a1b2c3d4`

**Why UUID suffix?**

IDs must be unique across all vignette files, not just within a single run. Without a unique suffix, running generation twice would create duplicate IDs (`vig_000_mdd_moderate` in both files).

We use 8 hex characters from UUID4 (32 bits of randomness):

| Vignettes | Collision probability |
|-----------|----------------------|
| 1,000 | 0.0001% |
| 10,000 | 0.01% |
| 77,000 | 50% (birthday paradox threshold) |

For typical usage (hundreds to low thousands of vignettes), collision risk is negligible.

**Alternatives considered:**
- Full UUID (32 chars): Overkill, reduces readability
- Timestamp prefix: Redundant with filename, still not guaranteed unique within same second
- Sequential counter across files: Requires scanning existing files, complex

### Forbidden Labels

Diagnostic labels are dynamically generated based on the disorder being created:

```python
label_map = {
    'mdd': ['major depressive', 'mdd', 'major depression', 'clinical depression'],
    'gad': ['generalized anxiety', 'gad', 'anxiety disorder'],
    'adhd': ['adhd', 'add', 'attention deficit', 'attention-deficit'],
    'ptsd': ['ptsd', 'post-traumatic', 'posttraumatic'],
    'asd': ['autism spectrum', 'asd', 'autism', 'asperger'],
}
```

This ensures that when generating an MDD vignette, we check for MDD-specific labels, and when generating ADHD, we check for ADHD-specific labels. For comorbid cases, labels from both disorders are combined.

### Retry Behaviour

- Max retries: 2
- Retry delay: 0.5s
- Retry triggers: Verification failures (not API errors)

### Progress Tracking

Uses `tqdm` for progress bar with:
- Completion percentage and ETA
- Current disorder and difficulty in postfix
- Warnings printed via `tqdm.write()` to avoid breaking progress bar

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

5. **UUID suffix for IDs**: Ensures uniqueness across files without complex state management. 8 hex chars balance readability with collision resistance.

6. **Dynamic forbidden labels**: Each disorder has its own set of diagnostic terms to block. Static lists miss disorder-specific terminology and include irrelevant checks.

7. **tqdm for progress**: Long-running generation (50 vignettes × 20s each = 15+ minutes) benefits from progress visibility. Warnings use `tqdm.write()` to preserve progress bar.

8. **High token limits for reasoning models**: GPT-5 models consume tokens for internal reasoning before producing output. Low limits cause empty responses with `finish_reason: length`. Use 10,000+ for generation, 5,000+ for validation.

9. **LLM-based validation**: Regex patterns are brittle and miss valid variations. Using gpt-5-nano for validation is more robust and handles natural language flexibility.

10. **Verbose logging**: API debugging requires full response metadata. Log finish_reason, token usage, and content previews to diagnose issues like empty responses.
