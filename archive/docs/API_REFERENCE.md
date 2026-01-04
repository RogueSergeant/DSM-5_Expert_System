# API Reference

This document describes the Prolog predicates that define the diagnostic knowledge base and inference engine.

## Core Predicates

These predicates define the knowledge base structure for each disorder.

### disorder/3

Defines basic disorder metadata.

**Signature:**
```prolog
disorder(DisorderID, FullName, Category).
```

**Parameters:**
- `DisorderID` - Unique atom identifier (e.g., `mdd`, `gad`, `adhd`)
- `FullName` - Human-readable disorder name (string)
- `Category` - DSM-5 disorder category (atom)

**Example:**
```prolog
disorder(mdd, 'Major Depressive Disorder', depressive_disorders).
disorder(gad, 'Generalised Anxiety Disorder', anxiety_disorders).
disorder(adhd, 'Attention-Deficit/Hyperactivity Disorder', neurodevelopmental_disorders).
disorder(ptsd, 'Posttraumatic Stress Disorder', trauma_stressor_related).
```

**Query Examples:**
```prolog
% List all disorders
?- disorder(ID, Name, Category).

% Get MDD full name
?- disorder(mdd, Name, _).

% Find all anxiety disorders
?- disorder(ID, Name, anxiety_disorders).
```

### symptom/4

Defines individual symptoms for a disorder.

**Signature:**
```prolog
symptom(DisorderID, SymptomID, Category, Description).
```

**Parameters:**
- `DisorderID` - Disorder this symptom belongs to
- `SymptomID` - Unique symptom identifier (format: `<disorder>_<criterion>`, e.g., `mdd_a1`)
- `Category` - Symptom grouping (e.g., `core`, `somatic`, `cognitive`, `cluster_b`)
- `Description` - Human-readable symptom description (string)

**Example:**
```prolog
symptom(mdd, mdd_a1, core, 'Depressed mood most of the day, nearly every day').
symptom(mdd, mdd_a2, core, 'Markedly diminished interest or pleasure in activities').
symptom(gad, gad_a1, core, 'Excessive anxiety and worry occurring more days than not').
```

**Query Examples:**
```prolog
% List all MDD symptoms
?- symptom(mdd, SID, Cat, Desc).

% Find core symptoms for GAD
?- symptom(gad, SID, core, Desc).

% Get description for a specific symptom
?- symptom(_, mdd_a1, _, Desc).

% Count symptoms per disorder
?- findall(S, symptom(mdd, S, _, _), Symptoms), length(Symptoms, Count).
```

### symptom_category/5

Defines groupings of symptoms with count requirements.

**Signature:**
```prolog
symptom_category(DisorderID, CategoryID, SymptomList, RequiredCount, RequirementType).
```

**Parameters:**
- `DisorderID` - Disorder this category belongs to
- `CategoryID` - Unique category identifier (e.g., `criterion_a`, `depressive_symptoms`)
- `SymptomList` - List of symptom IDs (e.g., `[mdd_a1, mdd_a2, ..., mdd_a9]`)
- `RequiredCount` - Minimum/exact number of symptoms required (integer)
- `RequirementType` - Counting logic (see types below)

**Requirement Types:**
- `at_least` - Must have ≥ RequiredCount symptoms
- `exactly` - Must have == RequiredCount symptoms
- `all` - Must have all symptoms in the list
- `at_least_one_of` - Must have ≥ 1 from the list

**Example:**
```prolog
% MDD: At least 5 of 9 depressive symptoms
symptom_category(mdd, depressive_symptoms,
    [mdd_a1, mdd_a2, mdd_a3, mdd_a4, mdd_a5, mdd_a6, mdd_a7, mdd_a8, mdd_a9],
    5, at_least).

% MDD: At least one core symptom (A1 or A2) must be present
symptom_category(mdd, core_symptoms,
    [mdd_a1, mdd_a2],
    1, at_least_one_of).

% GAD: At least 3 of 6 associated symptoms
symptom_category(gad, associated_symptoms,
    [gad_c1, gad_c2, gad_c3, gad_c4, gad_c5, gad_c6],
    3, at_least).
```

**Query Examples:**
```prolog
% Get all symptom categories for MDD
?- symptom_category(mdd, CatID, Symptoms, Count, Type).

% Check if a disorder requires "all" symptoms in a category
?- symptom_category(DisorderID, CatID, Symptoms, Count, all).
```

### duration_requirement/3

Specifies minimum symptom duration for diagnosis.

**Signature:**
```prolog
duration_requirement(DisorderID, MinDuration, Unit).
```

**Parameters:**
- `DisorderID` - Disorder this requirement applies to
- `MinDuration` - Minimum duration value (integer)
- `Unit` - Time unit (`days`, `weeks`, `months`, `years`)

**Example:**
```prolog
duration_requirement(mdd, 2, weeks).
duration_requirement(gad, 6, months).
duration_requirement(adhd, 6, months).
duration_requirement(ptsd, 1, months).
```

**Query Examples:**
```prolog
% Get MDD duration requirement
?- duration_requirement(mdd, Dur, Unit).

% Find disorders requiring ≥6 months
?- duration_requirement(ID, 6, months).
```

### onset_requirement/3

Specifies constraints on when symptoms must begin.

**Signature:**
```prolog
onset_requirement(DisorderID, ConstraintType, Value).
```

**Parameters:**
- `DisorderID` - Disorder this requirement applies to
- `ConstraintType` - Type of constraint (see types below)
- `Value` - Constraint value (depends on type)

**Constraint Types:**
- `before_age` - Symptoms must appear before age `Value` (e.g., `12` for ADHD)
- `after_event` - Symptoms must follow event type `Value` (e.g., `trauma` for PTSD)
- `any` - No onset constraint (`Value` = `none`)

**Example:**
```prolog
onset_requirement(adhd, before_age, 12).
onset_requirement(ptsd, after_event, trauma).
onset_requirement(mdd, any, none).
onset_requirement(gad, any, none).
```

**Query Examples:**
```prolog
% Get ADHD onset requirement
?- onset_requirement(adhd, Type, Value).

% Find disorders requiring early onset
?- onset_requirement(ID, before_age, Age), Age < 18.

% Find disorders requiring triggering event
?- onset_requirement(ID, after_event, EventType).
```

### exclusion_criterion/4

Defines conditions that rule out the diagnosis.

**Signature:**
```prolog
exclusion_criterion(DisorderID, ExclusionID, Type, Description).
```

**Parameters:**
- `DisorderID` - Disorder this exclusion applies to
- `ExclusionID` - Unique exclusion identifier (format: `<disorder>_exc_<type>`)
- `Type` - Exclusion category (e.g., `substance`, `medical`, `other_disorder`)
- `Description` - Human-readable exclusion description (string)

**Example:**
```prolog
exclusion_criterion(mdd, mdd_exc_bipolar, other_disorder,
    'There has never been a manic episode or a hypomanic episode.').

exclusion_criterion(gad, gad_exc_substance, substance,
    'The disturbance is not attributable to the physiological effects of a substance.').

exclusion_criterion(adhd, adhd_exc_psychotic, other_disorder,
    'The symptoms do not occur exclusively during psychotic disorder.').
```

**Query Examples:**
```prolog
% List all MDD exclusions
?- exclusion_criterion(mdd, ExcID, Type, Desc).

% Find substance-related exclusions
?- exclusion_criterion(DisorderID, ExcID, substance, Desc).

% Count exclusions per disorder
?- findall(E, exclusion_criterion(gad, E, _, _), Exclusions), length(Exclusions, Count).
```

### subjective_criterion/4

Defines criteria requiring clinical judgment.

**Signature:**
```prolog
subjective_criterion(DisorderID, CriterionID, Description, AssessmentType).
```

**Parameters:**
- `DisorderID` - Disorder this criterion applies to
- `CriterionID` - Unique criterion identifier (format: `<disorder>_subj_<type>`)
- `Description` - Human-readable criterion description (string)
- `AssessmentType` - Type of subjective judgment (see types below)

**Assessment Types:**
- `clinical_significance` - Clinically significant distress or impairment
- `severity` - Severity level assessment
- `excessiveness` - Whether symptom is "excessive" or "beyond normal"
- `functional_impairment` - Impact on social/occupational functioning
- `quality` - Qualitative assessment of symptom

**Example:**
```prolog
subjective_criterion(mdd, mdd_subj_distress,
    'The symptoms cause clinically significant distress or impairment in social, occupational, or other important areas of functioning.',
    clinical_significance).

subjective_criterion(gad, gad_subj_excessive,
    'The anxiety and worry are excessive (i.e., beyond what is appropriate for the situation).',
    excessiveness).

subjective_criterion(adhd, adhd_subj_functional_impairment,
    'There is clear evidence that symptoms interfere with functioning.',
    functional_impairment).
```

**Query Examples:**
```prolog
% List all subjective criteria for GAD
?- subjective_criterion(gad, CritID, Desc, Type).

% Find all criteria requiring excessiveness assessment
?- subjective_criterion(DisorderID, CritID, Desc, excessiveness).
```

### specifier/4

Defines optional diagnosis qualifiers (severity, features, etc.).

**Signature:**
```prolog
specifier(DisorderID, SpecifierType, Options, Description).
```

**Parameters:**
- `DisorderID` - Disorder this specifier applies to
- `SpecifierType` - Type of specifier (e.g., `severity`, `onset`, `features`)
- `Options` - List of possible values (e.g., `[mild, moderate, severe]`)
- `Description` - Human-readable description (string)

**Example:**
```prolog
specifier(mdd, severity, [mild, moderate, severe], 'Current severity level.').
specifier(mdd, psychotic_features, [absent, present], 'Presence of psychotic features.').
specifier(adhd, presentation, [inattentive, hyperactive, combined], 'Current presentation type.').
```

## Diagnostic Inference Predicates

These predicates perform diagnostic reasoning using patient facts.

### meets_symptom_criteria/2

Checks if symptom count requirements are satisfied.

**Signature:**
```prolog
meets_symptom_criteria(PatientID, DisorderID).
```

**Succeeds when:** Patient has sufficient symptoms from all required categories.

**Example:**
```prolog
% Check if patient pt001 meets MDD symptom criteria
?- meets_symptom_criteria(pt001, mdd).
```

### meets_duration_criteria/2

Checks if duration requirement is satisfied.

**Signature:**
```prolog
meets_duration_criteria(PatientID, DisorderID).
```

**Succeeds when:** `patient_duration(PatientID, DisorderID, Days)` ≥ required duration.

**Example:**
```prolog
% Check if patient has had symptoms long enough for GAD
?- meets_duration_criteria(pt001, gad).
```

### meets_onset_criteria/2

Checks if onset requirements are satisfied.

**Signature:**
```prolog
meets_onset_criteria(PatientID, DisorderID).
```

**Succeeds when:**
- `before_age`: `patient_onset_age(PatientID, Age)` < required age
- `after_event`: `patient_context(PatientID, EventType, present)`
- `any`: Always succeeds

**Example:**
```prolog
% Check if ADHD onset before age 12
?- meets_onset_criteria(pt001, adhd).
```

### meets_exclusion_criteria/2

Checks if all exclusions are cleared (none apply).

**Signature:**
```prolog
meets_exclusion_criteria(PatientID, DisorderID).
```

**Succeeds when:** All exclusions for the disorder have status `cleared` (not `excluded`).

**Example:**
```prolog
% Check if MDD exclusions are cleared
?- meets_exclusion_criteria(pt001, mdd).
```

### diagnosis_candidate/3

**Main diagnostic predicate** - combines all criteria checks.

**Signature:**
```prolog
diagnosis_candidate(PatientID, DisorderID, Confidence).
```

**Parameters:**
- `PatientID` - Patient identifier
- `DisorderID` - Disorder being evaluated
- `Confidence` - Diagnostic confidence score (0.0-1.0)

**Succeeds when:** Patient meets all of:
- Symptom criteria
- Duration criteria
- Onset criteria
- Exclusion criteria
- Subjective criteria

**Example:**
```prolog
% Get all diagnosis candidates for patient with confidence
?- diagnosis_candidate(pt001, DisorderID, Confidence).

% Check if MDD is a candidate
?- diagnosis_candidate(pt001, mdd, Conf).
```

### full_diagnosis/3

Extended diagnostic with detailed status tracking.

**Signature:**
```prolog
full_diagnosis(PatientID, DisorderID, Result).
```

**Parameters:**
- `PatientID` - Patient identifier
- `DisorderID` - Disorder being evaluated
- `Result` - Complex term with diagnostic details:
  ```prolog
  diagnosis(
      disorder: DisorderID,
      status: met | not_met | missing_data,
      confidence: Float,
      criteria_status: [
          symptoms: met | not_met | missing_data,
          duration: met | not_met | missing_data,
          onset: met | not_met | missing_data,
          exclusions: met | not_met | missing_data,
          subjective: met | not_met | missing_data
      ],
      missing_data: [list of missing information]
  )
  ```

**Example:**
```prolog
% Get full diagnostic report for MDD
?- full_diagnosis(pt001, mdd, Result).
```

### criterion_check/5

Check status of individual diagnostic criterion.

**Signature:**
```prolog
criterion_check(PatientID, DisorderID, CriterionType, Status, Details).
```

**Parameters:**
- `PatientID` - Patient identifier
- `DisorderID` - Disorder being evaluated
- `CriterionType` - Type of criterion (`symptoms`, `duration`, `onset`, `exclusions`, `subjective`)
- `Status` - Criterion status (`met`, `not_met`, `missing_data`)
- `Details` - Additional information (e.g., symptom counts, missing assessments)

**Example:**
```prolog
% Check MDD symptom criterion status
?- criterion_check(pt001, mdd, symptoms, Status, Details).

% Check all criteria for GAD
?- criterion_check(pt001, gad, CritType, Status, Details).
```

### collect_missing_data/3

Identify missing information needed for diagnosis.

**Signature:**
```prolog
collect_missing_data(PatientID, DisorderID, MissingList).
```

**Returns:** List of missing patient facts (symptoms, duration, onset, exclusions, subjective).

**Example:**
```prolog
% Get list of missing data for ADHD diagnosis
?- collect_missing_data(pt001, adhd, Missing).
```

## Patient Fact Predicates

These predicates assert patient-specific data into the knowledge base.

### patient_symptom/4

Records symptom status for a patient.

**Signature:**
```prolog
patient_symptom(PatientID, SymptomID, Status, Evidence).
```

**Parameters:**
- `PatientID` - Patient identifier
- `SymptomID` - Symptom identifier (from `symptom/4`)
- `Status` - Symptom status (`present`, `absent`, `unknown`)
- `Evidence` - Supporting evidence (string quote from clinical notes)

**Example:**
```prolog
% Assert patient has depressed mood
assertz(patient_symptom(pt001, mdd_a1, present, 'Patient reports feeling sad most days')).

% Query patient symptoms
?- patient_symptom(pt001, SymptomID, Status, Evidence).
```

### patient_duration/3

Records symptom duration for a disorder.

**Signature:**
```prolog
patient_duration(PatientID, DisorderID, Days).
```

**Parameters:**
- `PatientID` - Patient identifier
- `DisorderID` - Disorder identifier
- `Days` - Duration in days (integer)

**Example:**
```prolog
% Assert symptoms have persisted for 21 days (3 weeks)
assertz(patient_duration(pt001, mdd, 21)).
```

### patient_onset_age/2

Records age at symptom onset.

**Signature:**
```prolog
patient_onset_age(PatientID, Age).
```

**Parameters:**
- `PatientID` - Patient identifier
- `Age` - Age at onset (integer)

**Example:**
```prolog
% Assert ADHD symptoms appeared at age 8
assertz(patient_onset_age(pt001, 8)).
```

### patient_exclusion_status/3

Records exclusion criterion status.

**Signature:**
```prolog
patient_exclusion_status(PatientID, ExclusionID, Status).
```

**Parameters:**
- `PatientID` - Patient identifier
- `ExclusionID` - Exclusion identifier (from `exclusion_criterion/4`)
- `Status` - Exclusion status (`cleared`, `excluded`, `unknown`)

**Example:**
```prolog
% Assert substance exclusion is cleared
assertz(patient_exclusion_status(pt001, mdd_exc_substance, cleared)).

% Assert bipolar exclusion applies (manic episode present)
assertz(patient_exclusion_status(pt001, mdd_exc_bipolar, excluded)).
```

### subjective_assessment/4

Records LLM or clinician subjective assessment.

**Signature:**
```prolog
subjective_assessment(PatientID, CriterionID, Assessment, Confidence).
```

**Parameters:**
- `PatientID` - Patient identifier
- `CriterionID` - Subjective criterion identifier (from `subjective_criterion/4`)
- `Assessment` - Assessment result (`met`, `not_met`, `unclear`)
- `Confidence` - Confidence score (0.0-1.0)

**Example:**
```prolog
% Assert clinical significance criterion is met with high confidence
assertz(subjective_assessment(pt001, mdd_subj_distress, met, 0.95)).
```

### patient_context/3

Records patient context information (age, gender, event history).

**Signature:**
```prolog
patient_context(PatientID, ContextType, Value).
```

**Parameters:**
- `PatientID` - Patient identifier
- `ContextType` - Context type (`age`, `gender`, `trauma`, etc.)
- `Value` - Context value (type depends on ContextType)

**Example:**
```prolog
% Assert patient is 29 years old
assertz(patient_context(pt001, age, 29)).

% Assert patient has trauma history
assertz(patient_context(pt001, trauma, present)).
```

## Complete Example

Full diagnostic workflow for MDD:

```prolog
% 1. Load knowledge base
?- consult('schema.pl').
?- consult('gold_standard/loader.pl').

% 2. Assert patient facts
?- assertz(patient_symptom(pt001, mdd_a1, present, 'Reports sadness daily')).
?- assertz(patient_symptom(pt001, mdd_a2, present, 'Lost interest in hobbies')).
?- assertz(patient_symptom(pt001, mdd_a4, present, 'Insomnia reported')).
?- assertz(patient_symptom(pt001, mdd_a6, present, 'Fatigue daily')).
?- assertz(patient_symptom(pt001, mdd_a7, present, 'Feelings of worthlessness')).
?- assertz(patient_duration(pt001, mdd, 21)).
?- assertz(patient_exclusion_status(pt001, mdd_exc_bipolar, cleared)).
?- assertz(patient_exclusion_status(pt001, mdd_exc_substance, cleared)).
?- assertz(patient_exclusion_status(pt001, mdd_exc_medical, cleared)).
?- assertz(patient_exclusion_status(pt001, mdd_exc_psychotic, cleared)).
?- assertz(subjective_assessment(pt001, mdd_subj_distress, met, 0.95)).

% 3. Check individual criteria
?- meets_symptom_criteria(pt001, mdd).
% true.

?- meets_duration_criteria(pt001, mdd).
% true.

?- meets_exclusion_criteria(pt001, mdd).
% true.

% 4. Get diagnosis
?- diagnosis_candidate(pt001, mdd, Confidence).
% Confidence = 0.92.

% 5. Get full report
?- full_diagnosis(pt001, mdd, Result).
% Result = diagnosis(mdd, met, 0.92, [...], []).

% 6. Clear patient data
?- retractall(patient_symptom(pt001, _, _, _)).
?- retractall(patient_duration(pt001, _, _)).
```

## Related Documentation

- [Architecture](ARCHITECTURE.md) - System design and three-tier reasoning
- [Getting Started](GETTING_STARTED.md) - Setup and basic usage
- [Gold Standard README](../src/prolog/gold_standard/README.md) - Template guide for creating disorder files
