# Gold Standard Disorder Files

This guide explains how to create new disorder definition files for the diagnostic system.

## Quick Start

1. Create a new file: `gold_standard/<disorder_id>.pl`
2. Add the disorder ID to `loader.pl`
3. Define the required predicates (see template below)

## File Structure

Each disorder file must declare multifile predicates and define facts for that disorder.

### Required Multifile Declarations

```prolog
%% Required declarations (copy these exactly)
:- multifile disorder/3.
:- multifile symptom/4.
:- multifile symptom_category/5.
:- multifile duration_requirement/3.
:- multifile onset_requirement/3.
:- multifile exclusion_criterion/4.
:- multifile subjective_criterion/4.
:- multifile specifier/4.
:- multifile differential_feature/4.

%% Optional (only if your disorder uses these)
:- multifile age_adjusted_count/4.
:- multifile setting_requirement/2.
```

## Predicate Reference

### disorder/3 (Required)
```prolog
disorder(DisorderID, FullName, Category).

%% Example:
disorder(gad, 'Generalised Anxiety Disorder', anxiety_disorders).
```

### symptom/4 (Required)
```prolog
symptom(DisorderID, SymptomID, Category, Description).

%% Example:
symptom(gad, gad_a1, core, 'Excessive anxiety and worry occurring more days than not').
```
- Use consistent `SymptomID` format: `<disorder>_<criterion>` (e.g., `mdd_a1`, `gad_b3`)
- Category groups related symptoms (e.g., `core`, `somatic`, `cognitive`, `cluster_b`)

### symptom_category/5 (Required)
```prolog
symptom_category(DisorderID, CategoryID, SymptomList, RequiredCount, RequirementType).

%% RequirementType options:
%%   at_least     - Must have >= RequiredCount
%%   exactly      - Must have == RequiredCount
%%   all          - Must have all symptoms in list
%%   at_least_one_of - Must have >= 1 from list

%% Example:
symptom_category(gad, associated_symptoms,
    [gad_b1, gad_b2, gad_b3, gad_b4, gad_b5, gad_b6],
    3, at_least).
```

### duration_requirement/3
```prolog
duration_requirement(DisorderID, MinDuration, Unit).

%% Unit options: days, weeks, months, years

%% Example:
duration_requirement(gad, 6, months).
```

### onset_requirement/3
```prolog
onset_requirement(DisorderID, ConstraintType, Value).

%% ConstraintType options:
%%   before_age  - Value = maximum onset age (e.g., 12 for ADHD)
%%   after_event - Value = event type atom (e.g., trauma for PTSD)
%%   any         - Value = none (no onset constraint)

%% Examples:
onset_requirement(adhd, before_age, 12).
onset_requirement(ptsd, after_event, trauma).
onset_requirement(mdd, any, none).
```

### exclusion_criterion/4
```prolog
exclusion_criterion(DisorderID, ExclusionID, Type, Description).

%% Type options: substance, medical, other_disorder, developmental

%% Example:
exclusion_criterion(gad, gad_exc_substance, substance,
    'The disturbance is not attributable to the physiological effects of a substance.').
```

### subjective_criterion/4
```prolog
subjective_criterion(DisorderID, CriterionID, Description, AssessmentType).

%% AssessmentType options:
%%   clinical_significance, severity, excessiveness, functional_impairment, quality

%% Example:
subjective_criterion(gad, gad_subj_distress,
    'The anxiety, worry, or physical symptoms cause clinically significant distress.',
    clinical_significance).
```

### specifier/4 (Optional)
```prolog
specifier(DisorderID, SpecifierType, Options, Description).

%% Example:
specifier(gad, severity, [mild, moderate, severe], 'Current severity level.').
```

### differential_feature/4 (Optional)
```prolog
differential_feature(DisorderID, OtherDisorderID, FeatureID, Description).

%% Example:
differential_feature(gad, mdd, gad_diff_mdd,
    'In GAD, worry is primary; in MDD, depressed mood is primary.').
```

## Optional Disorder-Specific Predicates

### age_adjusted_count/4
For disorders with different thresholds by age (e.g., ADHD):

```prolog
age_adjusted_count(DisorderID, CategoryID, AgeThreshold, AdjustedCount).

%% Example: ADHD requires 6 symptoms for children, 5 for adults 17+
age_adjusted_count(adhd, inattention_symptoms, 17, 5).
age_adjusted_count(adhd, hyperactivity_symptoms, 17, 5).
```

### setting_requirement/2
For disorders requiring symptoms across multiple settings:

```prolog
setting_requirement(DisorderID, MinSettings).

%% Example: ADHD requires symptoms in 2+ settings
setting_requirement(adhd, 2).
```

## Registering Your Disorder

Add the disorder filename (without `.pl`) to `loader.pl`:

```prolog
gold_standard_disorder_files([
    'mdd',
    'gad',      % <-- Add here
    'adhd'
]).
```

## Minimal Template

```prolog
%% =============================================================================
%% <DISORDER NAME> - Gold Standard
%% =============================================================================
%% DSM-5 Reference: pp. XXX-XXX
%% Key features: <brief description>
%% =============================================================================

:- multifile disorder/3.
:- multifile symptom/4.
:- multifile symptom_category/5.
:- multifile duration_requirement/3.
:- multifile onset_requirement/3.
:- multifile exclusion_criterion/4.
:- multifile subjective_criterion/4.
:- multifile specifier/4.
:- multifile differential_feature/4.
:- multifile age_adjusted_count/4.
:- multifile setting_requirement/2.

%% Disorder Definition
disorder(<id>, '<Full Name>', <category>).

%% Symptoms
symptom(<id>, <id>_a1, core, '<description>').

%% Symptom Categories
symptom_category(<id>, all_symptoms, [<id>_a1], 1, at_least).

%% Duration
duration_requirement(<id>, 2, weeks).

%% Onset
onset_requirement(<id>, any, none).

%% Exclusions
exclusion_criterion(<id>, <id>_exc_1, substance, '<description>').

%% Subjective Criteria
subjective_criterion(<id>, <id>_subj_1, '<description>', clinical_significance).
```

## Testing Your Disorder

```prolog
?- [schema].
?- ['gold_standard/loader'].
?- disorder(your_id, Name, Cat).              % Verify loaded
?- symptom(your_id, S, Cat, Desc).            % List symptoms
?- validate_disorder(your_id, Issues).        % Check for issues
```
