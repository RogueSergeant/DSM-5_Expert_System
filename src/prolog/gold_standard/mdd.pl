%% =============================================================================
%% MAJOR DEPRESSIVE DISORDER (MDD) - Gold Standard
%% =============================================================================
%% DSM-5 Reference: pp. 160-168
%% Key features: Count-based (5/9 symptoms), 2-week duration, core symptom required
%%
%% Hand-coded diagnostic criteria for validation of LLM extraction.
%% =============================================================================

%% Declare predicates as multifile to extend the schema definitions
:- multifile disorder/3.
:- multifile symptom/4.
:- multifile symptom_category/5.
:- multifile duration_requirement/3.
:- multifile onset_requirement/3.
:- multifile exclusion_criterion/4.
:- multifile subjective_criterion/4.
:- multifile specifier/4.
:- multifile differential_feature/4.

%% Optional disorder-specific predicates (MDD doesn't use these, but declare for consistency)
:- multifile age_adjusted_count/4.
:- multifile setting_requirement/2.

%% -----------------------------------------------------------------------------
%% Disorder Definition
%% -----------------------------------------------------------------------------
disorder(mdd, 'Major Depressive Disorder', depressive_disorders).

%% -----------------------------------------------------------------------------
%% Symptoms - Criterion A
%% -----------------------------------------------------------------------------
%% Structure: symptom(DisorderID, SymptomID, Category, Description)
%%
%% Categories for MDD:
%%   - core: Must have at least 1 (A1 or A2)
%%   - somatic: Physical/vegetative symptoms (A3-A6)
%%   - cognitive: Psychological symptoms (A7-A9)
%% -----------------------------------------------------------------------------

% Core symptoms (at least ONE required)
symptom(mdd, mdd_a1, core,
    'Depressed mood most of the day, nearly every day, as indicated by subjective report (e.g., feels sad, empty, hopeless) or observation by others (e.g., appears tearful). Note: In children and adolescents, can be irritable mood.').

symptom(mdd, mdd_a2, core,
    'Markedly diminished interest or pleasure in all, or almost all, activities most of the day, nearly every day (as indicated by subjective account or observation).').

% Somatic symptoms
symptom(mdd, mdd_a3, somatic,
    'Significant weight loss when not dieting or weight gain (e.g., change of more than 5% of body weight in a month), or decrease or increase in appetite nearly every day. Note: In children, consider failure to make expected weight gain.').

symptom(mdd, mdd_a4, somatic,
    'Insomnia or hypersomnia nearly every day.').

symptom(mdd, mdd_a5, somatic,
    'Psychomotor agitation or retardation nearly every day (observable by others, not merely subjective feelings of restlessness or being slowed down).').

symptom(mdd, mdd_a6, somatic,
    'Fatigue or loss of energy nearly every day.').

% Cognitive symptoms
symptom(mdd, mdd_a7, cognitive,
    'Feelings of worthlessness or excessive or inappropriate guilt (which may be delusional) nearly every day (not merely self-reproach or guilt about being sick).').

symptom(mdd, mdd_a8, cognitive,
    'Diminished ability to think or concentrate, or indecisiveness, nearly every day (either by subjective account or as observed by others).').

symptom(mdd, mdd_a9, cognitive,
    'Recurrent thoughts of death (not just fear of dying), recurrent suicidal ideation without a specific plan, or a suicide attempt or a specific plan for committing suicide.').

%% -----------------------------------------------------------------------------
%% Symptom Category Requirements
%% -----------------------------------------------------------------------------
%% Structure: symptom_category(DisorderID, CategoryID, SymptomList, RequiredCount, RequirementType)
%%
%% MDD requires:
%%   - At least 1 core symptom (A1 or A2)
%%   - At least 5 total symptoms from all 9
%% -----------------------------------------------------------------------------

% Must have at least 1 core symptom (Criterion A specifies this)
symptom_category(mdd, core_symptoms,
    [mdd_a1, mdd_a2],
    1, at_least).

% Must have at least 5 total symptoms
symptom_category(mdd, all_symptoms,
    [mdd_a1, mdd_a2, mdd_a3, mdd_a4, mdd_a5, mdd_a6, mdd_a7, mdd_a8, mdd_a9],
    5, at_least).

%% -----------------------------------------------------------------------------
%% Duration Requirement
%% -----------------------------------------------------------------------------
%% DSM-5 Criterion A: "present during the same 2-week period"
%% -----------------------------------------------------------------------------

duration_requirement(mdd, 2, weeks).

%% -----------------------------------------------------------------------------
%% Onset Requirement
%% -----------------------------------------------------------------------------
%% MDD has no specific onset age requirement
%% -----------------------------------------------------------------------------

onset_requirement(mdd, any, none).

%% -----------------------------------------------------------------------------
%% Exclusion Criteria
%% -----------------------------------------------------------------------------
%% Structure: exclusion_criterion(DisorderID, ExclusionID, Type, Description)
%%
%% Types: substance, medical, other_disorder
%% These represent conditions that must NOT be present for diagnosis
%% -----------------------------------------------------------------------------

% Criterion C: Not attributable to substance or medical condition
exclusion_criterion(mdd, mdd_exc_substance, substance,
    'The episode is not attributable to the physiological effects of a substance (e.g., drug of abuse, medication).').

exclusion_criterion(mdd, mdd_exc_medical, medical,
    'The episode is not attributable to another medical condition.').

% Criterion D: Not better explained by psychotic disorders
exclusion_criterion(mdd, mdd_exc_psychotic, other_disorder,
    'The occurrence of the major depressive episode is not better explained by schizoaffective disorder, schizophrenia, schizophreniform disorder, delusional disorder, or other schizophrenia spectrum and psychotic disorders.').

% Criterion E: No manic/hypomanic episodes (rules out bipolar)
exclusion_criterion(mdd, mdd_exc_bipolar, other_disorder,
    'There has never been a manic episode or a hypomanic episode. Note: This exclusion does not apply if all manic-like or hypomanic-like episodes are substance-induced or attributable to another medical condition.').

%% -----------------------------------------------------------------------------
%% Subjective Criteria
%% -----------------------------------------------------------------------------
%% Structure: subjective_criterion(DisorderID, CriterionID, Description, AssessmentType)
%%
%% These require clinical judgment rather than objective counting
%% AssessmentType: clinical_significance, severity, excessiveness, functional_impairment
%% -----------------------------------------------------------------------------

% Criterion B: Clinical significance requirement
subjective_criterion(mdd, mdd_subj_clinical_significance,
    'The symptoms cause clinically significant distress or impairment in social, occupational, or other important areas of functioning.',
    clinical_significance).

%% -----------------------------------------------------------------------------
%% Specifiers
%% -----------------------------------------------------------------------------
%% Structure: specifier(DisorderID, SpecifierType, Options, Description)
%% -----------------------------------------------------------------------------

specifier(mdd, severity,
    [mild, moderate, severe],
    'Current severity based on number of criterion symptoms, severity of symptoms, and degree of functional disability.').

specifier(mdd, psychotic_features,
    [absent, mood_congruent, mood_incongruent],
    'Presence and type of psychotic features (delusions or hallucinations).').

specifier(mdd, remission_status,
    [in_partial_remission, in_full_remission],
    'Remission status when full criteria are not currently met.').

specifier(mdd, episode_type,
    [single_episode, recurrent],
    'Whether this is a single episode or recurrent (interval of at least 2 months between episodes).').

specifier(mdd, features,
    [with_anxious_distress, with_mixed_features, with_melancholic_features,
     with_atypical_features, with_catatonia, with_peripartum_onset, with_seasonal_pattern],
    'Additional feature specifiers that may apply to the current episode.').

%% -----------------------------------------------------------------------------
%% Differential Diagnosis Features
%% -----------------------------------------------------------------------------
%% Structure: differential_feature(DisorderID, OtherDisorderID, FeatureID, Description)
%% Documents what distinguishes this disorder from similar ones
%% -----------------------------------------------------------------------------

differential_feature(mdd, bipolar_disorder, mdd_diff_bipolar,
    'MDD is distinguished from bipolar disorder by absence of any history of manic or hypomanic episodes.').

differential_feature(mdd, gad, mdd_diff_gad,
    'In MDD, depressed mood and loss of interest are primary; in GAD, anxiety and worry are primary. Both may co-occur.').

differential_feature(mdd, adjustment_disorder, mdd_diff_adjustment,
    'Adjustment disorder with depressed mood does not meet full criteria for MDD (fewer than 5 symptoms or shorter duration).').

differential_feature(mdd, bereavement, mdd_diff_bereavement,
    'In grief, predominant affect is emptiness/loss with waves tied to reminders of deceased; in MDD, persistent depressed mood not tied to specific thoughts.').
