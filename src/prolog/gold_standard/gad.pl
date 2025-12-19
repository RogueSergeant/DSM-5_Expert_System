%% =============================================================================
%% GENERALIZED ANXIETY DISORDER (GAD) - Gold Standard
%% =============================================================================
%% DSM-5 Reference: 300.02 (F41.1)
%% Key features: Excessive worry for 6+ months, 3/6 associated symptoms (1 for children),
%%               difficulty controlling worry
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

%% Optional disorder-specific predicates
:- multifile age_adjusted_count/4.
:- multifile setting_requirement/2.

%% -----------------------------------------------------------------------------
%% Disorder Definition
%% -----------------------------------------------------------------------------
disorder(gad, 'Generalized Anxiety Disorder', anxiety_disorders).

%% -----------------------------------------------------------------------------
%% Symptoms - Criteria A, B, and C
%% -----------------------------------------------------------------------------
%% Structure: symptom(DisorderID, SymptomID, Category, Description)
%%
%% Categories for GAD:
%%   - core: Criterion A (excessive worry) and B (difficulty controlling)
%%   - associated: Criterion C symptoms (C1-C6)
%% -----------------------------------------------------------------------------

% Core symptoms - Criterion A & B (BOTH required)
symptom(gad, gad_a, core,
    'Excessive anxiety and worry (apprehensive expectation), occurring more days than not for at least 6 months, about a number of events or activities (such as work or school performance).').

symptom(gad, gad_b, core,
    'The individual finds it difficult to control the worry.').

% Associated symptoms - Criterion C (3+ required for adults, 1+ for children)
symptom(gad, gad_c1, associated,
    'Restlessness or feeling keyed up or on edge.').

symptom(gad, gad_c2, associated,
    'Being easily fatigued.').

symptom(gad, gad_c3, associated,
    'Difficulty concentrating or mind going blank.').

symptom(gad, gad_c4, associated,
    'Irritability.').

symptom(gad, gad_c5, associated,
    'Muscle tension.').

symptom(gad, gad_c6, associated,
    'Sleep disturbance (difficulty falling or staying asleep, or restless, unsatisfying sleep).').

%% -----------------------------------------------------------------------------
%% Symptom Category Requirements
%% -----------------------------------------------------------------------------
%% Structure: symptom_category(DisorderID, CategoryID, SymptomList, RequiredCount, RequirementType)
%%
%% GAD requires:
%%   - Both core symptoms (A and B)
%%   - At least 3 associated symptoms for adults (age-adjusted to 1 for children)
%% -----------------------------------------------------------------------------

% Must have BOTH core symptoms (Criterion A and B)
symptom_category(gad, core_symptoms,
    [gad_a, gad_b],
    2, exactly).

% Must have at least 3 associated symptoms for adults (Criterion C)
% Note: Children only need 1 - base count is 1, age_adjusted raises to 3 for adults
symptom_category(gad, associated_symptoms,
    [gad_c1, gad_c2, gad_c3, gad_c4, gad_c5, gad_c6],
    1, at_least).

%% -----------------------------------------------------------------------------
%% Age-Adjusted Requirements
%% -----------------------------------------------------------------------------
%% DSM-5 Note: "Only one item is required in children"
%% Schema logic: if age >= threshold, use adjusted count; otherwise use base count
%% Base count = 1 (children), adjusted to 3 for adults (age 18+)
%% -----------------------------------------------------------------------------

age_adjusted_count(gad, associated_symptoms, 18, 3).

%% -----------------------------------------------------------------------------
%% Duration Requirement
%% -----------------------------------------------------------------------------
%% DSM-5 Criterion A: "occurring more days than not for at least 6 months"
%% Criterion C also notes: "present for more days than not for the past 6 months"
%% -----------------------------------------------------------------------------

duration_requirement(gad, 6, months).

%% -----------------------------------------------------------------------------
%% Onset Requirement
%% -----------------------------------------------------------------------------
%% GAD has no specific onset age requirement
%% Median onset is 30 years but can occur at any age
%% -----------------------------------------------------------------------------

onset_requirement(gad, any, none).

%% -----------------------------------------------------------------------------
%% Exclusion Criteria
%% -----------------------------------------------------------------------------
%% Structure: exclusion_criterion(DisorderID, ExclusionID, Type, Description)
%%
%% Types: substance, medical, other_disorder
%% These represent conditions that must NOT be present for diagnosis
%% -----------------------------------------------------------------------------

% Criterion E: Not attributable to substance or medical condition
exclusion_criterion(gad, gad_exc_substance, substance,
    'The disturbance is not attributable to the physiological effects of a substance (e.g., a drug of abuse, a medication).').

exclusion_criterion(gad, gad_exc_medical, medical,
    'The disturbance is not attributable to another medical condition (e.g., hyperthyroidism).').

% Criterion F: Not better explained by another mental disorder
exclusion_criterion(gad, gad_exc_panic, other_disorder,
    'The disturbance is not better explained by anxiety or worry about having panic attacks in panic disorder.').

exclusion_criterion(gad, gad_exc_social_anxiety, other_disorder,
    'The disturbance is not better explained by negative evaluation in social anxiety disorder (social phobia).').

exclusion_criterion(gad, gad_exc_ocd, other_disorder,
    'The disturbance is not better explained by contamination or other obsessions in obsessive-compulsive disorder.').

exclusion_criterion(gad, gad_exc_separation, other_disorder,
    'The disturbance is not better explained by separation from attachment figures in separation anxiety disorder.').

exclusion_criterion(gad, gad_exc_ptsd, other_disorder,
    'The disturbance is not better explained by reminders of traumatic events in posttraumatic stress disorder.').

exclusion_criterion(gad, gad_exc_anorexia, other_disorder,
    'The disturbance is not better explained by gaining weight in anorexia nervosa.').

exclusion_criterion(gad, gad_exc_somatic, other_disorder,
    'The disturbance is not better explained by physical complaints in somatic symptom disorder.').

exclusion_criterion(gad, gad_exc_bdd, other_disorder,
    'The disturbance is not better explained by perceived appearance flaws in body dysmorphic disorder.').

exclusion_criterion(gad, gad_exc_illness_anxiety, other_disorder,
    'The disturbance is not better explained by having a serious illness in illness anxiety disorder.').

exclusion_criterion(gad, gad_exc_psychotic, other_disorder,
    'The disturbance is not better explained by the content of delusional beliefs in schizophrenia or delusional disorder.').

%% -----------------------------------------------------------------------------
%% Subjective Criteria
%% -----------------------------------------------------------------------------
%% Structure: subjective_criterion(DisorderID, CriterionID, Description, AssessmentType)
%%
%% These require clinical judgment rather than objective counting
%% AssessmentType: clinical_significance, severity, excessiveness, functional_impairment
%% -----------------------------------------------------------------------------

% Criterion D: Clinical significance requirement
subjective_criterion(gad, gad_subj_clinical_significance,
    'The anxiety, worry, or physical symptoms cause clinically significant distress or impairment in social, occupational, or other important areas of functioning.',
    clinical_significance).

% Additional subjective assessment: Excessiveness of worry
subjective_criterion(gad, gad_subj_excessive,
    'The intensity, duration, or frequency of the anxiety and worry is out of proportion to the actual likelihood or impact of the anticipated event.',
    excessiveness).

%% -----------------------------------------------------------------------------
%% Specifiers
%% -----------------------------------------------------------------------------
%% Structure: specifier(DisorderID, SpecifierType, Options, Description)
%% Note: GAD has fewer specifiers than MDD in DSM-5
%% -----------------------------------------------------------------------------

specifier(gad, severity,
    [mild, moderate, severe],
    'Current severity based on number and intensity of symptoms, difficulty controlling worry, and degree of functional impairment.').

%% -----------------------------------------------------------------------------
%% Differential Diagnosis Features
%% -----------------------------------------------------------------------------
%% Structure: differential_feature(DisorderID, OtherDisorderID, FeatureID, Description)
%% Documents what distinguishes this disorder from similar ones
%% -----------------------------------------------------------------------------

differential_feature(gad, mdd, gad_diff_mdd,
    'In GAD, anxiety and worry are primary; in MDD, depressed mood and loss of interest are primary. Both may co-occur.').

differential_feature(gad, panic_disorder, gad_diff_panic,
    'In GAD, worry is about multiple life circumstances; in panic disorder, anxiety is focused on having panic attacks.').

differential_feature(gad, social_anxiety_disorder, gad_diff_social,
    'In GAD, worry spans multiple domains; in social anxiety disorder, worry is specifically about social situations and evaluation by others.').

differential_feature(gad, ocd, gad_diff_ocd,
    'In GAD, worries are about real-life concerns and are excessive; in OCD, obsessions are intrusive, unwanted thoughts often unrelated to real-life problems.').

differential_feature(gad, ptsd, gad_diff_ptsd,
    'GAD is not diagnosed if anxiety and worry are better explained by symptoms of PTSD following a traumatic event.').

differential_feature(gad, adjustment_disorder, gad_diff_adjustment,
    'In adjustment disorder, anxiety occurs in response to an identifiable stressor within 3 months and does not persist more than 6 months after stressor termination.').

differential_feature(gad, illness_anxiety_disorder, gad_diff_illness,
    'In illness anxiety disorder, worry is specifically focused on having or acquiring a serious illness; in GAD, worry spans multiple domains.').
