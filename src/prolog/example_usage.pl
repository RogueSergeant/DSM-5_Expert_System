%% =============================================================================
%% DSM-5 Diagnostic System - Schema Usage Example
%% =============================================================================
%% This file demonstrates how the schema predicates are used.
%% Run with: swipl -s schema.pl -s example_usage.pl
%% =============================================================================

:- use_module(schema).

%% =============================================================================
%% EXAMPLE: Major Depressive Disorder (MDD) - Partial Definition
%% =============================================================================

%% Define the disorder
disorder(mdd, 'Major Depressive Disorder', depressive_disorders).

%% Define symptoms (DSM-5 Criterion A)
%% Core symptoms (at least one required)
symptom(mdd, mdd_a1, core, 'Depressed mood most of the day, nearly every day').
symptom(mdd, mdd_a2, core, 'Markedly diminished interest or pleasure in activities').

%% Additional symptoms
symptom(mdd, mdd_a3, somatic, 'Significant weight loss/gain or appetite change').
symptom(mdd, mdd_a4, somatic, 'Insomnia or hypersomnia nearly every day').
symptom(mdd, mdd_a5, somatic, 'Psychomotor agitation or retardation').
symptom(mdd, mdd_a6, somatic, 'Fatigue or loss of energy nearly every day').
symptom(mdd, mdd_a7, cognitive, 'Feelings of worthlessness or excessive guilt').
symptom(mdd, mdd_a8, cognitive, 'Diminished ability to think or concentrate').
symptom(mdd, mdd_a9, cognitive, 'Recurrent thoughts of death or suicidal ideation').

%% Symptom category requirements
%% Must have at least 1 core symptom
symptom_category(mdd, core_symptoms, [mdd_a1, mdd_a2], 1, at_least).

%% Must have at least 5 total symptoms
symptom_category(mdd, all_symptoms, 
    [mdd_a1, mdd_a2, mdd_a3, mdd_a4, mdd_a5, mdd_a6, mdd_a7, mdd_a8, mdd_a9], 
    5, at_least).

%% Duration requirement (Criterion A - 2 weeks)
duration_requirement(mdd, 2, weeks).

%% No specific onset requirement for MDD
onset_requirement(mdd, any, none).

%% Exclusion criteria
exclusion_criterion(mdd, mdd_exc_substance, substance,
    'Episode not attributable to physiological effects of a substance').
exclusion_criterion(mdd, mdd_exc_medical, medical,
    'Episode not attributable to another medical condition').
exclusion_criterion(mdd, mdd_exc_psychotic, other_disorder,
    'Episode not better explained by schizophrenia spectrum disorder').
exclusion_criterion(mdd, mdd_exc_bipolar, other_disorder,
    'No history of manic or hypomanic episode').

%% Subjective criteria requiring clinical judgment
subjective_criterion(mdd, mdd_subj_distress, 
    'Symptoms cause clinically significant distress or impairment in functioning',
    clinical_significance).

%% Specifiers
specifier(mdd, severity, [mild, moderate, severe], 
    'Based on symptom count and functional impairment').
specifier(mdd, psychotic_features, [absent, mood_congruent, mood_incongruent],
    'Presence and type of psychotic features').
specifier(mdd, remission, [partial, full],
    'Remission status if applicable').


%% =============================================================================
%% EXAMPLE: Test Patient Data
%% =============================================================================

%% Load test patient
load_test_patient :-
    clear_all_patient_facts,
    
    %% Patient pt001 - Classic MDD presentation
    %% Core symptoms
    assertz(patient_symptom(pt001, mdd_a1, present, 
        'Reports feeling sad and empty most days for 3 weeks')),
    assertz(patient_symptom(pt001, mdd_a2, present,
        'No longer enjoys hobbies, stopped playing guitar')),
    
    %% Additional symptoms (need 3 more for total of 5)
    assertz(patient_symptom(pt001, mdd_a4, present,
        'Sleeping only 4 hours per night, early morning awakening')),
    assertz(patient_symptom(pt001, mdd_a6, present,
        'Describes feeling exhausted, difficulty getting out of bed')),
    assertz(patient_symptom(pt001, mdd_a7, present,
        'States feeling worthless and like a burden to family')),
    
    %% Symptoms NOT present
    assertz(patient_symptom(pt001, mdd_a3, absent, 'Weight stable')),
    assertz(patient_symptom(pt001, mdd_a5, absent, 'No psychomotor changes observed')),
    assertz(patient_symptom(pt001, mdd_a8, absent, 'Concentration intact during interview')),
    assertz(patient_symptom(pt001, mdd_a9, absent, 'Denies suicidal ideation')),
    
    %% Duration - 3 weeks (21 days)
    assertz(patient_duration(pt001, mdd, 21)),
    
    %% Exclusions cleared
    assertz(patient_exclusion_status(pt001, mdd_exc_substance, cleared)),
    assertz(patient_exclusion_status(pt001, mdd_exc_medical, cleared)),
    assertz(patient_exclusion_status(pt001, mdd_exc_psychotic, cleared)),
    assertz(patient_exclusion_status(pt001, mdd_exc_bipolar, cleared)),
    
    %% Subjective assessment (from LLM or clinician)
    assertz(subjective_assessment(pt001, mdd_subj_distress, met, 0.9)),
    
    %% Context
    assertz(patient_context(pt001, age, 34)).


%% =============================================================================
%% EXAMPLE: Running Diagnosis
%% =============================================================================

%% Run diagnosis for test patient
run_example :-
    writeln('=== DSM-5 Diagnostic System Example ==='),
    writeln(''),
    
    writeln('Loading test patient pt001...'),
    load_test_patient,
    writeln('Done.'),
    writeln(''),
    
    writeln('--- Symptom Check ---'),
    (   meets_symptom_criteria(pt001, mdd)
    ->  writeln('Symptom criteria: MET')
    ;   writeln('Symptom criteria: NOT MET')
    ),
    
    %% Count symptoms
    findall(S, patient_symptom(pt001, S, present, _), PresentSyms),
    length(PresentSyms, SymCount),
    format('Present symptoms: ~w~n', [SymCount]),
    
    writeln(''),
    writeln('--- Duration Check ---'),
    (   meets_duration_criteria(pt001, mdd)
    ->  writeln('Duration criteria: MET')
    ;   writeln('Duration criteria: NOT MET')
    ),
    patient_duration(pt001, mdd, Days),
    format('Duration: ~w days (required: 14)~n', [Days]),
    
    writeln(''),
    writeln('--- Exclusion Check ---'),
    (   meets_exclusion_criteria(pt001, mdd)
    ->  writeln('Exclusion criteria: MET (no exclusions apply)')
    ;   writeln('Exclusion criteria: NOT MET (exclusion applies)')
    ),
    
    writeln(''),
    writeln('--- Subjective Check ---'),
    (   meets_subjective_criteria(pt001, mdd)
    ->  writeln('Subjective criteria: MET')
    ;   writeln('Subjective criteria: NOT MET')
    ),
    
    writeln(''),
    writeln('--- Final Diagnosis ---'),
    (   diagnosis_candidate(pt001, mdd, Confidence)
    ->  format('DIAGNOSIS: Major Depressive Disorder (confidence: ~2f)~n', [Confidence])
    ;   writeln('Diagnosis criteria NOT fully met')
    ),
    
    writeln(''),
    writeln('--- Explanation ---'),
    explain_diagnosis(pt001, mdd, Explanation),
    format('~w~n', [Explanation]),
    
    writeln(''),
    writeln('=== Example Complete ===').


%% =============================================================================
%% EXAMPLE: Negative Case (insufficient symptoms)
%% =============================================================================

load_negative_patient :-
    clear_all_patient_facts,
    
    %% Patient pt002 - Only 3 symptoms (needs 5)
    assertz(patient_symptom(pt002, mdd_a1, present, 'Feels sad sometimes')),
    assertz(patient_symptom(pt002, mdd_a4, present, 'Some sleep difficulties')),
    assertz(patient_symptom(pt002, mdd_a6, present, 'Feels tired')),
    
    %% Others absent
    assertz(patient_symptom(pt002, mdd_a2, absent, '')),
    assertz(patient_symptom(pt002, mdd_a3, absent, '')),
    assertz(patient_symptom(pt002, mdd_a5, absent, '')),
    assertz(patient_symptom(pt002, mdd_a7, absent, '')),
    assertz(patient_symptom(pt002, mdd_a8, absent, '')),
    assertz(patient_symptom(pt002, mdd_a9, absent, '')),
    
    assertz(patient_duration(pt002, mdd, 21)),
    assertz(patient_exclusion_status(pt002, mdd_exc_substance, cleared)),
    assertz(patient_exclusion_status(pt002, mdd_exc_medical, cleared)),
    assertz(patient_exclusion_status(pt002, mdd_exc_psychotic, cleared)),
    assertz(patient_exclusion_status(pt002, mdd_exc_bipolar, cleared)),
    assertz(subjective_assessment(pt002, mdd_subj_distress, met, 0.7)).

run_negative_example :-
    writeln('=== Negative Case Example ==='),
    writeln(''),
    load_negative_patient,
    
    findall(S, patient_symptom(pt002, S, present, _), PresentSyms),
    length(PresentSyms, SymCount),
    format('Present symptoms: ~w (need 5)~n', [SymCount]),
    
    (   diagnosis_candidate(pt002, mdd, _)
    ->  writeln('Result: DIAGNOSIS MET (unexpected!)')
    ;   writeln('Result: Diagnosis NOT met (correct - insufficient symptoms)')
    ),
    
    writeln('').


%% =============================================================================
%% Quick test goal
%% =============================================================================
:- initialization((
    writeln(''),
    writeln('Schema and example loaded.'),
    writeln('Run: run_example. for positive case'),
    writeln('Run: run_negative_example. for negative case'),
    writeln('')
)).
