%% =============================================================================
%% DSM-5 Diagnostic System - Core Schema
%% =============================================================================
%% This schema defines the predicate structures for representing DSM-5
%% diagnostic criteria and performing diagnostic reasoning.
%%
%% Architecture:
%%   - Knowledge Base: Static disorder definitions (populated by LLM extraction)
%%   - Patient Facts: Dynamic assertions from clinical input
%%   - Diagnostic Rules: Inference combining KB + patient facts
%%   - Explanation: Proof tree construction for transparency
%%
%% Usage:
%%   ?- [schema].
%%   ?- ['gold_standard/loader'].
%%   ?- disorder(X, Name, Category).
%%
%% Exported Predicates:
%%   Knowledge Base: disorder/3, symptom/4, symptom_category/5,
%%                   duration_requirement/3, onset_requirement/3,
%%                   exclusion_criterion/4, subjective_criterion/4,
%%                   specifier/4, differential_feature/4
%%   Patient Facts:  patient_symptom/4, patient_duration/3, patient_onset_age/2,
%%                   patient_exclusion_status/3, subjective_assessment/4,
%%                   patient_context/3
%%   Diagnostics:    meets_symptom_criteria/2, meets_duration_criteria/2,
%%                   meets_onset_criteria/2, meets_exclusion_criteria/2,
%%                   meets_subjective_criteria/2, diagnosis_candidate/3
%%   Explanation:    explain_diagnosis/3, criterion_status/4
%% =============================================================================

%% =============================================================================
%% PART 1: KNOWLEDGE BASE PREDICATES
%% =============================================================================
%% These predicates define disorder criteria. They are populated either by
%% manual gold-standard encoding or LLM-assisted extraction from DSM-5 text.
%% =============================================================================

%% disorder(+DisorderID, +FullName, +Category)
%% Defines a disorder with its identifier, full name, and DSM-5 category.
%%
%% Example:
%%   disorder(mdd, 'Major Depressive Disorder', depressive_disorders).
%%   disorder(gad, 'Generalised Anxiety Disorder', anxiety_disorders).
%%   disorder(adhd, 'Attention-Deficit/Hyperactivity Disorder', neurodevelopmental).
%%   disorder(ptsd, 'Post-Traumatic Stress Disorder', trauma_stressor_related).
%%   disorder(asd, 'Autism Spectrum Disorder', neurodevelopmental).
:- multifile disorder/3.
:- discontiguous disorder/3.

%% symptom(+DisorderID, +SymptomID, +Category, +Description)
%% Defines individual symptoms for a disorder.
%% Category groups symptoms (e.g., 'core', 'somatic', 'cognitive' for MDD;
%% 'cluster_b', 'cluster_c' for PTSD).
%%
%% Example:
%%   symptom(mdd, mdd_sym_01, core, 'Depressed mood most of the day, nearly every day').
%%   symptom(mdd, mdd_sym_02, core, 'Markedly diminished interest or pleasure').
%%   symptom(mdd, mdd_sym_03, somatic, 'Significant weight loss or gain').
:- multifile symptom/4.
:- discontiguous symptom/4.

%% symptom_category(+DisorderID, +CategoryID, +SymptomList, +RequiredCount, +RequirementType)
%% Defines symptom groupings and count requirements.
%% RequirementType: 'at_least', 'exactly', 'all', 'at_least_one_of'
%%
%% Example:
%%   symptom_category(mdd, core_symptoms, [mdd_sym_01, mdd_sym_02], 1, at_least).
%%   symptom_category(mdd, all_symptoms, [mdd_sym_01, ..., mdd_sym_09], 5, at_least).
%%   symptom_category(ptsd, cluster_b, [ptsd_b1, ptsd_b2, ...], 1, at_least).
:- multifile symptom_category/5.
:- discontiguous symptom_category/5.

%% duration_requirement(+DisorderID, +MinDuration, +Unit)
%% Specifies minimum duration for symptoms.
%% Unit: days, weeks, months, years
%%
%% Example:
%%   duration_requirement(mdd, 2, weeks).
%%   duration_requirement(gad, 6, months).
%%   duration_requirement(ptsd, 1, months).
:- multifile duration_requirement/3.
:- discontiguous duration_requirement/3.

%% onset_requirement(+DisorderID, +ConstraintType, +Value)
%% Specifies onset timing constraints.
%% ConstraintType: 'before_age', 'after_event', 'any'
%%
%% Example:
%%   onset_requirement(adhd, before_age, 12).
%%   onset_requirement(ptsd, after_event, trauma).
%%   onset_requirement(mdd, any, none).
:- multifile onset_requirement/3.
:- discontiguous onset_requirement/3.

%% exclusion_criterion(+DisorderID, +ExclusionID, +Type, +Description)
%% Defines what must NOT be present for diagnosis.
%% Type: 'substance', 'medical', 'other_disorder', 'developmental'
%%
%% Example:
%%   exclusion_criterion(mdd, mdd_exc_01, substance, 
%%       'Symptoms not attributable to substance use').
%%   exclusion_criterion(mdd, mdd_exc_02, medical,
%%       'Symptoms not attributable to another medical condition').
%%   exclusion_criterion(mdd, mdd_exc_03, other_disorder,
%%       'Never had a manic or hypomanic episode').
:- multifile exclusion_criterion/4.
:- discontiguous exclusion_criterion/4.

%% subjective_criterion(+DisorderID, +CriterionID, +Description, +AssessmentType)
%% Flags criteria requiring clinical/LLM judgment rather than objective counting.
%% AssessmentType: 'clinical_significance', 'severity', 'excessiveness', 
%%                 'functional_impairment', 'quality'
%%
%% Example:
%%   subjective_criterion(mdd, mdd_subj_01, 
%%       'Symptoms cause clinically significant distress or impairment',
%%       clinical_significance).
%%   subjective_criterion(gad, gad_subj_01,
%%       'Anxiety and worry are excessive',
%%       excessiveness).
%%   subjective_criterion(asd, asd_subj_01,
%%       'Severity level for social communication',
%%       severity).
:- multifile subjective_criterion/4.
:- discontiguous subjective_criterion/4.

%% specifier(+DisorderID, +SpecifierType, +Options, +Description)
%% Defines specifiers that qualify a diagnosis.
%% Options is a list of valid values.
%%
%% Example:
%%   specifier(mdd, severity, [mild, moderate, severe], 'Current severity').
%%   specifier(mdd, psychotic_features, [present, absent], 'With/without psychotic features').
%%   specifier(adhd, presentation, [inattentive, hyperactive_impulsive, combined],
%%       'Predominant presentation').
:- multifile specifier/4.
:- discontiguous specifier/4.

%% differential_feature(+DisorderID, +OtherDisorderID, +FeatureID, +Description)
%% Documents features that distinguish between disorders with overlapping criteria.
%%
%% Example:
%%   differential_feature(mdd, gad, diff_mdd_gad_01,
%%       'MDD: mood symptoms primary; GAD: worry/anxiety primary').
%%   differential_feature(adhd, asd, diff_adhd_asd_01,
%%       'ADHD: symptoms due to attention/impulsivity; ASD: social communication core deficit').
:- multifile differential_feature/4.
:- discontiguous differential_feature/4.

%% -----------------------------------------------------------------------------
%% OPTIONAL DISORDER-SPECIFIC PREDICATES
%% -----------------------------------------------------------------------------
%% These predicates are OPTIONAL - disorders opt-in by defining relevant facts.
%% Schema provides fallback behavior when facts are not defined.
%% -----------------------------------------------------------------------------

%% age_adjusted_count(+DisorderID, +CategoryID, +AgeThreshold, +AdjustedCount)
%% Allows disorders to specify different symptom counts for different age groups.
%% If not defined for a disorder, base count from symptom_category is used.
%%
%% Example:
%%   age_adjusted_count(adhd, inattention_symptoms, 17, 5).  % 5 for adults 17+
:- multifile age_adjusted_count/4.
:- discontiguous age_adjusted_count/4.

%% setting_requirement(+DisorderID, +MinSettings)
%% Specifies that symptoms must be present in multiple settings.
%% If not defined, setting check is not applicable for that disorder.
%%
%% Example:
%%   setting_requirement(adhd, 2).  % Symptoms in 2+ settings
:- multifile setting_requirement/2.
:- discontiguous setting_requirement/2.

%% disorder_age_range(+DisorderID, +MinAge, +MaxAge)
%% Specifies applicable age range for a disorder.
%% Use 0 for no minimum, 999 for no maximum.
%% If not defined, disorder has no age restriction.
%%
%% Example:
%%   disorder_age_range(ptsd_preschool, 0, 6).  % Children ≤6 only
:- multifile disorder_age_range/3.
:- discontiguous disorder_age_range/3.

%% Define age ranges for PTSD variants
disorder_age_range(ptsd, 7, 999).           % Adults, adolescents, children >6
disorder_age_range(ptsd_preschool, 0, 6).   % Children ≤6 only


%% =============================================================================
%% PART 2: PATIENT FACT PREDICATES
%% =============================================================================
%% These predicates are dynamically asserted based on clinical input.
%% They represent the current patient's presentation.
%% =============================================================================

:- dynamic patient_symptom/4.
:- dynamic patient_duration/3.
:- dynamic patient_onset_age/2.
:- dynamic patient_exclusion_status/3.
:- dynamic subjective_assessment/4.
:- dynamic patient_context/3.

%% patient_symptom(+PatientID, +SymptomID, +Status, +Evidence)
%% Records presence/absence of symptoms for a patient.
%% Status: present, absent, unclear
%% Evidence: quote or description from clinical notes
%%
%% Example:
%%   patient_symptom(pt001, mdd_sym_01, present, 
%%       'Patient reports feeling sad most days for past month').

%% patient_duration(+PatientID, +DisorderID, +DurationDays)
%% Records how long symptoms have been present (normalised to days).
%%
%% Example:
%%   patient_duration(pt001, mdd, 21).  % 3 weeks

%% patient_onset_age(+PatientID, +OnsetAge)
%% Records age when symptoms first appeared.
%%
%% Example:
%%   patient_onset_age(pt001, 8).  % Relevant for ADHD

%% patient_exclusion_status(+PatientID, +ExclusionID, +Status)
%% Records whether exclusion criteria are met (i.e., diagnosis should be excluded).
%% Status: excluded (exclusion applies), cleared (exclusion does not apply), unknown
%%
%% Example:
%%   patient_exclusion_status(pt001, mdd_exc_01, cleared).  % No substance cause

%% subjective_assessment(+PatientID, +CriterionID, +Assessment, +Confidence)
%% Records LLM or clinician judgment on subjective criteria.
%% Assessment: met, not_met, unclear
%% Confidence: 0.0 to 1.0
%%
%% Example:
%%   subjective_assessment(pt001, mdd_subj_01, met, 0.85).

%% patient_context(+PatientID, +ContextType, +Value)
%% Additional patient information relevant to diagnosis.
%% ContextType: age, settings_affected, trauma_history, developmental_history
%%
%% Example:
%%   patient_context(pt001, age, 34).
%%   patient_context(pt001, settings_affected, [work, home]).


%% =============================================================================
%% PART 3: DIAGNOSTIC INFERENCE PREDICATES
%% =============================================================================
%% These predicates implement the diagnostic logic, combining knowledge base
%% definitions with patient facts.
%% =============================================================================

%% meets_symptom_criteria(+PatientID, +DisorderID)
%% True if patient meets symptom count requirements for all categories.
meets_symptom_criteria(PatientID, DisorderID) :-
    disorder(DisorderID, _, _),
    forall(
        symptom_category(DisorderID, CategoryID, SymptomList, RequiredCount, RequirementType),
        category_satisfied(PatientID, DisorderID, CategoryID, SymptomList, RequiredCount, RequirementType)
    ).

%% category_satisfied/6 - Helper for symptom category checking
category_satisfied(PatientID, DisorderID, CategoryID, SymptomList, BaseRequiredCount, at_least) :-
    get_required_count(PatientID, DisorderID, CategoryID, BaseRequiredCount, AdjustedRequiredCount),
    count_present_symptoms(PatientID, SymptomList, Count),
    Count >= AdjustedRequiredCount.

category_satisfied(PatientID, DisorderID, CategoryID, SymptomList, BaseRequiredCount, exactly) :-
    get_required_count(PatientID, DisorderID, CategoryID, BaseRequiredCount, AdjustedRequiredCount),
    count_present_symptoms(PatientID, SymptomList, Count),
    Count =:= AdjustedRequiredCount.

category_satisfied(PatientID, _DisorderID, _CategoryID, SymptomList, _RequiredCount, all) :-
    forall(
        member(SymptomID, SymptomList),
        patient_symptom(PatientID, SymptomID, present, _)
    ).

category_satisfied(PatientID, _DisorderID, _CategoryID, SymptomList, _RequiredCount, at_least_one_of) :-
    member(SymptomID, SymptomList),
    patient_symptom(PatientID, SymptomID, present, _),
    !.

%% count_present_symptoms/3 - Count how many symptoms from list are present
count_present_symptoms(PatientID, SymptomList, Count) :-
    findall(S, (
        member(S, SymptomList),
        patient_symptom(PatientID, S, present, _)
    ), PresentSymptoms),
    length(PresentSymptoms, Count).

%% meets_duration_criteria(+PatientID, +DisorderID)
%% True if patient symptoms have persisted for required duration.
meets_duration_criteria(PatientID, DisorderID) :-
    duration_requirement(DisorderID, MinDuration, Unit),
    normalise_to_days(MinDuration, Unit, MinDays),
    patient_duration(PatientID, DisorderID, ActualDays),
    ActualDays >= MinDays.

%% Duration not specified = automatically met
meets_duration_criteria(_PatientID, DisorderID) :-
    \+ duration_requirement(DisorderID, _, _).

%% normalise_to_days/3 - Convert duration to days
normalise_to_days(N, days, N).
normalise_to_days(N, weeks, Days) :- Days is N * 7.
normalise_to_days(N, months, Days) :- Days is N * 30.
normalise_to_days(N, years, Days) :- Days is N * 365.

%% meets_onset_criteria(+PatientID, +DisorderID)
%% True if onset timing requirements are satisfied.
meets_onset_criteria(PatientID, DisorderID) :-
    onset_requirement(DisorderID, before_age, MaxAge),
    patient_onset_age(PatientID, OnsetAge),
    OnsetAge =< MaxAge.

meets_onset_criteria(PatientID, DisorderID) :-
    onset_requirement(DisorderID, after_event, EventType),
    patient_context(PatientID, EventType, present).

meets_onset_criteria(_PatientID, DisorderID) :-
    onset_requirement(DisorderID, any, _).

meets_onset_criteria(_PatientID, DisorderID) :-
    \+ onset_requirement(DisorderID, _, _).

%% meets_exclusion_criteria(+PatientID, +DisorderID)
%% True if NO exclusion criteria apply (i.e., all are cleared or unknown).
meets_exclusion_criteria(PatientID, DisorderID) :-
    forall(
        exclusion_criterion(DisorderID, ExclusionID, _, _),
        \+ patient_exclusion_status(PatientID, ExclusionID, excluded)
    ).

%% meets_subjective_criteria(+PatientID, +DisorderID)
%% True if all subjective criteria are assessed as met.
meets_subjective_criteria(PatientID, DisorderID) :-
    forall(
        subjective_criterion(DisorderID, CriterionID, _, _),
        subjective_assessment(PatientID, CriterionID, met, _)
    ).

%% diagnosis_candidate(+PatientID, +DisorderID, -ConfidenceScore)
%% Generates candidate diagnoses with confidence scores.
diagnosis_candidate(PatientID, DisorderID, Confidence) :-
    disorder(DisorderID, _, _),
    meets_symptom_criteria(PatientID, DisorderID),
    meets_duration_criteria(PatientID, DisorderID),
    meets_onset_criteria(PatientID, DisorderID),
    meets_exclusion_criteria(PatientID, DisorderID),
    meets_subjective_criteria(PatientID, DisorderID),
    calculate_confidence(PatientID, DisorderID, Confidence).

%% calculate_confidence/3 - Compute overall confidence from subjective assessments
calculate_confidence(PatientID, DisorderID, Confidence) :-
    findall(C, (
        subjective_criterion(DisorderID, CritID, _, _),
        subjective_assessment(PatientID, CritID, met, C)
    ), Confidences),
    (   Confidences = []
    ->  Confidence = 1.0
    ;   sum_list(Confidences, Sum),
        length(Confidences, Len),
        Confidence is Sum / Len
    ).


%% =============================================================================
%% PART 4: EXPLANATION PREDICATES
%% =============================================================================
%% These predicates support generating human-readable explanations of
%% diagnostic reasoning.
%% =============================================================================

%% criterion_status(+PatientID, +DisorderID, +CriterionType, -Status)
%% Returns status of each criterion type for explanation.
%% CriterionType: symptoms, duration, onset, exclusions, subjective
%% Status: met, not_met, partial, unknown

criterion_status(PatientID, DisorderID, symptoms, Status) :-
    (   meets_symptom_criteria(PatientID, DisorderID)
    ->  Status = met
    ;   Status = not_met
    ).

criterion_status(PatientID, DisorderID, duration, Status) :-
    (   meets_duration_criteria(PatientID, DisorderID)
    ->  Status = met
    ;   Status = not_met
    ).

criterion_status(PatientID, DisorderID, onset, Status) :-
    (   meets_onset_criteria(PatientID, DisorderID)
    ->  Status = met
    ;   Status = not_met
    ).

criterion_status(PatientID, DisorderID, exclusions, Status) :-
    (   meets_exclusion_criteria(PatientID, DisorderID)
    ->  Status = met
    ;   Status = not_met
    ).

criterion_status(PatientID, DisorderID, subjective, Status) :-
    (   meets_subjective_criteria(PatientID, DisorderID)
    ->  Status = met
    ;   Status = not_met
    ).

%% explain_diagnosis(+PatientID, +DisorderID, -Explanation)
%% Generates structured explanation of diagnostic reasoning.
explain_diagnosis(PatientID, DisorderID, Explanation) :-
    disorder(DisorderID, Name, _),
    criterion_status(PatientID, DisorderID, symptoms, SympStatus),
    criterion_status(PatientID, DisorderID, duration, DurStatus),
    criterion_status(PatientID, DisorderID, onset, OnsetStatus),
    criterion_status(PatientID, DisorderID, exclusions, ExclStatus),
    criterion_status(PatientID, DisorderID, subjective, SubjStatus),
    get_symptom_details(PatientID, DisorderID, SymptomDetails),
    Explanation = explanation{
        disorder: Name,
        disorder_id: DisorderID,
        criteria: criteria{
            symptoms: SympStatus,
            duration: DurStatus,
            onset: OnsetStatus,
            exclusions: ExclStatus,
            subjective: SubjStatus
        },
        symptom_evidence: SymptomDetails,
        overall: (SympStatus = met, DurStatus = met, OnsetStatus = met, 
                  ExclStatus = met, SubjStatus = met -> met ; not_met)
    }.

%% get_symptom_details/3 - Collect evidence for present symptoms
get_symptom_details(PatientID, DisorderID, Details) :-
    findall(
        detail{symptom_id: SID, description: Desc, evidence: Ev},
        (
            symptom(DisorderID, SID, _, Desc),
            patient_symptom(PatientID, SID, present, Ev)
        ),
        Details
    ).


%% =============================================================================
%% PART 5: UTILITY PREDICATES
%% =============================================================================

%% clear_patient_facts/1 - Remove all facts for a patient
clear_patient_facts(PatientID) :-
    retractall(patient_symptom(PatientID, _, _, _)),
    retractall(patient_duration(PatientID, _, _)),
    retractall(patient_onset_age(PatientID, _)),
    retractall(patient_exclusion_status(PatientID, _, _)),
    retractall(subjective_assessment(PatientID, _, _, _)),
    retractall(patient_context(PatientID, _, _)).

%% clear_all_patient_facts/0 - Remove all patient facts
clear_all_patient_facts :-
    retractall(patient_symptom(_, _, _, _)),
    retractall(patient_duration(_, _, _)),
    retractall(patient_onset_age(_, _)),
    retractall(patient_exclusion_status(_, _, _)),
    retractall(subjective_assessment(_, _, _, _)),
    retractall(patient_context(_, _, _)).

%% list_pending_subjective/2 - Find subjective criteria not yet assessed
list_pending_subjective(PatientID, DisorderID, Pending) :-
    findall(
        pending{criterion_id: CID, description: Desc, type: Type},
        (
            subjective_criterion(DisorderID, CID, Desc, Type),
            \+ subjective_assessment(PatientID, CID, _, _)
        ),
        Pending
    ).

%% all_disorders/1 - List all defined disorders
all_disorders(Disorders) :-
    findall(DisorderID, disorder(DisorderID, _, _), Disorders).

%% symptoms_for_disorder/2 - List all symptoms for a disorder
symptoms_for_disorder(DisorderID, Symptoms) :-
    findall(
        sym{id: SID, category: Cat, description: Desc},
        symptom(DisorderID, SID, Cat, Desc),
        Symptoms
    ).


%% =============================================================================
%% PART 6: SCHEMA VALIDATION PREDICATES
%% =============================================================================
%% Used to validate extracted knowledge bases for consistency.
%% =============================================================================

%% validate_disorder/2 - Check a disorder has required components
validate_disorder(DisorderID, Issues) :-
    findall(Issue, disorder_issue(DisorderID, Issue), Issues).

disorder_issue(DisorderID, 'No symptoms defined') :-
    disorder(DisorderID, _, _),
    \+ symptom(DisorderID, _, _, _).

disorder_issue(DisorderID, 'No symptom categories defined') :-
    disorder(DisorderID, _, _),
    \+ symptom_category(DisorderID, _, _, _, _).

disorder_issue(DisorderID, 'Symptom in category not defined') :-
    symptom_category(DisorderID, _, SymptomList, _, _),
    member(SID, SymptomList),
    \+ symptom(DisorderID, SID, _, _).

disorder_issue(DisorderID, 'No subjective criterion for clinical significance') :-
    disorder(DisorderID, _, _),
    \+ subjective_criterion(DisorderID, _, _, clinical_significance).

%% validate_all/1 - Validate all disorders
validate_all(AllIssues) :-
    findall(
        validation{disorder: DID, issues: Issues},
        (disorder(DID, _, _), validate_disorder(DID, Issues), Issues \= []),
        AllIssues
    ).


%% =============================================================================
%% PART 7: ENHANCED DIAGNOSTIC SYSTEM WITH STATUS TRACKING
%% =============================================================================
%% Status-based criterion checks that distinguish between:
%%   - met: criterion is satisfied
%%   - not_met: criterion is explicitly not satisfied
%%   - missing_data: required data not available
%%   - not_applicable: criterion doesn't apply to this disorder
%%
%% This enables tracking of incomplete assessments and follow-up needs.
%% =============================================================================

%% Declare criterion_check as discontiguous (clauses spread across sections)
:- discontiguous criterion_check/5.

%% -----------------------------------------------------------------------------
%% 7.1 Age-Adjusted Count Helper
%% -----------------------------------------------------------------------------
%% Gets the required count, adjusting for patient age if disorder defines
%% age_adjusted_count/4 for that category.
%% -----------------------------------------------------------------------------

%% get_required_count(+PatientID, +DisorderID, +CategoryID, +BaseCount, -AdjustedCount)
get_required_count(PatientID, DisorderID, CategoryID, BaseCount, AdjustedCount) :-
    (   age_adjusted_count(DisorderID, CategoryID, AgeThreshold, AdultCount),
        patient_context(PatientID, age, Age),
        Age >= AgeThreshold
    ->  AdjustedCount = AdultCount
    ;   AdjustedCount = BaseCount
    ).

%% -----------------------------------------------------------------------------
%% 7.2 Symptom Criterion Check
%% -----------------------------------------------------------------------------

%% criterion_check(+PatientID, +DisorderID, symptoms, -Status, -Details)
criterion_check(PatientID, DisorderID, symptoms, Status, Details) :-
    disorder(DisorderID, _, _),
    (   \+ symptom_category(DisorderID, _, _, _, _)
    ->  Status = not_applicable, Details = []
    ;   check_all_symptom_categories(PatientID, DisorderID, CategoryResults),
        aggregate_symptom_status(DisorderID, CategoryResults, Status, Details)
    ).

%% check_all_symptom_categories/3 - Check each category and collect results
check_all_symptom_categories(PatientID, DisorderID, Results) :-
    findall(
        category_result{
            category_id: CategoryID,
            status: CatStatus,
            present_count: PresentCount,
            required_count: RequiredCount,
            missing_symptoms: MissingSymptoms
        },
        check_single_category(PatientID, DisorderID, CategoryID, CatStatus,
                             PresentCount, RequiredCount, MissingSymptoms),
        Results
    ).

%% check_single_category/7 - Check one symptom category
check_single_category(PatientID, DisorderID, CategoryID, Status,
                      PresentCount, RequiredCount, MissingSymptoms) :-
    symptom_category(DisorderID, CategoryID, SymptomList, BaseCount, RequirementType),
    get_required_count(PatientID, DisorderID, CategoryID, BaseCount, RequiredCount),
    count_present_symptoms(PatientID, SymptomList, PresentCount),
    find_unevaluated_symptoms(PatientID, SymptomList, MissingSymptoms),
    determine_category_status(RequirementType, PresentCount, RequiredCount,
                              MissingSymptoms, SymptomList, Status).

%% find_unevaluated_symptoms/3 - Find symptoms with no patient_symptom fact
find_unevaluated_symptoms(PatientID, SymptomList, Missing) :-
    findall(
        SID,
        (   member(SID, SymptomList),
            \+ patient_symptom(PatientID, SID, _, _)
        ),
        Missing
    ).

%% determine_category_status/6 - Determine status based on requirement type
determine_category_status(at_least, PresentCount, RequiredCount, _, _, met) :-
    PresentCount >= RequiredCount, !.
determine_category_status(at_least, PresentCount, RequiredCount, Missing, _, Status) :-
    PresentCount < RequiredCount,
    length(Missing, MissingLen),
    PotentialMax is PresentCount + MissingLen,
    (   PotentialMax >= RequiredCount
    ->  Status = missing_data  % Could still meet criteria if missing data is gathered
    ;   Status = not_met       % Cannot meet criteria even if all missing were present
    ).

determine_category_status(exactly, PresentCount, RequiredCount, [], _, met) :-
    PresentCount =:= RequiredCount, !.
determine_category_status(exactly, _, _, Missing, _, missing_data) :-
    Missing \= [], !.
determine_category_status(exactly, _, _, _, _, not_met).

determine_category_status(all, _, _, Missing, _, missing_data) :-
    Missing \= [], !.
determine_category_status(all, PresentCount, _, [], SymptomList, Status) :-
    length(SymptomList, Total),
    (   PresentCount =:= Total
    ->  Status = met
    ;   Status = not_met
    ).

determine_category_status(at_least_one_of, PresentCount, _, _, _, met) :-
    PresentCount >= 1, !.
determine_category_status(at_least_one_of, 0, _, Missing, _, Status) :-
    (   Missing \= []
    ->  Status = missing_data
    ;   Status = not_met
    ).

%% aggregate_symptom_status/4 - Combine category results into overall status (disorder-aware)
%% ADHD uses OR logic (either category can pass), other disorders use AND logic
aggregate_symptom_status(DisorderID, CategoryResults, Status, Details) :-
    (   DisorderID = adhd
    ->  adhd_symptom_check(CategoryResults, Status, Details)
    ;   standard_symptom_check(CategoryResults, Status, Details)
    ).

%% adhd_symptom_check/3 - ADHD allows EITHER category to pass (OR logic per DSM-5)
%% DSM-5 Criterion A: "inattention and/or hyperactivity-impulsivity"
%% Three presentations: Combined (both), Predominantly Inattentive (A1 only), Predominantly Hyperactive (A2 only)
adhd_symptom_check(CategoryResults, met, CategoryResults) :-
    member(R, CategoryResults),
    R.status = met,
    !.  % At least one category met = symptoms criterion satisfied

adhd_symptom_check(CategoryResults, missing_data, CategoryResults) :-
    member(R, CategoryResults),
    R.status = missing_data,
    \+ (member(R2, CategoryResults), R2.status = met),
    !.  % Has missing data but no category is met yet

adhd_symptom_check(CategoryResults, not_met, CategoryResults).
%% All categories are not_met = symptoms criterion not met

%% standard_symptom_check/3 - Original logic for other disorders (AND - all categories must be met)
standard_symptom_check(CategoryResults, not_met, CategoryResults) :-
    member(R, CategoryResults), R.status = not_met, !.
standard_symptom_check(CategoryResults, missing_data, CategoryResults) :-
    member(R, CategoryResults), R.status = missing_data, !.
standard_symptom_check(CategoryResults, met, CategoryResults).

%% -----------------------------------------------------------------------------
%% 7.3 Duration Criterion Check
%% -----------------------------------------------------------------------------

%% criterion_check(+PatientID, +DisorderID, duration, -Status, -Details)
criterion_check(PatientID, DisorderID, duration, Status, Details) :-
    (   \+ duration_requirement(DisorderID, _, _)
    ->  Status = not_applicable, Details = []
    ;   duration_requirement(DisorderID, MinDuration, Unit),
        normalise_to_days(MinDuration, Unit, MinDays),
        (   patient_duration(PatientID, DisorderID, ActualDays)
        ->  (   ActualDays >= MinDays
            ->  Status = met,
                Details = [actual_days(ActualDays), required_days(MinDays)]
            ;   Status = not_met,
                Details = [actual_days(ActualDays), required_days(MinDays)]
            )
        ;   Status = missing_data,
            Details = [required_days(MinDays)]
        )
    ).

%% -----------------------------------------------------------------------------
%% 7.4 Onset Criterion Check
%% -----------------------------------------------------------------------------

%% criterion_check(+PatientID, +DisorderID, onset, -Status, -Details)
criterion_check(PatientID, DisorderID, onset, Status, Details) :-
    (   \+ onset_requirement(DisorderID, _, _)
    ->  Status = not_applicable, Details = []
    ;   onset_requirement(DisorderID, any, _)
    ->  Status = not_applicable, Details = []
    ;   onset_requirement(DisorderID, before_age, MaxAge)
    ->  (   patient_onset_age(PatientID, OnsetAge)
        ->  (   OnsetAge =< MaxAge
            ->  Status = met, Details = [onset_age(OnsetAge), max_age(MaxAge)]
            ;   Status = not_met, Details = [onset_age(OnsetAge), max_age(MaxAge)]
            )
        ;   Status = missing_data, Details = [max_age(MaxAge)]
        )
    ;   onset_requirement(DisorderID, after_event, EventType)
    ->  (   patient_context(PatientID, EventType, present)
        ->  Status = met, Details = [event_type(EventType)]
        ;   patient_context(PatientID, EventType, absent)
        ->  Status = not_met, Details = [event_type(EventType)]
        ;   Status = missing_data, Details = [event_type(EventType)]
        )
    ).

%% -----------------------------------------------------------------------------
%% 7.5 Exclusion Criterion Check
%% -----------------------------------------------------------------------------

%% criterion_check(+PatientID, +DisorderID, exclusions, -Status, -Details)
criterion_check(PatientID, DisorderID, exclusions, Status, Details) :-
    (   \+ exclusion_criterion(DisorderID, _, _, _)
    ->  Status = not_applicable, Details = []
    ;   check_all_exclusions(PatientID, DisorderID, ExclusionResults),
        aggregate_exclusion_status(ExclusionResults, Status, Details)
    ).

%% check_all_exclusions/3 - Check each exclusion criterion
check_all_exclusions(PatientID, DisorderID, Results) :-
    findall(
        exclusion_result{
            exclusion_id: ExclusionID,
            type: Type,
            status: ExclStatus
        },
        check_single_exclusion(PatientID, DisorderID, ExclusionID, Type, ExclStatus),
        Results
    ).

%% check_single_exclusion/5 - Check one exclusion criterion
check_single_exclusion(PatientID, DisorderID, ExclusionID, Type, Status) :-
    exclusion_criterion(DisorderID, ExclusionID, Type, _),
    (   patient_exclusion_status(PatientID, ExclusionID, cleared)
    ->  Status = cleared
    ;   patient_exclusion_status(PatientID, ExclusionID, excluded)
    ->  Status = excluded
    ;   Status = unassessed
    ).

%% aggregate_exclusion_status/3 - Combine exclusion results
%% met = all cleared or unassessed (can proceed cautiously)
%% not_met = any excluded
%% missing_data = some unassessed (flag for follow-up)
aggregate_exclusion_status(ExclusionResults, Status, Details) :-
    (   member(R, ExclusionResults), R.status = excluded
    ->  Status = not_met, Details = ExclusionResults
    ;   member(R, ExclusionResults), R.status = unassessed
    ->  Status = missing_data, Details = ExclusionResults
    ;   Status = met, Details = ExclusionResults
    ).

%% -----------------------------------------------------------------------------
%% 7.6 Subjective Criterion Check
%% -----------------------------------------------------------------------------

%% criterion_check(+PatientID, +DisorderID, subjective, -Status, -Details)
criterion_check(PatientID, DisorderID, subjective, Status, Details) :-
    (   \+ subjective_criterion(DisorderID, _, _, _)
    ->  Status = not_applicable, Details = []
    ;   check_all_subjective(PatientID, DisorderID, SubjResults),
        aggregate_subjective_status(SubjResults, Status, Details)
    ).

%% check_all_subjective/3 - Check each subjective criterion
check_all_subjective(PatientID, DisorderID, Results) :-
    findall(
        subjective_result{
            criterion_id: CriterionID,
            type: Type,
            status: SubjStatus,
            confidence: Conf
        },
        check_single_subjective(PatientID, DisorderID, CriterionID, Type, SubjStatus, Conf),
        Results
    ).

%% check_single_subjective/6 - Check one subjective criterion
check_single_subjective(PatientID, DisorderID, CriterionID, Type, Status, Confidence) :-
    subjective_criterion(DisorderID, CriterionID, _, Type),
    (   subjective_assessment(PatientID, CriterionID, met, Conf)
    ->  Status = met, Confidence = Conf
    ;   subjective_assessment(PatientID, CriterionID, not_met, Conf)
    ->  Status = not_met, Confidence = Conf
    ;   subjective_assessment(PatientID, CriterionID, unclear, Conf)
    ->  Status = unclear, Confidence = Conf
    ;   Status = unassessed, Confidence = 0.0
    ).

%% aggregate_subjective_status/3 - Combine subjective results
aggregate_subjective_status(SubjResults, Status, Details) :-
    (   member(R, SubjResults), R.status = not_met
    ->  Status = not_met, Details = SubjResults
    ;   member(R, SubjResults), R.status = unassessed
    ->  Status = missing_data, Details = SubjResults
    ;   member(R, SubjResults), R.status = unclear
    ->  Status = missing_data, Details = SubjResults
    ;   Status = met, Details = SubjResults
    ).

%% -----------------------------------------------------------------------------
%% 7.7 Setting Criterion Check (Optional - ADHD-specific)
%% -----------------------------------------------------------------------------

%% criterion_check(+PatientID, +DisorderID, settings, -Status, -Details)
criterion_check(PatientID, DisorderID, settings, Status, Details) :-
    (   \+ setting_requirement(DisorderID, _)
    ->  Status = not_applicable, Details = []
    ;   setting_requirement(DisorderID, MinSettings),
        findall(S, patient_context(PatientID, setting, S), Settings),
        length(Settings, Count),
        (   Count >= MinSettings
        ->  Status = met,
            Details = [settings_found(Settings), required(MinSettings)]
        ;   Count > 0
        ->  Status = not_met,
            Details = [settings_found(Settings), required(MinSettings)]
        ;   Status = missing_data,
            Details = [required(MinSettings)]
        )
    ).


%% =============================================================================
%% PART 8: MISSING DATA COLLECTORS
%% =============================================================================
%% Aggregates information about incomplete assessments to guide follow-up.
%% =============================================================================

%% collect_missing_data(+PatientID, +DisorderID, -MissingList)
%% Collects all criteria with missing_data status
collect_missing_data(PatientID, DisorderID, MissingList) :-
    findall(
        missing{criterion: CriterionName, details: Details},
        (   member(CriterionName, [symptoms, duration, onset, exclusions, subjective, settings]),
            criterion_check(PatientID, DisorderID, CriterionName, missing_data, Details)
        ),
        MissingList
    ).

%% collect_unassessed_exclusions(+PatientID, +DisorderID, -Unassessed)
%% Lists exclusion criteria that haven't been evaluated
collect_unassessed_exclusions(PatientID, DisorderID, Unassessed) :-
    findall(
        unassessed_exclusion{id: ExclusionID, type: Type, description: Desc},
        (   exclusion_criterion(DisorderID, ExclusionID, Type, Desc),
            \+ patient_exclusion_status(PatientID, ExclusionID, _)
        ),
        Unassessed
    ).

%% collect_unevaluated_symptoms(+PatientID, +DisorderID, -Unevaluated)
%% Lists symptoms that haven't been evaluated
collect_unevaluated_symptoms(PatientID, DisorderID, Unevaluated) :-
    findall(
        unevaluated_symptom{id: SymptomID, category: Cat, description: Desc},
        (   symptom(DisorderID, SymptomID, Cat, Desc),
            \+ patient_symptom(PatientID, SymptomID, _, _)
        ),
        Unevaluated
    ).

%% collect_pending_subjective(+PatientID, +DisorderID, -Pending)
%% Lists subjective criteria awaiting assessment
collect_pending_subjective(PatientID, DisorderID, Pending) :-
    findall(
        pending_subjective{id: CriterionID, type: Type, description: Desc},
        (   subjective_criterion(DisorderID, CriterionID, Desc, Type),
            \+ subjective_assessment(PatientID, CriterionID, _, _)
        ),
        Pending
    ).

%% generate_follow_up_questions(+PatientID, +DisorderID, -Questions)
%% Generates prioritized follow-up questions based on missing data
generate_follow_up_questions(PatientID, DisorderID, Questions) :-
    collect_missing_data(PatientID, DisorderID, Missing),
    maplist(missing_to_question(DisorderID), Missing, QuestionLists),
    append(QuestionLists, Questions).

%% missing_to_question/3 - Convert missing data item to follow-up question
missing_to_question(DisorderID, missing{criterion: symptoms, details: Details}, Questions) :-
    findall(
        question{
            priority: high,
            category: symptoms,
            symptom_id: SID,
            text: QuestionText
        },
        (   member(CatResult, Details),
            member(SID, CatResult.missing_symptoms),
            symptom(DisorderID, SID, _, Desc),
            format(atom(QuestionText), 'Has the patient experienced: ~w?', [Desc])
        ),
        Questions
    ).

missing_to_question(_, missing{criterion: duration, details: _}, [
    question{
        priority: high,
        category: duration,
        text: 'How long have these symptoms been present?'
    }
]).

missing_to_question(_, missing{criterion: onset, details: Details}, Questions) :-
    (   member(max_age(_), Details)
    ->  Questions = [question{priority: medium, category: onset,
                              text: 'At what age did symptoms first appear?'}]
    ;   member(event_type(Event), Details)
    ->  format(atom(Q), 'Is there a history of ~w?', [Event]),
        Questions = [question{priority: medium, category: onset, text: Q}]
    ;   Questions = []
    ).

missing_to_question(DisorderID, missing{criterion: exclusions, details: Details}, Questions) :-
    findall(
        question{
            priority: high,
            category: exclusions,
            exclusion_id: ExcID,
            text: QuestionText
        },
        (   member(ExclResult, Details),
            ExclResult.status = unassessed,
            ExcID = ExclResult.exclusion_id,
            exclusion_criterion(DisorderID, ExcID, _, Desc),
            format(atom(QuestionText), 'Assess exclusion: ~w', [Desc])
        ),
        Questions
    ).

missing_to_question(DisorderID, missing{criterion: subjective, details: Details}, Questions) :-
    findall(
        question{
            priority: medium,
            category: subjective,
            criterion_id: CritID,
            text: QuestionText
        },
        (   member(SubjResult, Details),
            (SubjResult.status = unassessed ; SubjResult.status = unclear),
            CritID = SubjResult.criterion_id,
            subjective_criterion(DisorderID, CritID, Desc, _),
            format(atom(QuestionText), 'Evaluate: ~w', [Desc])
        ),
        Questions
    ).

missing_to_question(_, missing{criterion: settings, details: _}, [
    question{
        priority: medium,
        category: settings,
        text: 'In which settings have symptoms been observed? (e.g., home, school, work)'
    }
]).


%% =============================================================================
%% PART 9: ENHANCED DIAGNOSIS PREDICATE
%% =============================================================================
%% Comprehensive diagnosis with full status tracking.
%% =============================================================================

%% full_diagnosis(+PatientID, +DisorderID, -Result)
%% Returns comprehensive diagnostic result with all status information
full_diagnosis(PatientID, DisorderID, Result) :-
    disorder(DisorderID, Name, Category),

    % Check all criteria
    criterion_check(PatientID, DisorderID, symptoms, SympStatus, SympDetails),
    criterion_check(PatientID, DisorderID, duration, DurStatus, DurDetails),
    criterion_check(PatientID, DisorderID, onset, OnsetStatus, OnsetDetails),
    criterion_check(PatientID, DisorderID, exclusions, ExclStatus, ExclDetails),
    criterion_check(PatientID, DisorderID, subjective, SubjStatus, SubjDetails),
    criterion_check(PatientID, DisorderID, settings, SetStatus, SetDetails),

    % Collect missing data
    collect_missing_data(PatientID, DisorderID, MissingList),
    collect_unassessed_exclusions(PatientID, DisorderID, UnassessedExcl),

    % Generate follow-up questions
    (   MissingList \= []
    ->  generate_follow_up_questions(PatientID, DisorderID, FollowUp)
    ;   FollowUp = []
    ),

    % Determine overall status
    CriteriaStatuses = [SympStatus, DurStatus, OnsetStatus, ExclStatus, SubjStatus, SetStatus],
    determine_overall_status(CriteriaStatuses, OverallStatus),

    % Calculate confidence
    calculate_enhanced_confidence(PatientID, DisorderID, CriteriaStatuses, Confidence),

    % Build result
    Result = diagnosis_result{
        disorder_id: DisorderID,
        disorder_name: Name,
        category: Category,
        overall_status: OverallStatus,
        confidence: Confidence,
        criteria: [
            criterion{name: symptoms, status: SympStatus, details: SympDetails},
            criterion{name: duration, status: DurStatus, details: DurDetails},
            criterion{name: onset, status: OnsetStatus, details: OnsetDetails},
            criterion{name: exclusions, status: ExclStatus, details: ExclDetails},
            criterion{name: subjective, status: SubjStatus, details: SubjDetails},
            criterion{name: settings, status: SetStatus, details: SetDetails}
        ],
        missing_data: MissingList,
        unassessed_exclusions: UnassessedExcl,
        follow_up_needed: FollowUp
    }.

%% determine_overall_status/2 - Determine overall diagnostic status
determine_overall_status(Statuses, not_met) :-
    member(not_met, Statuses), !.
determine_overall_status(Statuses, incomplete) :-
    member(missing_data, Statuses), !.
determine_overall_status(_, met).

%% calculate_enhanced_confidence/4 - Calculate confidence considering missing data
calculate_enhanced_confidence(PatientID, DisorderID, Statuses, Confidence) :-
    % Base confidence from subjective assessments
    findall(C, (
        subjective_criterion(DisorderID, CritID, _, _),
        subjective_assessment(PatientID, CritID, met, C)
    ), SubjConfidences),

    % Calculate symptom match rate for evidence-based confidence
    calculate_symptom_match_rate(PatientID, DisorderID, MatchRate),

    % Calculate base confidence: combine subjective and symptom match
    (   SubjConfidences = []
    ->  BaseConf = MatchRate  % Use symptom match rate, NOT 1.0!
    ;   sum_list(SubjConfidences, Sum),
        length(SubjConfidences, Len),
        SubjConf is Sum / Len,
        BaseConf is (SubjConf + MatchRate) / 2
    ),

    % Penalize for missing data (each missing_data reduces confidence by 10%)
    include(=(missing_data), Statuses, MissingStatuses),
    length(MissingStatuses, MissingCount),
    Penalty is MissingCount * 0.1,
    Confidence is max(0.0, BaseConf - Penalty).

%% calculate_symptom_match_rate/3 - Ratio of present to required symptoms
calculate_symptom_match_rate(PatientID, DisorderID, Rate) :-
    findall(cat(Present, Required), (
        symptom_category(DisorderID, _, SymptomList, Required, _),
        count_present_symptoms(PatientID, SymptomList, Present)
    ), Categories),
    (   Categories = []
    ->  Rate = 0.5  % Default 50% for disorders without symptom categories
    ;   aggregate_all(sum(P), member(cat(P, _), Categories), TotalPresent),
        aggregate_all(sum(R), member(cat(_, R), Categories), TotalRequired),
        (   TotalRequired > 0
        ->  Rate is min(1.0, TotalPresent / TotalRequired)
        ;   Rate = 0.5
        )
    ).

%% quick_diagnosis(+PatientID, +DisorderID, -Status, -Confidence)
%% Simplified wrapper for backwards compatibility
quick_diagnosis(PatientID, DisorderID, Status, Confidence) :-
    full_diagnosis(PatientID, DisorderID, Result),
    Status = Result.overall_status,
    Confidence = Result.confidence.

%% diagnosis_summary(+PatientID, +DisorderID, -Summary)
%% Human-readable summary of diagnosis
diagnosis_summary(PatientID, DisorderID, Summary) :-
    full_diagnosis(PatientID, DisorderID, Result),
    format(atom(Summary),
           '~w Assessment~n  Status: ~w~n  Confidence: ~2f~n  Missing Data: ~w items~n  Follow-up Needed: ~w questions',
           [Result.disorder_name, Result.overall_status, Result.confidence,
            Result.missing_data, Result.follow_up_needed]).
