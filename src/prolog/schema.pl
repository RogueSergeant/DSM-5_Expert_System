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
%%   Diagnostics:    criterion_check/5, full_diagnosis/3, quick_diagnosis/4
%%   Explanation:    explain_diagnosis/3, collect_missing_data/3
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

%% symptom_logic(+DisorderID, +LogicType)
%% Specifies how symptom categories combine for a disorder.
%% LogicType: 'or_any' (any category satisfies criterion), default is AND (all must be met)
%% If not defined for a disorder, AND logic is used.
%%
%% Example:
%%   symptom_logic(adhd, or_any).  % Either inattention OR hyperactivity can satisfy
:- multifile symptom_logic/2.
:- discontiguous symptom_logic/2.


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
%% PART 3: SHARED UTILITY PREDICATES
%% =============================================================================
%% Utility predicates used by the diagnostic inference engine.
%% =============================================================================

%% count_present_symptoms/3 - Count how many symptoms from list are present
count_present_symptoms(PatientID, SymptomList, Count) :-
    findall(S, (
        member(S, SymptomList),
        patient_symptom(PatientID, S, present, _)
    ), PresentSymptoms),
    length(PresentSymptoms, Count).

%% normalise_to_days/3 - Convert duration to days
normalise_to_days(N, days, N).
normalise_to_days(N, weeks, Days) :- Days is N * 7.
normalise_to_days(N, months, Days) :- Days is N * 30.
normalise_to_days(N, years, Days) :- Days is N * 365.


%% =============================================================================
%% PART 4: EXPLANATION PREDICATES
%% =============================================================================
%% These predicates support generating human-readable explanations of
%% diagnostic reasoning. Uses criterion_check/5 from Part 7.
%% =============================================================================

%% explain_diagnosis(+PatientID, +DisorderID, -Explanation)
%% Generates structured explanation of diagnostic reasoning.
explain_diagnosis(PatientID, DisorderID, Explanation) :-
    disorder(DisorderID, Name, _),
    criterion_check(PatientID, DisorderID, symptoms, SympStatus, _),
    criterion_check(PatientID, DisorderID, duration, DurStatus, _),
    criterion_check(PatientID, DisorderID, onset, OnsetStatus, _),
    criterion_check(PatientID, DisorderID, exclusions, ExclStatus, _),
    criterion_check(PatientID, DisorderID, subjective, SubjStatus, _),
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
        symptom_evidence: SymptomDetails
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

%% aggregate_symptom_status/4 - Combine category results into overall status
%% Uses symptom_logic/2 to determine OR vs AND logic (data-driven, not hardcoded)
aggregate_symptom_status(DisorderID, CategoryResults, Status, Details) :-
    (   symptom_logic(DisorderID, or_any)
    ->  or_any_symptom_check(CategoryResults, Status, Details)
    ;   and_all_symptom_check(CategoryResults, Status, Details)
    ).

%% or_any_symptom_check/3 - OR logic: any category meeting criteria satisfies the symptom requirement
%% Used when symptom_logic(DisorderID, or_any) is defined
or_any_symptom_check(CategoryResults, met, CategoryResults) :-
    member(R, CategoryResults),
    R.status = met,
    !.  % At least one category met = symptoms criterion satisfied

or_any_symptom_check(CategoryResults, missing_data, CategoryResults) :-
    member(R, CategoryResults),
    R.status = missing_data,
    \+ (member(R2, CategoryResults), R2.status = met),
    !.  % Has missing data but no category is met yet

or_any_symptom_check(CategoryResults, not_met, CategoryResults).
%% All categories are not_met = symptoms criterion not met

%% and_all_symptom_check/3 - AND logic: all categories must be met (default)
and_all_symptom_check(CategoryResults, not_met, CategoryResults) :-
    member(R, CategoryResults), R.status = not_met, !.
and_all_symptom_check(CategoryResults, missing_data, CategoryResults) :-
    member(R, CategoryResults), R.status = missing_data, !.
and_all_symptom_check(CategoryResults, met, CategoryResults).

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
%% PART 8: MISSING DATA COLLECTION
%% =============================================================================
%% Returns IDs of missing data items. Python handles text formatting.
%% =============================================================================

%% get_missing_items(+PatientID, +DisorderID, -Items)
%% Returns list of missing data items with IDs only (Python handles text formatting)
get_missing_items(PatientID, DisorderID, Items) :-
    findall(Item, get_missing_item(PatientID, DisorderID, Item), Items).

%% get_missing_item/3 - Get individual missing data items
get_missing_item(PatientID, DisorderID, item{type: symptom, id: SID, category: Cat}) :-
    symptom(DisorderID, SID, Cat, _),
    \+ patient_symptom(PatientID, SID, _, _).

get_missing_item(PatientID, DisorderID, item{type: duration, id: DisorderID, category: none}) :-
    duration_requirement(DisorderID, _, _),
    \+ patient_duration(PatientID, DisorderID, _).

% For before_age onset - check patient_onset_age
get_missing_item(PatientID, DisorderID, item{type: onset, id: DisorderID, category: none}) :-
    onset_requirement(DisorderID, before_age, _),
    \+ patient_onset_age(PatientID, _).

% For after_event onset - check patient_context
get_missing_item(PatientID, DisorderID, item{type: onset, id: DisorderID, category: none}) :-
    onset_requirement(DisorderID, after_event, EventType),
    \+ patient_context(PatientID, EventType, _).

get_missing_item(PatientID, DisorderID, item{type: exclusion, id: ExcID, category: Type}) :-
    exclusion_criterion(DisorderID, ExcID, Type, _),
    \+ patient_exclusion_status(PatientID, ExcID, _).

get_missing_item(PatientID, DisorderID, item{type: subjective, id: CritID, category: Type}) :-
    subjective_criterion(DisorderID, CritID, _, Type),
    \+ subjective_assessment(PatientID, CritID, _, _).

get_missing_item(PatientID, DisorderID, item{type: settings, id: DisorderID, category: none}) :-
    setting_requirement(DisorderID, _),
    \+ patient_context(PatientID, setting, _).


%% =============================================================================
%% PART 8b: OPTIMISED NEXT QUESTION RETRIEVAL
%% =============================================================================
%% Returns the single next question to ask, with description included.
%% Handles all active candidates, deduplication, and priority sorting in Prolog.
%% =============================================================================

%% missing_item_with_priority/7 - Get missing item with its priority value
missing_item_with_priority(PatientID, DID, symptom, SID, Cat, Desc, 1) :-
    symptom(DID, SID, Cat, Desc),
    \+ patient_symptom(PatientID, SID, _, _).

missing_item_with_priority(PatientID, DID, exclusion, ExcID, Type, Desc, 2) :-
    exclusion_criterion(DID, ExcID, Type, Desc),
    \+ patient_exclusion_status(PatientID, ExcID, _).

missing_item_with_priority(PatientID, DID, duration, DID, none, 'How long have these symptoms been present?', 3) :-
    duration_requirement(DID, _, _),
    \+ patient_duration(PatientID, DID, _).

missing_item_with_priority(PatientID, DID, onset, DID, none, 'At what age did symptoms first appear?', 4) :-
    onset_requirement(DID, before_age, _),
    \+ patient_onset_age(PatientID, _).

missing_item_with_priority(PatientID, DID, onset, DID, none, 'When did symptoms begin relative to the event?', 4) :-
    onset_requirement(DID, after_event, EventType),
    \+ patient_context(PatientID, EventType, _).

missing_item_with_priority(PatientID, DID, subjective, CritID, Type, Desc, 5) :-
    subjective_criterion(DID, CritID, Desc, Type),
    \+ subjective_assessment(PatientID, CritID, _, _).

missing_item_with_priority(PatientID, DID, settings, DID, none, 'In which settings do these symptoms occur?', 6) :-
    setting_requirement(DID, _),
    \+ patient_context(PatientID, setting, _).

%% compare_questions/3 - Sort by priority, then by ID alphabetically
compare_questions(Order, Q1, Q2) :-
    P1 = Q1.priority, P2 = Q2.priority,
    ID1 = Q1.id, ID2 = Q2.id,
    (P1 < P2 -> Order = (<)
    ; P1 > P2 -> Order = (>)
    ; compare(Order, ID1, ID2)).

%% dedupe_by_id/2 - Remove items with duplicate IDs (keep first)
dedupe_by_id([], []).
dedupe_by_id([H|T], [H|Result]) :-
    H_ID = H.id,
    exclude(has_same_id(H_ID), T, Filtered),
    dedupe_by_id(Filtered, Result).

%% has_same_id/2 - Helper for dedupe
has_same_id(ID, X) :- X.id = ID.

%% next_question(+PatientID, -Item)
%% Returns the single next question to ask, with description included.
next_question(PatientID, Item) :-
    findall(
        q{priority: Priority, id: ID, disorder: DID, type: Type, category: Cat, description: Desc},
        (
            disorder(DID, _, _),
            \+ disorder_pruned(PatientID, DID),
            missing_item_with_priority(PatientID, DID, Type, ID, Cat, Desc, Priority)
        ),
        AllItems
    ),
    AllItems \= [],
    predsort(compare_questions, AllItems, Sorted),
    dedupe_by_id(Sorted, Deduped),
    Deduped = [First|_],
    Item = First.


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

    % Collect missing items (IDs only - Python formats text)
    get_missing_items(PatientID, DisorderID, MissingItems),

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
        missing_items: MissingItems
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


%% =============================================================================
%% PART 10: QUESTION GENERATION
%% =============================================================================
%% Generates questions from the knowledge base for the diagnostic driver.
%% Python handles text formatting and ordering; Prolog provides the data.
%% =============================================================================

%% disorder_questions(+DisorderID, -Questions)
%% Generate all questions for a specific disorder.
%% Returns list of q(Type, ID, Category, Description) terms.
disorder_questions(DisorderID, Questions) :-
    disorder(DisorderID, _, _),
    findall(
        q(symptom, SID, Cat, Desc),
        symptom(DisorderID, SID, Cat, Desc),
        SymptomQs
    ),
    findall(
        q(exclusion, ExcID, Type, Desc),
        exclusion_criterion(DisorderID, ExcID, Type, Desc),
        ExclQs
    ),
    findall(
        q(subjective, CritID, Type, Desc),
        subjective_criterion(DisorderID, CritID, Desc, Type),
        SubjQs
    ),
    append([SymptomQs, ExclQs, SubjQs], Questions).

%% all_disorder_questions(-Questions)
%% Generate questions for all defined disorders.
%% Returns list with disorder ID attached: dq(DisorderID, QuestionList).
all_disorder_questions(Questions) :-
    findall(
        dq(DisorderID, Qs),
        (disorder(DisorderID, _, _), disorder_questions(DisorderID, Qs)),
        Questions
    ).

%% get_question_text(+DisorderID, +ItemType, +ItemID, -Description)
%% Retrieve the description text for a specific question item.
get_question_text(DisorderID, symptom, ItemID, Description) :-
    symptom(DisorderID, ItemID, _, Description), !.
get_question_text(DisorderID, exclusion, ItemID, Description) :-
    exclusion_criterion(DisorderID, ItemID, _, Description), !.
get_question_text(DisorderID, subjective, ItemID, Description) :-
    subjective_criterion(DisorderID, ItemID, Description, _), !.
get_question_text(_, duration, _, 'How long have these symptoms been present?') :- !.
get_question_text(_, onset, _, 'At what age did these symptoms first appear?') :- !.
get_question_text(_, settings, _, 'In which settings do these symptoms occur?') :- !.
get_question_text(_, _, _, 'No description available').


%% =============================================================================
%% PART 11: DISORDER PRUNING
%% =============================================================================
%% Determines whether a disorder can be definitively ruled out based on
%% current patient data, enabling efficient question reduction.
%% =============================================================================

%% disorder_pruned(+PatientID, +DisorderID)
%% True if the disorder is definitively ruled out for this patient.
%% A disorder is pruned if:
%%   1. All symptoms in a required category are explicitly absent
%%   2. An exclusion criterion is confirmed (i.e., the exclusion applies)

%% Rule 1: Category with all symptoms absent when at least one is required
disorder_pruned(PatientID, DisorderID) :-
    symptom_category(DisorderID, _CategoryID, SymptomList, RequiredCount, at_least),
    RequiredCount > 0,
    SymptomList \= [],
    % Check that ALL symptoms in category are explicitly absent
    forall(
        member(SID, SymptomList),
        patient_symptom(PatientID, SID, absent, _)
    ).

%% Rule 2: Category requires all symptoms, but any is absent
disorder_pruned(PatientID, DisorderID) :-
    symptom_category(DisorderID, _CategoryID, SymptomList, _, all),
    member(SID, SymptomList),
    patient_symptom(PatientID, SID, absent, _).

%% Rule 3: Exclusion criterion is confirmed
disorder_pruned(PatientID, DisorderID) :-
    exclusion_criterion(DisorderID, ExcID, _, _),
    patient_exclusion_status(PatientID, ExcID, excluded).

%% Rule 4: Age range mismatch (if disorder has age restriction)
disorder_pruned(PatientID, DisorderID) :-
    disorder_age_range(DisorderID, MinAge, MaxAge),
    patient_context(PatientID, age, Age),
    (Age < MinAge ; Age > MaxAge).

%% active_candidates(+PatientID, -Candidates)
%% Get list of disorders that have NOT been pruned.
active_candidates(PatientID, Candidates) :-
    findall(
        DisorderID,
        (disorder(DisorderID, _, _), \+ disorder_pruned(PatientID, DisorderID)),
        Candidates
    ).

%% pruned_disorders(+PatientID, -Pruned)
%% Get list of disorders that HAVE been pruned (for debugging/explanation).
pruned_disorders(PatientID, Pruned) :-
    findall(
        DisorderID,
        (disorder(DisorderID, _, _), disorder_pruned(PatientID, DisorderID)),
        Pruned
    ).

%% prune_reason(+PatientID, +DisorderID, -Reason)
%% Explain why a disorder was pruned.
prune_reason(PatientID, DisorderID, Reason) :-
    symptom_category(DisorderID, CategoryID, SymptomList, RequiredCount, at_least),
    RequiredCount > 0,
    forall(member(SID, SymptomList), patient_symptom(PatientID, SID, absent, _)),
    format(atom(Reason), 'All symptoms in category ~w are absent', [CategoryID]).

prune_reason(PatientID, DisorderID, Reason) :-
    exclusion_criterion(DisorderID, ExcID, _, _),
    patient_exclusion_status(PatientID, ExcID, excluded),
    format(atom(Reason), 'Exclusion criterion ~w applies', [ExcID]).

prune_reason(PatientID, DisorderID, Reason) :-
    disorder_age_range(DisorderID, MinAge, MaxAge),
    patient_context(PatientID, age, Age),
    (Age < MinAge ; Age > MaxAge),
    format(atom(Reason), 'Patient age ~w outside range ~w-~w', [Age, MinAge, MaxAge]).
