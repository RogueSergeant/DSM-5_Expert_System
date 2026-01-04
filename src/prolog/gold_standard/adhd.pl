%% =============================================================================
%% ATTENTION-DEFICIT/HYPERACTIVITY DISORDER (ADHD) - Gold Standard
%% =============================================================================
%% DSM-5 Reference: 314.00/314.01 (F90.0/F90.1/F90.2)
%% Key features:
%%   - Two symptom clusters: Inattention (A1) and Hyperactivity-Impulsivity (A2)
%%   - 6/9 symptoms per cluster for children, 5/9 for adults (17+)
%%   - Onset before age 12
%%   - Symptoms in 2+ settings
%%   - Three presentations: Combined, Predominantly Inattentive, Predominantly Hyperactive/Impulsive
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

%% Optional disorder-specific predicates (ADHD uses all three!)
:- multifile age_adjusted_count/4.
:- multifile setting_requirement/2.
:- multifile symptom_logic/2.

%% -----------------------------------------------------------------------------
%% Disorder Definition
%% -----------------------------------------------------------------------------
disorder(adhd, 'Attention-Deficit/Hyperactivity Disorder', neurodevelopmental_disorders).

%% -----------------------------------------------------------------------------
%% Symptoms - Criterion A1: Inattention (9 symptoms)
%% -----------------------------------------------------------------------------
%% Structure: symptom(DisorderID, SymptomID, Category, Description)
%%
%% Categories for ADHD:
%%   - inattention: Criterion A1 symptoms (a-i)
%%   - hyperactivity_impulsivity: Criterion A2 symptoms (a-i)
%% -----------------------------------------------------------------------------

% Inattention symptoms (A1a - A1i)
symptom(adhd, adhd_a1a, inattention,
    'Often fails to give close attention to details or makes careless mistakes in schoolwork, at work, or during other activities (e.g., overlooks or misses details, work is inaccurate).').

symptom(adhd, adhd_a1b, inattention,
    'Often has difficulty sustaining attention in tasks or play activities (e.g., has difficulty remaining focused during lectures, conversations, or lengthy reading).').

symptom(adhd, adhd_a1c, inattention,
    'Often does not seem to listen when spoken to directly (e.g., mind seems elsewhere, even in the absence of any obvious distraction).').

symptom(adhd, adhd_a1d, inattention,
    'Often does not follow through on instructions and fails to finish schoolwork, chores, or duties in the workplace (e.g., starts tasks but quickly loses focus and is easily sidetracked).').

symptom(adhd, adhd_a1e, inattention,
    'Often has difficulty organizing tasks and activities (e.g., difficulty managing sequential tasks; difficulty keeping materials and belongings in order; messy, disorganized work; has poor time management; fails to meet deadlines).').

symptom(adhd, adhd_a1f, inattention,
    'Often avoids, dislikes, or is reluctant to engage in tasks that require sustained mental effort (e.g., schoolwork or homework; for older adolescents and adults, preparing reports, completing forms, reviewing lengthy papers).').

symptom(adhd, adhd_a1g, inattention,
    'Often loses things necessary for tasks or activities (e.g., school materials, pencils, books, tools, wallets, keys, paperwork, eyeglasses, mobile telephones).').

symptom(adhd, adhd_a1h, inattention,
    'Is often easily distracted by extraneous stimuli (for older adolescents and adults, may include unrelated thoughts).').

symptom(adhd, adhd_a1i, inattention,
    'Is often forgetful in daily activities (e.g., doing chores, running errands; for older adolescents and adults, returning calls, paying bills, keeping appointments).').

%% -----------------------------------------------------------------------------
%% Symptoms - Criterion A2: Hyperactivity-Impulsivity (9 symptoms)
%% -----------------------------------------------------------------------------

% Hyperactivity symptoms (A2a - A2f)
symptom(adhd, adhd_a2a, hyperactivity_impulsivity,
    'Often fidgets with or taps hands or feet or squirms in seat.').

symptom(adhd, adhd_a2b, hyperactivity_impulsivity,
    'Often leaves seat in situations when remaining seated is expected (e.g., leaves his or her place in the classroom, in the office or other workplace, or in other situations that require remaining in place).').

symptom(adhd, adhd_a2c, hyperactivity_impulsivity,
    'Often runs about or climbs in situations where it is inappropriate. (Note: In adolescents or adults, may be limited to feeling restless.)').

symptom(adhd, adhd_a2d, hyperactivity_impulsivity,
    'Often unable to play or engage in leisure activities quietly.').

symptom(adhd, adhd_a2e, hyperactivity_impulsivity,
    'Is often "on the go," acting as if "driven by a motor" (e.g., is unable to be or uncomfortable being still for extended time, as in restaurants, meetings; may be experienced by others as being restless or difficult to keep up with).').

symptom(adhd, adhd_a2f, hyperactivity_impulsivity,
    'Often talks excessively.').

% Impulsivity symptoms (A2g - A2i)
symptom(adhd, adhd_a2g, hyperactivity_impulsivity,
    'Often blurts out an answer before a question has been completed (e.g., completes people\'s sentences; cannot wait for turn in conversation).').

symptom(adhd, adhd_a2h, hyperactivity_impulsivity,
    'Often has difficulty waiting his or her turn (e.g., while waiting in line).').

symptom(adhd, adhd_a2i, hyperactivity_impulsivity,
    'Often interrupts or intrudes on others (e.g., butts into conversations, games, or activities; may start using other people\'s things without asking or receiving permission; for adolescents and adults, may intrude into or take over what others are doing).').

%% -----------------------------------------------------------------------------
%% Symptom Category Requirements
%% -----------------------------------------------------------------------------
%% Structure: symptom_category(DisorderID, CategoryID, SymptomList, RequiredCount, RequirementType)
%%
%% ADHD requires meeting A1 AND/OR A2:
%%   - Combined: Both A1 and A2 met
%%   - Predominantly Inattentive: A1 met, A2 not met
%%   - Predominantly Hyperactive/Impulsive: A2 met, A1 not met
%%
%% Base count = 6 (for children <17), age_adjusted lowers to 5 for adults (17+)
%% -----------------------------------------------------------------------------

% Inattention cluster (A1): 6/9 for children, 5/9 for adults
symptom_category(adhd, inattention_symptoms,
    [adhd_a1a, adhd_a1b, adhd_a1c, adhd_a1d, adhd_a1e, adhd_a1f, adhd_a1g, adhd_a1h, adhd_a1i],
    6, at_least).

% Hyperactivity-Impulsivity cluster (A2): 6/9 for children, 5/9 for adults
symptom_category(adhd, hyperactivity_impulsivity_symptoms,
    [adhd_a2a, adhd_a2b, adhd_a2c, adhd_a2d, adhd_a2e, adhd_a2f, adhd_a2g, adhd_a2h, adhd_a2i],
    6, at_least).

%% -----------------------------------------------------------------------------
%% Symptom Logic
%% -----------------------------------------------------------------------------
%% DSM-5 Criterion A: "inattention and/or hyperactivity-impulsivity"
%% ADHD allows EITHER symptom category to be met (OR logic), unlike most
%% disorders which require ALL categories (AND logic).
%% Three presentations result: Combined (both), Predominantly Inattentive (A1 only),
%% Predominantly Hyperactive/Impulsive (A2 only).
%% -----------------------------------------------------------------------------

symptom_logic(adhd, or_any).

%% -----------------------------------------------------------------------------
%% Age-Adjusted Requirements
%% -----------------------------------------------------------------------------
%% DSM-5 Note: "For older adolescents and adults (age 17 and older), at least
%% five symptoms are required."
%%
%% Schema logic: if age >= threshold, use adjusted count; otherwise use base count
%% Base count = 6 (children), age_adjusted to 5 for adults (17+)
%% -----------------------------------------------------------------------------

age_adjusted_count(adhd, inattention_symptoms, 17, 5).
age_adjusted_count(adhd, hyperactivity_impulsivity_symptoms, 17, 5).

%% -----------------------------------------------------------------------------
%% Duration Requirement
%% -----------------------------------------------------------------------------
%% DSM-5 Criterion A: "persisted for at least 6 months"
%% -----------------------------------------------------------------------------

duration_requirement(adhd, 6, months).

%% -----------------------------------------------------------------------------
%% Onset Requirement
%% -----------------------------------------------------------------------------
%% DSM-5 Criterion B: "Several inattentive or hyperactive-impulsive symptoms
%% were present prior to age 12 years."
%% -----------------------------------------------------------------------------

onset_requirement(adhd, before_age, 12).

%% -----------------------------------------------------------------------------
%% Setting Requirement
%% -----------------------------------------------------------------------------
%% DSM-5 Criterion C: "Several inattentive or hyperactive-impulsive symptoms
%% are present in two or more settings"
%% -----------------------------------------------------------------------------

setting_requirement(adhd, 2).

%% -----------------------------------------------------------------------------
%% Exclusion Criteria
%% -----------------------------------------------------------------------------
%% Structure: exclusion_criterion(DisorderID, ExclusionID, Type, Description)
%%
%% DSM-5 Criterion E exclusions
%% -----------------------------------------------------------------------------

% Criterion E: Not during psychotic disorder
exclusion_criterion(adhd, adhd_exc_psychotic, other_disorder,
    'The symptoms do not occur exclusively during the course of schizophrenia or another psychotic disorder.').

% Criterion E: Not better explained by other mental disorders
exclusion_criterion(adhd, adhd_exc_mood, other_disorder,
    'The symptoms are not better explained by a mood disorder.').

exclusion_criterion(adhd, adhd_exc_anxiety, other_disorder,
    'The symptoms are not better explained by an anxiety disorder.').

exclusion_criterion(adhd, adhd_exc_dissociative, other_disorder,
    'The symptoms are not better explained by a dissociative disorder.').

exclusion_criterion(adhd, adhd_exc_personality, other_disorder,
    'The symptoms are not better explained by a personality disorder.').

exclusion_criterion(adhd, adhd_exc_substance, substance,
    'The symptoms are not better explained by substance intoxication or withdrawal.').

%% -----------------------------------------------------------------------------
%% Subjective Criteria
%% -----------------------------------------------------------------------------
%% Structure: subjective_criterion(DisorderID, CriterionID, Description, AssessmentType)
%% -----------------------------------------------------------------------------

% Criterion D: Clinical significance / functional impairment
subjective_criterion(adhd, adhd_subj_functional_impairment,
    'There is clear evidence that the symptoms interfere with, or reduce the quality of, social, academic, or occupational functioning.',
    functional_impairment).

% Criterion A note: Symptoms inconsistent with developmental level
subjective_criterion(adhd, adhd_subj_developmental,
    'The symptoms are inconsistent with developmental level and negatively impact directly on social and academic/occupational activities.',
    clinical_significance).

% Criterion A note: Not solely oppositional behavior
subjective_criterion(adhd, adhd_subj_not_oppositional,
    'The symptoms are not solely a manifestation of oppositional behavior, defiance, hostility, or failure to understand tasks or instructions.',
    quality).

%% -----------------------------------------------------------------------------
%% Specifiers
%% -----------------------------------------------------------------------------
%% Structure: specifier(DisorderID, SpecifierType, Options, Description)
%% -----------------------------------------------------------------------------

specifier(adhd, presentation,
    [combined, predominantly_inattentive, predominantly_hyperactive_impulsive],
    'Combined (314.01/F90.2): Both A1 and A2 met for past 6 months. Predominantly Inattentive (314.00/F90.0): A1 met but not A2. Predominantly Hyperactive/Impulsive (314.01/F90.1): A2 met but not A1.').

specifier(adhd, remission_status,
    [in_partial_remission],
    'When full criteria were previously met, fewer than the full criteria have been met for the past 6 months, and the symptoms still result in impairment.').

specifier(adhd, severity,
    [mild, moderate, severe],
    'Mild: Few symptoms in excess of those required, minor impairments. Moderate: Between mild and severe. Severe: Many excess symptoms or several particularly severe symptoms, marked impairment.').

%% -----------------------------------------------------------------------------
%% Differential Diagnosis Features
%% -----------------------------------------------------------------------------
%% Structure: differential_feature(DisorderID, OtherDisorderID, FeatureID, Description)
%% -----------------------------------------------------------------------------

differential_feature(adhd, oppositional_defiant_disorder, adhd_diff_odd,
    'In ODD, resistance to tasks is due to negativity and defiance; in ADHD, it is due to difficulty sustaining effort, forgetting instructions, and impulsivity.').

differential_feature(adhd, intermittent_explosive_disorder, adhd_diff_ied,
    'IED shows serious aggression toward others not characteristic of ADHD; IED does not include sustained attention problems.').

differential_feature(adhd, autism_spectrum_disorder, adhd_diff_asd,
    'In ADHD, social dysfunction is due to impulsivity and peer rejection; in ASD, it is due to social disengagement and indifference to social cues.').

differential_feature(adhd, specific_learning_disorder, adhd_diff_sld,
    'In SLD, inattention is due to frustration with academic tasks and is not impairing outside academic work; ADHD inattention is pervasive.').

differential_feature(adhd, anxiety_disorders, adhd_diff_anxiety,
    'In anxiety disorders, inattention is due to worry and rumination; in ADHD, it is due to attraction to external stimuli or preoccupation with enjoyable activities.').

differential_feature(adhd, depressive_disorders, adhd_diff_depression,
    'In depression, poor concentration only occurs during depressive episodes; ADHD symptoms are persistent.').

differential_feature(adhd, bipolar_disorder, adhd_diff_bipolar,
    'In bipolar disorder, increased activity and impulsivity are episodic (lasting days); ADHD symptoms are persistent. Mood lability in ADHD occurs within the same day.').

differential_feature(adhd, personality_disorders, adhd_diff_personality,
    'ADHD is not characterized by fear of abandonment, self-injury, or extreme ambivalence seen in borderline PD; detailed history needed to distinguish impulsive behaviors.').

differential_feature(adhd, intellectual_disability, adhd_diff_id,
    'ADHD in intellectual disability requires that inattention or hyperactivity be excessive for mental age, not just chronological age.').
