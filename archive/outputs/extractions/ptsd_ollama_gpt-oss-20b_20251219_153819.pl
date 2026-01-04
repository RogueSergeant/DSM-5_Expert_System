%% =============================================================================
%% Posttraumatic Stress Disorder - Gold Standard
%% DSM-5 Reference: pp. 309.81
%% Key features: Exposure to traumatic event followed by intrusion, avoidance, negative alterations, and arousal symptoms lasting >1 month.
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

%% Disorder Definition
disorder(ptsd, 'Posttraumatic Stress Disorder', trauma_stressor_related).

%% Symptoms
symptom(ptsd, ptsd_b1, intrusion, 'Recurrent, involuntary, and intrusive distressing memories of the traumatic event(s).').
symptom(ptsd, ptsd_b2, intrusion, 'Recurrent distressing dreams in which the content and/or affect of the dream are related to the traumatic event(s).').
symptom(ptsd, ptsd_b3, intrusion, 'Dissociative reactions (e.g., flashbacks) in which the individual feels or acts as if the traumatic event(s) were recurring.').
symptom(ptsd, ptsd_b4, intrusion, 'Intense or prolonged psychological distress at exposure to internal or external cues that symbolize or resemble an aspect of the traumatic event(s).').
symptom(ptsd, ptsd_b5, intrusion, 'Marked physiological reactions to internal or external cues that symbolize or resemble an aspect of the traumatic event(s).').
symptom(ptsd, ptsd_c1, avoidance, 'Avoidance of or efforts to avoid distressing memories, thoughts, or feelings about or closely associated with the traumatic event(s).').
symptom(ptsd, ptsd_c2, avoidance, 'Avoidance of or efforts to avoid external reminders (people, places, conversations, activities, objects, situations) that arouse distressing memories, thoughts, or feelings about or closely associated with the traumatic event(s).').
symptom(ptsd, ptsd_d1, negative_alterations, 'Inability to remember an important aspect of the traumatic event(s) (typically due to dissociative amnesia).').
symptom(ptsd, ptsd_d2, negative_alterations, 'Persistent and exaggerated negative beliefs or expectations about oneself, others, or the world.').
symptom(ptsd, ptsd_d3, negative_alterations, 'Persistent, distorted cognitions about the cause or consequences of the traumatic event(s) that lead the individual to blame himself/herself or others.').
symptom(ptsd, ptsd_d4, negative_alterations, 'Persistent negative emotional state (e.g., fear, horror, anger, guilt, or shame).').
symptom(ptsd, ptsd_d5, negative_alterations, 'Markedly diminished interest or participation in significant activities.').
symptom(ptsd, ptsd_d6, negative_alterations, 'Feelings of detachment or estrangement from others.').
symptom(ptsd, ptsd_d7, negative_alterations, 'Persistent inability to experience positive emotions (e.g., happiness, satisfaction, or loving feelings).').
symptom(ptsd, ptsd_e1, arousal, 'Irritable behavior and angry outbursts (with little or no provocation).').
symptom(ptsd, ptsd_e2, arousal, 'Reckless or self-destructive behavior.').
symptom(ptsd, ptsd_e3, arousal, 'Hypervigilance.').
symptom(ptsd, ptsd_e4, arousal, 'Exaggerated startle response.').
symptom(ptsd, ptsd_e5, arousal, 'Problems with concentration.').
symptom(ptsd, ptsd_e6, arousal, 'Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep).').

%% Symptom Categories
symptom_category(ptsd, intrusion_symptoms,
    [ptsd_b1, ptsd_b2, ptsd_b3, ptsd_b4, ptsd_b5],
    1, at_least).
symptom_category(ptsd, avoidance_symptoms,
    [ptsd_c1, ptsd_c2],
    1, at_least).
symptom_category(ptsd, negative_alterations_symptoms,
    [ptsd_d1, ptsd_d2, ptsd_d3, ptsd_d4, ptsd_d5, ptsd_d6, ptsd_d7],
    2, at_least).
symptom_category(ptsd, arousal_symptoms,
    [ptsd_e1, ptsd_e2, ptsd_e3, ptsd_e4, ptsd_e5, ptsd_e6],
    2, at_least).

%% Duration Requirement
duration_requirement(ptsd, 1, months).

%% Onset Requirement
onset_requirement(ptsd, after_event, trauma).

%% Exclusion Criteria
exclusion_criterion(ptsd, ptsd_exc_substance, substance,
    'The disturbance is not attributable to the physiological effects of a substance (e.g., medication, alcohol).').
exclusion_criterion(ptsd, ptsd_exc_medical, medical,
    'The disturbance is not attributable to another medical condition.').

%% Subjective Criterion
subjective_criterion(ptsd, ptsd_subj_distress,
    'Symptoms cause clinically significant distress or impairment in social, occupational, or other important areas of functioning.',
    clinical_significance).

%% Specifiers
specifier(ptsd, severity, [mild, moderate, severe],
    'Current severity level of PTSD symptoms.').
specifier(ptsd, dissociative_subtype, [present, absent],
    'Presence of dissociative symptoms (depersonalization or derealization).').
specifier(ptsd, delayed_expression, [present, absent],
    'Symptoms meet full criteria only after at least 6 months post-trauma.').

%% Differential Features
differential_feature(ptsd, mdd, ptsd_diff_mdd,
    'PTSD: intrusive memories and avoidance are primary; MDD: depressed mood is primary.').
differential_feature(ptsd, gad, ptsd_diff_gad,
    'PTSD: intrusive memories and hyperarousal; GAD: generalized worry and excessive anxiety.').