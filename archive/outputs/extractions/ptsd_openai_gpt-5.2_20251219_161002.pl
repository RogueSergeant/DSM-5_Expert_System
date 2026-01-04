%% =============================================================================
%% Posttraumatic Stress Disorder (PTSD) - Gold Standard
%% =============================================================================
%% DSM-5 Reference: Posttraumatic Stress Disorder 309.81 (F43.10)
%% Applies to: adults, adolescents, and children older than 6 years
%% Key features: Trauma exposure with intrusion, avoidance, negative cognitions/mood,
%%               and arousal/reactivity symptoms lasting > 1 month with distress/impairment.
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

%% -----------------------------------------------------------------------------
%% Disorder Definition
%% -----------------------------------------------------------------------------

disorder(ptsd, 'Posttraumatic Stress Disorder', trauma_stressor_related).

%% -----------------------------------------------------------------------------
%% Symptoms (DSM-5 Criteria A-E)
%% -----------------------------------------------------------------------------

%% Criterion A: Exposure to actual or threatened death, serious injury, or sexual violence
symptom(ptsd, ptsd_a1, exposure, 'Directly experiencing the traumatic event(s).').
symptom(ptsd, ptsd_a2, exposure, 'Witnessing, in person, the traumatic event(s) as it occurred to others.').
symptom(ptsd, ptsd_a3, exposure, 'Learning that the traumatic event(s) occurred to a close family member or close friend; if actual or threatened death, it must have been violent or accidental.').
symptom(ptsd, ptsd_a4, exposure, 'Experiencing repeated or extreme exposure to aversive details of the traumatic event(s) (not through media unless work-related).').

%% Criterion B: Intrusion symptoms (>= 1)
symptom(ptsd, ptsd_b1, intrusion, 'Recurrent, involuntary, and intrusive distressing memories of the traumatic event(s).').
symptom(ptsd, ptsd_b2, intrusion, 'Recurrent distressing dreams in which the content and/or affect are related to the traumatic event(s).').
symptom(ptsd, ptsd_b3, intrusion, 'Dissociative reactions (e.g., flashbacks) in which the individual feels or acts as if the traumatic event(s) were recurring.').
symptom(ptsd, ptsd_b4, intrusion, 'Intense or prolonged psychological distress at exposure to internal or external cues that symbolize or resemble an aspect of the traumatic event(s).').
symptom(ptsd, ptsd_b5, intrusion, 'Marked physiological reactions to internal or external cues that symbolize or resemble an aspect of the traumatic event(s).').

%% Criterion C: Avoidance (>= 1)
symptom(ptsd, ptsd_c1, avoidance, 'Avoidance of or efforts to avoid distressing memories, thoughts, or feelings about or closely associated with the traumatic event(s).').
symptom(ptsd, ptsd_c2, avoidance, 'Avoidance of or efforts to avoid external reminders (people, places, conversations, activities, objects, situations) that arouse distressing memories, thoughts, or feelings closely associated with the traumatic event(s).').

%% Criterion D: Negative alterations in cognitions and mood (>= 2)
symptom(ptsd, ptsd_d1, negative_cognitions_mood, 'Inability to remember an important aspect of the traumatic event(s) (typically due to dissociative amnesia, not head injury, alcohol, or drugs).').
symptom(ptsd, ptsd_d2, negative_cognitions_mood, 'Persistent and exaggerated negative beliefs or expectations about oneself, others, or the world.').
symptom(ptsd, ptsd_d3, negative_cognitions_mood, 'Persistent, distorted cognitions about the cause or consequences of the traumatic event(s) that lead the individual to blame self or others.').
symptom(ptsd, ptsd_d4, negative_cognitions_mood, 'Persistent negative emotional state (e.g., fear, horror, anger, guilt, shame).').
symptom(ptsd, ptsd_d5, negative_cognitions_mood, 'Markedly diminished interest or participation in significant activities.').
symptom(ptsd, ptsd_d6, negative_cognitions_mood, 'Feelings of detachment or estrangement from others.').
symptom(ptsd, ptsd_d7, negative_cognitions_mood, 'Persistent inability to experience positive emotions (e.g., happiness, satisfaction, loving feelings).').

%% Criterion E: Marked alterations in arousal and reactivity (>= 2)
symptom(ptsd, ptsd_e1, arousal_reactivity, 'Irritable behavior and angry outbursts (with little or no provocation), typically expressed as verbal or physical aggression toward people or objects.').
symptom(ptsd, ptsd_e2, arousal_reactivity, 'Reckless or self-destructive behavior.').
symptom(ptsd, ptsd_e3, arousal_reactivity, 'Hypervigilance.').
symptom(ptsd, ptsd_e4, arousal_reactivity, 'Exaggerated startle response.').
symptom(ptsd, ptsd_e5, arousal_reactivity, 'Problems with concentration.').
symptom(ptsd, ptsd_e6, arousal_reactivity, 'Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep).').

%% Specifier-related symptoms (not part of core symptom counting categories)
symptom(ptsd, ptsd_spec_dep, dissociative, 'Depersonalization: persistent or recurrent experiences of feeling detached from, and as if one were an outside observer of, one''s mental processes or body.').
symptom(ptsd, ptsd_spec_der, dissociative, 'Derealization: persistent or recurrent experiences of unreality of surroundings (world experienced as unreal, dreamlike, distant, distorted).').

%% -----------------------------------------------------------------------------
%% Symptom Categories (DSM-5 Required Counts)
%% -----------------------------------------------------------------------------

symptom_category(ptsd, exposure_criterion,
    [ptsd_a1, ptsd_a2, ptsd_a3, ptsd_a4],
    1, at_least).

symptom_category(ptsd, intrusion_symptoms,
    [ptsd_b1, ptsd_b2, ptsd_b3, ptsd_b4, ptsd_b5],
    1, at_least).

symptom_category(ptsd, avoidance_symptoms,
    [ptsd_c1, ptsd_c2],
    1, at_least).

symptom_category(ptsd, negative_cognitions_mood_symptoms,
    [ptsd_d1, ptsd_d2, ptsd_d3, ptsd_d4, ptsd_d5, ptsd_d6, ptsd_d7],
    2, at_least).

symptom_category(ptsd, arousal_reactivity_symptoms,
    [ptsd_e1, ptsd_e2, ptsd_e3, ptsd_e4, ptsd_e5, ptsd_e6],
    2, at_least).

%% -----------------------------------------------------------------------------
%% Duration / Onset
%% -----------------------------------------------------------------------------

duration_requirement(ptsd, 1, months).
onset_requirement(ptsd, after_event, trauma).

%% -----------------------------------------------------------------------------
%% Exclusion Criteria (Criterion H)
%% -----------------------------------------------------------------------------

exclusion_criterion(ptsd, ptsd_exc_substance, substance,
    'The disturbance is not attributable to the physiological effects of a substance (e.g., medication, alcohol).').

exclusion_criterion(ptsd, ptsd_exc_medical, medical,
    'The disturbance is not attributable to another medical condition.').

%% -----------------------------------------------------------------------------
%% Subjective / Clinical Significance (Criterion G)
%% -----------------------------------------------------------------------------

subjective_criterion(ptsd, ptsd_subj_clin_sig,
    'The disturbance causes clinically significant distress or impairment in social, occupational, or other important areas of functioning.',
    clinical_significance).

%% -----------------------------------------------------------------------------
%% Specifiers
%% -----------------------------------------------------------------------------

specifier(ptsd, dissociative_symptoms,
    [with_dissociative_symptoms, without_dissociative_symptoms],
    'With dissociative symptoms: in addition to meeting PTSD criteria, the individual experiences persistent or recurrent depersonalization and/or derealization in response to the stressor; to use this specifier, dissociative symptoms must not be attributable to a substance or another medical condition.').

specifier(ptsd, delayed_expression,
    [with_delayed_expression, without_delayed_expression],
    'With delayed expression: full diagnostic criteria are not met until at least 6 months after the event (although onset and expression of some symptoms may be immediate).').

%% -----------------------------------------------------------------------------
%% Differential Diagnosis (selected DSM-5 differential features)
%% -----------------------------------------------------------------------------

differential_feature(ptsd, adjustment_disorder, ptsd_diff_adj_01,
    'Adjustment disorders can follow stressors of any severity; PTSD requires exposure meeting Criterion A, and is used when the full PTSD criteria are met after such exposure.').

differential_feature(ptsd, acute_stress_disorder, ptsd_diff_asd_01,
    'Acute stress disorder is limited to symptom duration from 3 days to 1 month after trauma exposure; PTSD requires duration greater than 1 month.').

differential_feature(ptsd, obsessive_compulsive_disorder, ptsd_diff_ocd_01,
    'In OCD, intrusive thoughts are obsessions not necessarily tied to a traumatic event and are often accompanied by compulsions; PTSD intrusions are trauma-related and accompanied by avoidance and trauma-related negative mood/cognition and arousal changes.').

differential_feature(ptsd, panic_disorder, ptsd_diff_panic_01,
    'Panic disorder arousal symptoms are not necessarily linked to a specific traumatic event; in PTSD, distress and arousal are typically triggered by trauma reminders and occur with intrusion and avoidance symptoms.').

differential_feature(ptsd, generalized_anxiety_disorder, ptsd_diff_gad_01,
    'Generalized anxiety disorder involves pervasive worry not anchored to a specific traumatic event; PTSD symptoms are defined by trauma exposure with re-experiencing and avoidance of trauma reminders.').

differential_feature(ptsd, separation_anxiety_disorder, ptsd_diff_sad_01,
    'Separation anxiety disorder symptoms are centered on separation from attachment figures; PTSD symptoms are centered on re-experiencing and avoidance related to a traumatic event.').

differential_feature(ptsd, major_depressive_disorder, ptsd_diff_mdd_01,
    'Major depressive disorder does not include PTSD intrusion (Criterion B) or avoidance (Criterion C); PTSD requires trauma-related re-experiencing and avoidance alongside negative mood/cognition and arousal changes.').

differential_feature(ptsd, dissociative_amnesia, ptsd_diff_da_01,
    'Dissociative amnesia may occur with or without PTSD; if full PTSD criteria are met, PTSD is diagnosed and dissociative amnesia may be comorbid or reflected in Criterion D1 when trauma-related.').

differential_feature(ptsd, depersonalization_derealization_disorder, ptsd_diff_dpdr_01,
    'Depersonalization/derealization symptoms may occur in multiple disorders; when full PTSD criteria are met, consider the PTSD "with dissociative symptoms" specifier rather than a standalone depersonalization-derealization disorder.').

differential_feature(ptsd, conversion_disorder, ptsd_diff_conv_01,
    'New onset somatic/neurological symptoms in the context of posttraumatic distress may indicate PTSD rather than conversion disorder; PTSD is distinguished by trauma-related intrusion and avoidance symptoms.').

differential_feature(ptsd, schizophrenia_spectrum_psychotic, ptsd_diff_psych_01,
    'PTSD flashbacks and dissociative re-experiencing must be distinguished from hallucinations and other psychotic perceptual disturbances; PTSD re-experiencing is trauma-linked and typically episodic, often triggered by reminders.').

differential_feature(ptsd, neurocognitive_disorder_due_to_tbi, ptsd_diff_tbi_01,
    'PTSD and traumatic brain injury can co-occur; re-experiencing and avoidance are characteristic of PTSD, whereas persistent disorientation and confusion are more specific to neurocognitive effects of TBI.').