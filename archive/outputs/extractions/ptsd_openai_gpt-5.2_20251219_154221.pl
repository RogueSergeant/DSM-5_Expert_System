%% =============================================================================
%% Posttraumatic Stress Disorder - Gold Standard
%% =============================================================================
%% DSM-5 Reference: PTSD 309.81 (F43.10)
%% Key features: Trauma exposure with intrusion, avoidance, negative cognitions/mood,
%%               and arousal/reactivity changes; duration > 1 month; distress/impairment.
%% Note: Core criteria below encode DSM-5 criteria for adults/adolescents/children > 6.
%%       Preschool (<=6 years) criteria symptoms are included as additional symptom facts
%%       but are not enforced via symptom_category/5 in this file.
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
%% Symptoms (Adults, adolescents, and children older than 6 years)
%% -----------------------------------------------------------------------------

%% Criterion A: Exposure (must meet >=1)
symptom(ptsd, ptsd_a1, exposure, 'Directly experiencing the traumatic event(s).').
symptom(ptsd, ptsd_a2, exposure, 'Witnessing, in person, the event(s) as it occurred to others.').
symptom(ptsd, ptsd_a3, exposure, 'Learning that the traumatic event(s) occurred to a close family member or close friend; if actual or threatened death, it must have been violent or accidental.').
symptom(ptsd, ptsd_a4, exposure, 'Experiencing repeated or extreme exposure to aversive details of the traumatic event(s) (work-related; not via media unless work related).').

%% Criterion B: Intrusion symptoms (>=1)
symptom(ptsd, ptsd_b1, intrusion, 'Recurrent, involuntary, and intrusive distressing memories of the traumatic event(s).').
symptom(ptsd, ptsd_b2, intrusion, 'Recurrent distressing dreams in which the content and/or affect of the dream are related to the traumatic event(s).').
symptom(ptsd, ptsd_b3, intrusion, 'Dissociative reactions (e.g., flashbacks) in which the individual feels or acts as if the traumatic event(s) were recurring.').
symptom(ptsd, ptsd_b4, intrusion, 'Intense or prolonged psychological distress at exposure to internal or external cues that symbolize or resemble an aspect of the traumatic event(s).').
symptom(ptsd, ptsd_b5, intrusion, 'Marked physiological reactions to internal or external cues that symbolize or resemble an aspect of the traumatic event(s).').

%% Criterion C: Avoidance (>=1)
symptom(ptsd, ptsd_c1, avoidance, 'Avoidance of or efforts to avoid distressing memories, thoughts, or feelings about or closely associated with the traumatic event(s).').
symptom(ptsd, ptsd_c2, avoidance, 'Avoidance of or efforts to avoid external reminders (people, places, conversations, activities, objects, situations) that arouse distressing memories, thoughts, or feelings about or closely associated with the traumatic event(s).').

%% Criterion D: Negative alterations in cognitions and mood (>=2)
symptom(ptsd, ptsd_d1, negative_cognitions_mood, 'Inability to remember an important aspect of the traumatic event(s) (typically due to dissociative amnesia, not head injury, alcohol, or drugs).').
symptom(ptsd, ptsd_d2, negative_cognitions_mood, 'Persistent and exaggerated negative beliefs or expectations about oneself, others, or the world.').
symptom(ptsd, ptsd_d3, negative_cognitions_mood, 'Persistent, distorted cognitions about the cause or consequences of the traumatic event(s) that lead the individual to blame self or others.').
symptom(ptsd, ptsd_d4, negative_cognitions_mood, 'Persistent negative emotional state (e.g., fear, horror, anger, guilt, or shame).').
symptom(ptsd, ptsd_d5, negative_cognitions_mood, 'Markedly diminished interest or participation in significant activities.').
symptom(ptsd, ptsd_d6, negative_cognitions_mood, 'Feelings of detachment or estrangement from others.').
symptom(ptsd, ptsd_d7, negative_cognitions_mood, 'Persistent inability to experience positive emotions (e.g., happiness, satisfaction, or loving feelings).').

%% Criterion E: Alterations in arousal and reactivity (>=2)
symptom(ptsd, ptsd_e1, arousal_reactivity, 'Irritable behavior and angry outbursts (with little or no provocation), typically expressed as verbal or physical aggression toward people or objects.').
symptom(ptsd, ptsd_e2, arousal_reactivity, 'Reckless or self-destructive behavior.').
symptom(ptsd, ptsd_e3, arousal_reactivity, 'Hypervigilance.').
symptom(ptsd, ptsd_e4, arousal_reactivity, 'Exaggerated startle response.').
symptom(ptsd, ptsd_e5, arousal_reactivity, 'Problems with concentration.').
symptom(ptsd, ptsd_e6, arousal_reactivity, 'Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep).').

%% -----------------------------------------------------------------------------
%% Additional Symptoms (Children 6 years and younger - Preschool variant)
%% Note: Included for completeness, but not enforced via symptom_category/5 here.
%% -----------------------------------------------------------------------------

%% Preschool Criterion A (exposure)
symptom(ptsd, ptsd_p_a1, preschool_exposure, 'Preschool: Directly experiencing the traumatic event(s).').
symptom(ptsd, ptsd_p_a2, preschool_exposure, 'Preschool: Witnessing, in person, the event(s) as it occurred to others, especially primary caregivers (not via electronic media).').
symptom(ptsd, ptsd_p_a3, preschool_exposure, 'Preschool: Learning that the traumatic event(s) occurred to a parent or caregiving figure.').

%% Preschool Criterion B (intrusion; >=1)
symptom(ptsd, ptsd_p_b1, preschool_intrusion, 'Preschool: Recurrent, involuntary, and intrusive distressing memories of the traumatic event(s) (may be expressed as play reenactment; may not appear distressing).').
symptom(ptsd, ptsd_p_b2, preschool_intrusion, 'Preschool: Recurrent distressing dreams related to the traumatic event(s) (may be difficult to ascertain).').
symptom(ptsd, ptsd_p_b3, preschool_intrusion, 'Preschool: Dissociative reactions (e.g., flashbacks) in which the child feels or acts as if the traumatic event(s) were recurring; trauma-specific reenactment may occur in play.').
symptom(ptsd, ptsd_p_b4, preschool_intrusion, 'Preschool: Intense or prolonged psychological distress at exposure to internal or external cues that symbolize or resemble an aspect of the traumatic event(s).').
symptom(ptsd, ptsd_p_b5, preschool_intrusion, 'Preschool: Marked physiological reactions to reminders of the traumatic event(s).').

%% Preschool Criterion C (avoidance or negative cognitions/mood combined; >=1)
symptom(ptsd, ptsd_p_c1, preschool_avoidance_negative, 'Preschool: Avoidance of or efforts to avoid activities, places, or physical reminders that arouse recollections of the traumatic event(s).').
symptom(ptsd, ptsd_p_c2, preschool_avoidance_negative, 'Preschool: Avoidance of or efforts to avoid people, conversations, or interpersonal situations that arouse recollections of the traumatic event(s).').
symptom(ptsd, ptsd_p_c3, preschool_avoidance_negative, 'Preschool: Substantially increased frequency of negative emotional states (e.g., fear, guilt, sadness, shame, confusion).').
symptom(ptsd, ptsd_p_c4, preschool_avoidance_negative, 'Preschool: Markedly diminished interest or participation in significant activities, including constriction of play.').
symptom(ptsd, ptsd_p_c5, preschool_avoidance_negative, 'Preschool: Socially withdrawn behavior.').
symptom(ptsd, ptsd_p_c6, preschool_avoidance_negative, 'Preschool: Persistent reduction in expression of positive emotions.').

%% Preschool Criterion D (arousal/reactivity; >=2)
symptom(ptsd, ptsd_p_d1, preschool_arousal_reactivity, 'Preschool: Irritable behavior and angry outbursts (with little or no provocation), expressed as verbal or physical aggression toward people or objects (including extreme temper tantrums).').
symptom(ptsd, ptsd_p_d2, preschool_arousal_reactivity, 'Preschool: Hypervigilance.').
symptom(ptsd, ptsd_p_d3, preschool_arousal_reactivity, 'Preschool: Exaggerated startle response.').
symptom(ptsd, ptsd_p_d4, preschool_arousal_reactivity, 'Preschool: Problems with concentration.').
symptom(ptsd, ptsd_p_d5, preschool_arousal_reactivity, 'Preschool: Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep).').

%% -----------------------------------------------------------------------------
%% Symptom Categories (Adults/adolescents/children > 6 years)
%% -----------------------------------------------------------------------------
symptom_category(ptsd, trauma_exposure,
    [ptsd_a1, ptsd_a2, ptsd_a3, ptsd_a4],
    1, at_least_one_of).

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
%% Exclusions
%% -----------------------------------------------------------------------------
exclusion_criterion(ptsd, ptsd_exc_substance, substance,
    'The disturbance is not attributable to the physiological effects of a substance (e.g., medication, alcohol).').
exclusion_criterion(ptsd, ptsd_exc_medical, medical,
    'The disturbance is not attributable to another medical condition.').

%% -----------------------------------------------------------------------------
%% Subjective Criteria
%% -----------------------------------------------------------------------------
subjective_criterion(ptsd, ptsd_subj_clin_sig,
    'The disturbance causes clinically significant distress or impairment in social, occupational, or other important areas of functioning.',
    clinical_significance).

%% -----------------------------------------------------------------------------
%% Specifiers
%% -----------------------------------------------------------------------------
specifier(ptsd, dissociative_symptoms,
    [none, depersonalization, derealization, both],
    'With dissociative symptoms specifier: in response to the stressor, persistent/recurrent depersonalization and/or derealization; dissociative symptoms must not be attributable to substance effects or another medical condition.').

specifier(ptsd, delayed_expression,
    [present, absent],
    'With delayed expression specifier: full diagnostic criteria are not met until at least 6 months after the event (although some symptoms may be immediate).').

specifier(ptsd, developmental_variant,
    [older_than_6, six_or_younger],
    'Developmental variant: DSM-5 provides separate criteria for children 6 years and younger (preschool subtype).').

%% -----------------------------------------------------------------------------
%% Differential Features
%% -----------------------------------------------------------------------------
differential_feature(ptsd, adjustment_disorder, ptsd_diff_adjustment_01,
    'Adjustment disorders may follow stressors of any severity; PTSD requires Criterion A trauma exposure, and adjustment disorder is used when PTSD criteria are not met or when symptoms occur after a non-Criterion A stressor.').
differential_feature(ptsd, acute_stress_disorder, ptsd_diff_asd_01,
    'Acute stress disorder has similar symptom patterns but is limited to 3 days to 1 month following trauma exposure; PTSD requires duration > 1 month.').
differential_feature(ptsd, obsessive_compulsive_disorder, ptsd_diff_ocd_01,
    'In OCD, intrusive thoughts are obsessions not necessarily linked to a traumatic event, and compulsions are typically present; PTSD intrusions are trauma-related and accompanied by other PTSD clusters.').
differential_feature(ptsd, panic_disorder, ptsd_diff_panic_01,
    'Panic disorder arousal/dissociative symptoms are not specifically tied to a traumatic event; PTSD symptoms are linked to trauma reminders and include re-experiencing and avoidance.').
differential_feature(ptsd, generalized_anxiety_disorder, ptsd_diff_gad_01,
    'Generalized anxiety disorder involves pervasive worry not tied to a specific traumatic event; PTSD involves trauma-linked intrusions, avoidance, and negative cognitions/mood changes.').
differential_feature(ptsd, separation_anxiety_disorder, ptsd_diff_sep_01,
    'Separation anxiety disorder symptoms are centered on separation from attachment figures; PTSD symptoms follow trauma exposure and include trauma-specific intrusion and avoidance.').
differential_feature(ptsd, major_depressive_disorder, ptsd_diff_mdd_01,
    'Major depressive disorder may be preceded by trauma but does not include PTSD intrusion (Criterion B) or avoidance (Criterion C); PTSD requires these clusters.').
differential_feature(ptsd, personality_disorder, ptsd_diff_pd_01,
    'Interpersonal difficulties that begin or markedly worsen after trauma exposure may indicate PTSD; personality disorders generally show enduring patterns independent of a traumatic exposure.').
differential_feature(ptsd, dissociative_disorders, ptsd_diff_dissoc_01,
    'Dissociative disorders may occur with or without trauma; if full PTSD criteria are met alongside depersonalization/derealization, consider PTSD with dissociative symptoms specifier.').
differential_feature(ptsd, conversion_disorder, ptsd_diff_conversion_01,
    'New somatic/neurologic symptoms in the context of posttraumatic distress may reflect PTSD rather than conversion disorder; PTSD requires trauma-linked symptom clusters.').
differential_feature(ptsd, psychotic_disorders, ptsd_diff_psychotic_01,
    'PTSD flashbacks must be distinguished from hallucinations/psychosis in primary psychotic disorders or mood disorders with psychotic features; PTSD perceptual experiences are trauma-related and typically occur as dissociative re-experiencing.').
differential_feature(ptsd, neurocognitive_disorder_due_to_tbi, ptsd_diff_tbi_01,
    'PTSD is characterized by re-experiencing and avoidance; TBI-related neurocognitive disorder is more specifically associated with persistent disorientation and confusion, though overlap can occur.').