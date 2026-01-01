%% =============================================================================
%% Posttraumatic Stress Disorder (PTSD) - Gold Standard
%% =============================================================================
%% DSM-5 Reference: pp. 271-280 (Diagnostic Criteria 309.81, F43.10)
%%
%% This file defines TWO separate disorders:
%%   1. ptsd: For adults, adolescents, and children >6 years
%%   2. ptsd_preschool: For children ≤6 years (with developmentally adapted criteria)
%%
%% The clinician selects which disorder to evaluate based on patient age.
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

%% =============================================================================
%% DISORDER 1: PTSD (Adults/Adolescents/Children >6 years)
%% =============================================================================

disorder(ptsd, 'Posttraumatic Stress Disorder', trauma_stressor_related).

%% Criterion A: Trauma Exposure
symptom(ptsd, ptsd_a1, trauma_exposure, 'Directly experiencing the traumatic event(s)').
symptom(ptsd, ptsd_a2, trauma_exposure, 'Witnessing, in person, the event(s) as it occurred to others').
symptom(ptsd, ptsd_a3, trauma_exposure, 'Learning that the traumatic event(s) occurred to a close family member or close friend (violent or accidental death)').
symptom(ptsd, ptsd_a4, trauma_exposure, 'Experiencing repeated or extreme exposure to aversive details of the traumatic event(s) (e.g., first responders; not through media unless work-related)').

%% Criterion B: Intrusion Symptoms
symptom(ptsd, ptsd_b1, intrusion, 'Recurrent, involuntary, and intrusive distressing memories of the traumatic event(s)').
symptom(ptsd, ptsd_b2, intrusion, 'Recurrent distressing dreams in which the content and/or affect of the dream are related to the traumatic event(s)').
symptom(ptsd, ptsd_b3, intrusion, 'Dissociative reactions (e.g., flashbacks) in which the individual feels or acts as if the traumatic event(s) were recurring').
symptom(ptsd, ptsd_b4, intrusion, 'Intense or prolonged psychological distress at exposure to internal or external cues that symbolize or resemble an aspect of the traumatic event(s)').
symptom(ptsd, ptsd_b5, intrusion, 'Marked physiological reactions to internal or external cues that symbolize or resemble an aspect of the traumatic event(s)').

%% Criterion C: Avoidance Symptoms
symptom(ptsd, ptsd_c1, avoidance, 'Avoidance of or efforts to avoid distressing memories, thoughts, or feelings about or closely associated with the traumatic event(s)').
symptom(ptsd, ptsd_c2, avoidance, 'Avoidance of or efforts to avoid external reminders (people, places, conversations, activities, objects, situations) that arouse distressing memories, thoughts, or feelings about the traumatic event(s)').

%% Criterion D: Negative Alterations in Cognitions/Mood
symptom(ptsd, ptsd_d1, negative_cognition_mood, 'Inability to remember an important aspect of the traumatic event(s) (typically due to dissociative amnesia)').
symptom(ptsd, ptsd_d2, negative_cognition_mood, 'Persistent and exaggerated negative beliefs or expectations about oneself, others, or the world').
symptom(ptsd, ptsd_d3, negative_cognition_mood, 'Persistent, distorted cognitions about the cause or consequences of the traumatic event(s) that lead the individual to blame himself/herself or others').
symptom(ptsd, ptsd_d4, negative_cognition_mood, 'Persistent negative emotional state (e.g., fear, horror, anger, guilt, or shame)').
symptom(ptsd, ptsd_d5, negative_cognition_mood, 'Markedly diminished interest or participation in significant activities').
symptom(ptsd, ptsd_d6, negative_cognition_mood, 'Feelings of detachment or estrangement from others').
symptom(ptsd, ptsd_d7, negative_cognition_mood, 'Persistent inability to experience positive emotions (e.g., happiness, satisfaction, or loving feelings)').

%% Criterion E: Alterations in Arousal/Reactivity
symptom(ptsd, ptsd_e1, arousal_reactivity, 'Irritable behavior and angry outbursts (with little or no provocation) typically expressed as verbal or physical aggression toward people or objects').
symptom(ptsd, ptsd_e2, arousal_reactivity, 'Reckless or self-destructive behavior').
symptom(ptsd, ptsd_e3, arousal_reactivity, 'Hypervigilance').
symptom(ptsd, ptsd_e4, arousal_reactivity, 'Exaggerated startle response').
symptom(ptsd, ptsd_e5, arousal_reactivity, 'Problems with concentration').
symptom(ptsd, ptsd_e6, arousal_reactivity, 'Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep)').

%% Symptom Categories
symptom_category(ptsd, trauma_exposure,
    [ptsd_a1, ptsd_a2, ptsd_a3, ptsd_a4],
    1, at_least).

symptom_category(ptsd, intrusion_symptoms,
    [ptsd_b1, ptsd_b2, ptsd_b3, ptsd_b4, ptsd_b5],
    1, at_least).

symptom_category(ptsd, avoidance_symptoms,
    [ptsd_c1, ptsd_c2],
    1, at_least).

symptom_category(ptsd, negative_cognition_mood_symptoms,
    [ptsd_d1, ptsd_d2, ptsd_d3, ptsd_d4, ptsd_d5, ptsd_d6, ptsd_d7],
    2, at_least).

symptom_category(ptsd, arousal_reactivity_symptoms,
    [ptsd_e1, ptsd_e2, ptsd_e3, ptsd_e4, ptsd_e5, ptsd_e6],
    2, at_least).

%% Duration Requirement
duration_requirement(ptsd, 1, months).

%% Onset Requirement
onset_requirement(ptsd, after_event, trauma).

%% Exclusion Criteria
exclusion_criterion(ptsd, ptsd_exc_substance, substance,
    'The disturbance is not attributable to the physiological effects of a substance (e.g., medication, alcohol)').

exclusion_criterion(ptsd, ptsd_exc_medical, medical,
    'The disturbance is not attributable to another medical condition').

exclusion_criterion(ptsd, ptsd_exc_dissociative_substance, substance,
    'Dissociative symptoms (if present) must not be attributable to the physiological effects of a substance (e.g., blackouts, behavior during alcohol intoxication)').

exclusion_criterion(ptsd, ptsd_exc_dissociative_medical, medical,
    'Dissociative symptoms (if present) must not be attributable to another medical condition (e.g., complex partial seizures)').

%% Subjective Criteria
subjective_criterion(ptsd, ptsd_subj_distress,
    'The disturbance causes clinically significant distress or impairment in social, occupational, or other important areas of functioning',
    clinical_significance).

subjective_criterion(ptsd, ptsd_subj_trauma_quality,
    'Exposure involves actual or threatened death, serious injury, or sexual violence',
    quality).

subjective_criterion(ptsd, ptsd_subj_violent_accidental,
    'For learning about events (Criterion A3), the event(s) must have been violent or accidental',
    quality).

%% Specifiers
specifier(ptsd, dissociative_symptoms, [present, absent],
    'With dissociative symptoms: persistent or recurrent depersonalization or derealization').

specifier(ptsd, dissociative_type, [depersonalization, derealization, both],
    'Type of dissociative symptoms: depersonalization (feeling detached from oneself), derealization (unreality of surroundings), or both').

specifier(ptsd, delayed_expression, [present, absent],
    'With delayed expression: full diagnostic criteria not met until at least 6 months after the event (although onset and expression of some symptoms may be immediate)').

%% Differential Features
differential_feature(ptsd, acute_stress_disorder, ptsd_diff_asd,
    'Acute stress disorder: symptom duration 3 days to 1 month; PTSD: duration >1 month').

differential_feature(ptsd, adjustment_disorder, ptsd_diff_adjustment,
    'Adjustment disorder: stressor can be of any severity; PTSD: requires Criterion A trauma exposure').

differential_feature(ptsd, ocd, ptsd_diff_ocd,
    'OCD: intrusive thoughts meet definition of obsession, not related to traumatic event, compulsions present; PTSD: intrusions are trauma-related memories').

differential_feature(ptsd, panic_disorder, ptsd_diff_panic,
    'Panic disorder: arousal/dissociative symptoms not associated with specific traumatic event; PTSD: symptoms linked to trauma').

differential_feature(ptsd, gad, ptsd_diff_gad,
    'GAD: worry/anxiety not related to specific traumatic event; PTSD: symptoms follow trauma exposure').

differential_feature(ptsd, mdd, ptsd_diff_mdd,
    'MDD: may or may not follow traumatic event, lacks PTSD Criteria B (intrusion) and C (avoidance); PTSD: requires trauma exposure and specific symptom clusters').

differential_feature(ptsd, dissociative_amnesia, ptsd_diff_dissociative_amnesia,
    'Dissociative amnesia: may lack other PTSD symptoms; if full PTSD criteria met, use PTSD with dissociative symptoms specifier').

differential_feature(ptsd, dissociative_identity_disorder, ptsd_diff_did,
    'Dissociative identity disorder: may have co-occurring PTSD; if full criteria met, use PTSD with dissociative symptoms specifier').

differential_feature(ptsd, traumatic_brain_injury, ptsd_diff_tbi,
    'TBI: persistent disorientation and confusion more specific to TBI; reexperiencing and avoidance characteristic of PTSD; symptoms can co-occur').

differential_feature(ptsd, schizophrenia, ptsd_diff_schizophrenia,
    'Schizophrenia: flashbacks must be distinguished from hallucinations; PTSD flashbacks are memory-based reexperiencing').

differential_feature(ptsd, personality_disorder, ptsd_diff_personality,
    'Personality disorder: interpersonal difficulties independent of trauma; PTSD: difficulties had onset or exacerbation after trauma').

differential_feature(ptsd, separation_anxiety_disorder, ptsd_diff_separation_anxiety,
    'Separation anxiety: symptoms related to separation from home/family; PTSD: symptoms related to traumatic event').

%% =============================================================================
%% DISORDER 2: PTSD Preschool Subtype (Children ≤6 years)
%% =============================================================================

disorder(ptsd_preschool, 'Posttraumatic Stress Disorder (Preschool Subtype)', trauma_stressor_related).

%% Criterion A: Trauma Exposure (Children ≤6 years)
symptom(ptsd_preschool, ptsd_a1_child, trauma_exposure_child, 'Directly experiencing the traumatic event(s) (child ≤6 years)').
symptom(ptsd_preschool, ptsd_a2_child, trauma_exposure_child, 'Witnessing, in person, the event(s) as it occurred to others, especially primary caregivers (child ≤6 years; not electronic media)').
symptom(ptsd_preschool, ptsd_a3_child, trauma_exposure_child, 'Learning that the traumatic event(s) occurred to a parent or caregiving figure (child ≤6 years)').

%% Criterion B: Intrusion Symptoms (Children ≤6 years)
symptom(ptsd_preschool, ptsd_b1_child, intrusion_child, 'Recurrent, involuntary, and intrusive distressing memories of the traumatic event(s) (may appear as play reenactment in children ≤6)').
symptom(ptsd_preschool, ptsd_b2_child, intrusion_child, 'Recurrent distressing dreams related to the traumatic event(s) (may not be possible to ascertain frightening content is trauma-related in children ≤6)').
symptom(ptsd_preschool, ptsd_b3_child, intrusion_child, 'Dissociative reactions in which the child feels or acts as if the traumatic event(s) were recurring (trauma-specific reenactment may occur in play)').
symptom(ptsd_preschool, ptsd_b4_child, intrusion_child, 'Intense or prolonged psychological distress at exposure to internal or external cues that symbolize or resemble an aspect of the traumatic event(s) (child ≤6)').
symptom(ptsd_preschool, ptsd_b5_child, intrusion_child, 'Marked physiological reactions to reminders of the traumatic event(s) (child ≤6)').

%% Criterion C: Avoidance/Negative Alterations (Children ≤6 years)
%% Note: For children ≤6, C and D criteria are combined into one category
symptom(ptsd_preschool, ptsd_c1_child, avoidance_cognition_child, 'Avoidance of or efforts to avoid activities, places, or physical reminders that arouse recollections of the traumatic event(s) (child ≤6)').
symptom(ptsd_preschool, ptsd_c2_child, avoidance_cognition_child, 'Avoidance of or efforts to avoid people, conversations, or interpersonal situations that arouse recollections of the traumatic event(s) (child ≤6)').
symptom(ptsd_preschool, ptsd_c3_child, avoidance_cognition_child, 'Substantially increased frequency of negative emotional states (e.g., fear, guilt, sadness, shame, confusion) (child ≤6)').
symptom(ptsd_preschool, ptsd_c4_child, avoidance_cognition_child, 'Markedly diminished interest or participation in significant activities, including constriction of play (child ≤6)').
symptom(ptsd_preschool, ptsd_c5_child, avoidance_cognition_child, 'Socially withdrawn behavior (child ≤6)').
symptom(ptsd_preschool, ptsd_c6_child, avoidance_cognition_child, 'Persistent reduction in expression of positive emotions (child ≤6)').

%% Criterion D: Alterations in Arousal/Reactivity (Children ≤6 years)
symptom(ptsd_preschool, ptsd_d1_child, arousal_reactivity_child, 'Irritable behavior and angry outbursts (with little or no provocation) typically expressed as verbal or physical aggression toward people or objects, including extreme temper tantrums (child ≤6)').
symptom(ptsd_preschool, ptsd_d2_child, arousal_reactivity_child, 'Hypervigilance (child ≤6)').
symptom(ptsd_preschool, ptsd_d3_child, arousal_reactivity_child, 'Exaggerated startle response (child ≤6)').
symptom(ptsd_preschool, ptsd_d4_child, arousal_reactivity_child, 'Problems with concentration (child ≤6)').
symptom(ptsd_preschool, ptsd_d5_child, arousal_reactivity_child, 'Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep) (child ≤6)').

%% Symptom Categories
symptom_category(ptsd_preschool, trauma_exposure_child,
    [ptsd_a1_child, ptsd_a2_child, ptsd_a3_child],
    1, at_least).

symptom_category(ptsd_preschool, intrusion_symptoms_child,
    [ptsd_b1_child, ptsd_b2_child, ptsd_b3_child, ptsd_b4_child, ptsd_b5_child],
    1, at_least).

symptom_category(ptsd_preschool, avoidance_cognition_symptoms_child,
    [ptsd_c1_child, ptsd_c2_child, ptsd_c3_child, ptsd_c4_child, ptsd_c5_child, ptsd_c6_child],
    1, at_least).

symptom_category(ptsd_preschool, arousal_reactivity_symptoms_child,
    [ptsd_d1_child, ptsd_d2_child, ptsd_d3_child, ptsd_d4_child, ptsd_d5_child],
    2, at_least).

%% Duration Requirement
duration_requirement(ptsd_preschool, 1, months).

%% Onset Requirement
onset_requirement(ptsd_preschool, after_event, trauma).

%% Exclusion Criteria
exclusion_criterion(ptsd_preschool, ptsd_exc_substance_child, substance,
    'The disturbance is not attributable to the physiological effects of a substance (e.g., medication)').

exclusion_criterion(ptsd_preschool, ptsd_exc_medical_child, medical,
    'The disturbance is not attributable to another medical condition').

exclusion_criterion(ptsd_preschool, ptsd_exc_dissociative_substance_child, substance,
    'Dissociative symptoms (if present) must not be attributable to the physiological effects of a substance').

exclusion_criterion(ptsd_preschool, ptsd_exc_dissociative_medical_child, medical,
    'Dissociative symptoms (if present) must not be attributable to another medical condition').

%% Subjective Criteria
subjective_criterion(ptsd_preschool, ptsd_subj_distress_child,
    'The disturbance causes clinically significant distress or impairment in relationships with parents, siblings, peers, or other caregivers or with school behavior',
    clinical_significance).

subjective_criterion(ptsd_preschool, ptsd_subj_trauma_quality_child,
    'Exposure involves actual or threatened death, serious injury, or sexual violence',
    quality).

%% Specifiers
specifier(ptsd_preschool, dissociative_symptoms, [present, absent],
    'With dissociative symptoms: persistent or recurrent depersonalization or derealization (may be expressed as play disruption or behavioral changes in young children)').

specifier(ptsd_preschool, delayed_expression, [present, absent],
    'With delayed expression: full diagnostic criteria not met until at least 6 months after the event (although onset and expression of some symptoms may be immediate)').

%% Differential Features (same as adult PTSD)
differential_feature(ptsd_preschool, acute_stress_disorder, ptsd_diff_asd_child,
    'Acute stress disorder: symptom duration 3 days to 1 month; PTSD: duration >1 month').

differential_feature(ptsd_preschool, adjustment_disorder, ptsd_diff_adjustment_child,
    'Adjustment disorder: stressor can be of any severity; PTSD: requires Criterion A trauma exposure').

differential_feature(ptsd_preschool, separation_anxiety_disorder, ptsd_diff_separation_anxiety_child,
    'Separation anxiety: symptoms related to separation from home/family; PTSD: symptoms related to traumatic event').

%% =============================================================================
%% End of PTSD Definitions
%% =============================================================================
