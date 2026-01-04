%% =============================================================================
%% Posttraumatic Stress Disorder (PTSD) - Gold Standard
%% =============================================================================
%% DSM-5 Reference: pp. 271-280
%% Key features: Development of characteristic symptoms following exposure to
%% traumatic events, including intrusion, avoidance, negative alterations in
%% cognition/mood, and alterations in arousal/reactivity.
%% Two versions: standard (age 7+) and preschool (age ≤6)
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
%% PTSD (Standard Version - Adults, Adolescents, Children > 6 years)
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

%% Criterion C: Avoidance
symptom(ptsd, ptsd_c1, avoidance, 'Avoidance of or efforts to avoid distressing memories, thoughts, or feelings about or closely associated with the traumatic event(s)').
symptom(ptsd, ptsd_c2, avoidance, 'Avoidance of or efforts to avoid external reminders (people, places, conversations, activities, objects, situations) that arouse distressing memories, thoughts, or feelings about the traumatic event(s)').

%% Criterion D: Negative Alterations in Cognitions and Mood
symptom(ptsd, ptsd_d1, negative_cognition_mood, 'Inability to remember an important aspect of the traumatic event(s) (typically due to dissociative amnesia)').
symptom(ptsd, ptsd_d2, negative_cognition_mood, 'Persistent and exaggerated negative beliefs or expectations about oneself, others, or the world').
symptom(ptsd, ptsd_d3, negative_cognition_mood, 'Persistent, distorted cognitions about the cause or consequences of the traumatic event(s) that lead the individual to blame himself/herself or others').
symptom(ptsd, ptsd_d4, negative_cognition_mood, 'Persistent negative emotional state (e.g., fear, horror, anger, guilt, or shame)').
symptom(ptsd, ptsd_d5, negative_cognition_mood, 'Markedly diminished interest or participation in significant activities').
symptom(ptsd, ptsd_d6, negative_cognition_mood, 'Feelings of detachment or estrangement from others').
symptom(ptsd, ptsd_d7, negative_cognition_mood, 'Persistent inability to experience positive emotions (e.g., inability to experience happiness, satisfaction, or loving feelings)').

%% Criterion E: Alterations in Arousal and Reactivity
symptom(ptsd, ptsd_e1, arousal_reactivity, 'Irritable behavior and angry outbursts (with little or no provocation) typically expressed as verbal or physical aggression toward people or objects').
symptom(ptsd, ptsd_e2, arousal_reactivity, 'Reckless or self-destructive behavior').
symptom(ptsd, ptsd_e3, arousal_reactivity, 'Hypervigilance').
symptom(ptsd, ptsd_e4, arousal_reactivity, 'Exaggerated startle response').
symptom(ptsd, ptsd_e5, arousal_reactivity, 'Problems with concentration').
symptom(ptsd, ptsd_e6, arousal_reactivity, 'Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep)').

symptom_category(ptsd, trauma_exposure, [ptsd_a1, ptsd_a2, ptsd_a3, ptsd_a4], 1, at_least_one_of).
symptom_category(ptsd, intrusion_symptoms, [ptsd_b1, ptsd_b2, ptsd_b3, ptsd_b4, ptsd_b5], 1, at_least).
symptom_category(ptsd, avoidance_symptoms, [ptsd_c1, ptsd_c2], 1, at_least).
symptom_category(ptsd, negative_cognition_mood_symptoms, [ptsd_d1, ptsd_d2, ptsd_d3, ptsd_d4, ptsd_d5, ptsd_d6, ptsd_d7], 2, at_least).
symptom_category(ptsd, arousal_reactivity_symptoms, [ptsd_e1, ptsd_e2, ptsd_e3, ptsd_e4, ptsd_e5, ptsd_e6], 2, at_least).

duration_requirement(ptsd, 1, months).

onset_requirement(ptsd, after_event, trauma).

exclusion_criterion(ptsd, ptsd_exc_substance, substance, 'The disturbance is not attributable to the physiological effects of a substance (e.g., medication, alcohol)').
exclusion_criterion(ptsd, ptsd_exc_medical, medical, 'The disturbance is not attributable to another medical condition').

subjective_criterion(ptsd, ptsd_subj_distress, 'The disturbance causes clinically significant distress or impairment in social, occupational, or other important areas of functioning', clinical_significance).

specifier(ptsd, dissociative_symptoms, [present, absent], 'With dissociative symptoms: persistent or recurrent depersonalization or derealization').
specifier(ptsd, delayed_expression, [present, absent], 'With delayed expression: full diagnostic criteria not met until at least 6 months after the event').
specifier(ptsd, severity, [mild, moderate, severe], 'Current severity level').

differential_feature(ptsd, acute_stress_disorder, ptsd_diff_asd, 'Acute stress disorder is restricted to 3 days to 1 month following trauma; PTSD requires duration greater than 1 month').
differential_feature(ptsd, adjustment_disorder, ptsd_diff_adj, 'Adjustment disorders can occur with any stressor; PTSD requires Criterion A traumatic event').
differential_feature(ptsd, mdd, ptsd_diff_mdd, 'MDD may follow trauma but lacks PTSD Criterion B (intrusion) and C (avoidance) symptoms').
differential_feature(ptsd, gad, ptsd_diff_gad, 'GAD anxiety not tied to specific traumatic event; PTSD symptoms directly related to trauma').
differential_feature(ptsd, ocd, ptsd_diff_ocd, 'OCD intrusive thoughts meet definition of obsession and are not related to experienced traumatic event; compulsions usually present').
differential_feature(ptsd, panic_disorder, ptsd_diff_panic, 'Panic disorder arousal and dissociative symptoms not associated with specific traumatic event').
differential_feature(ptsd, tbi, ptsd_diff_tbi, 'TBI symptoms include persistent disorientation and confusion; PTSD characterized by reexperiencing and avoidance').

%% =============================================================================
%% PTSD for Preschool Children (6 Years and Younger)
%% =============================================================================

disorder(ptsd_preschool, 'Posttraumatic Stress Disorder for Children 6 Years and Younger', trauma_stressor_related).

symptom(ptsd_preschool, ptsd_pre_a1, trauma_exposure, 'Directly experiencing the traumatic event(s)').
symptom(ptsd_preschool, ptsd_pre_a2, trauma_exposure, 'Witnessing, in person, the event(s) as it occurred to others, especially primary caregivers').
symptom(ptsd_preschool, ptsd_pre_a3, trauma_exposure, 'Learning that the traumatic event(s) occurred to a parent or caregiving figure').

symptom(ptsd_preschool, ptsd_pre_b1, intrusion, 'Recurrent, involuntary, and intrusive distressing memories of the traumatic event(s) (spontaneous and intrusive memories may be expressed as play reenactment)').
symptom(ptsd_preschool, ptsd_pre_b2, intrusion, 'Recurrent distressing dreams in which the content and/or affect of the dream are related to the traumatic event(s)').
symptom(ptsd_preschool, ptsd_pre_b3, intrusion, 'Dissociative reactions (e.g., flashbacks) in which the child feels or acts as if the traumatic event(s) were recurring (trauma-specific reenactment may occur in play)').
symptom(ptsd_preschool, ptsd_pre_b4, intrusion, 'Intense or prolonged psychological distress at exposure to internal or external cues that symbolize or resemble an aspect of the traumatic event(s)').
symptom(ptsd_preschool, ptsd_pre_b5, intrusion, 'Marked physiological reactions to reminders of the traumatic event(s)').

symptom(ptsd_preschool, ptsd_pre_c1, avoidance_or_negative, 'Avoidance of or efforts to avoid activities, places, or physical reminders that arouse recollections of the traumatic event(s)').
symptom(ptsd_preschool, ptsd_pre_c2, avoidance_or_negative, 'Avoidance of or efforts to avoid people, conversations, or interpersonal situations that arouse recollections of the traumatic event(s)').
symptom(ptsd_preschool, ptsd_pre_c3, avoidance_or_negative, 'Substantially increased frequency of negative emotional states (e.g., fear, guilt, sadness, shame, confusion)').
symptom(ptsd_preschool, ptsd_pre_c4, avoidance_or_negative, 'Markedly diminished interest or participation in significant activities, including constriction of play').
symptom(ptsd_preschool, ptsd_pre_c5, avoidance_or_negative, 'Socially withdrawn behavior').
symptom(ptsd_preschool, ptsd_pre_c6, avoidance_or_negative, 'Persistent reduction in expression of positive emotions').

symptom(ptsd_preschool, ptsd_pre_d1, arousal_reactivity, 'Irritable behavior and angry outbursts (with little or no provocation) typically expressed as verbal or physical aggression toward people or objects (including extreme temper tantrums)').
symptom(ptsd_preschool, ptsd_pre_d2, arousal_reactivity, 'Hypervigilance').
symptom(ptsd_preschool, ptsd_pre_d3, arousal_reactivity, 'Exaggerated startle response').
symptom(ptsd_preschool, ptsd_pre_d4, arousal_reactivity, 'Problems with concentration').
symptom(ptsd_preschool, ptsd_pre_d5, arousal_reactivity, 'Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep)').

symptom_category(ptsd_preschool, trauma_exposure, [ptsd_pre_a1, ptsd_pre_a2, ptsd_pre_a3], 1, at_least_one_of).
symptom_category(ptsd_preschool, intrusion_symptoms, [ptsd_pre_b1, ptsd_pre_b2, ptsd_pre_b3, ptsd_pre_b4, ptsd_pre_b5], 1, at_least).
symptom_category(ptsd_preschool, avoidance_or_negative_symptoms, [ptsd_pre_c1, ptsd_pre_c2, ptsd_pre_c3, ptsd_pre_c4, ptsd_pre_c5, ptsd_pre_c6], 1, at_least).
symptom_category(ptsd_preschool, arousal_reactivity_symptoms, [ptsd_pre_d1, ptsd_pre_d2, ptsd_pre_d3, ptsd_pre_d4, ptsd_pre_d5], 2, at_least).

duration_requirement(ptsd_preschool, 1, months).

onset_requirement(ptsd_preschool, after_event, trauma).

exclusion_criterion(ptsd_preschool, ptsd_pre_exc_substance, substance, 'The disturbance is not attributable to the physiological effects of a substance (e.g., medication or alcohol)').
exclusion_criterion(ptsd_preschool, ptsd_pre_exc_medical, medical, 'The disturbance is not attributable to another medical condition').

subjective_criterion(ptsd_preschool, ptsd_pre_subj_distress, 'The disturbance causes clinically significant distress or impairment in relationships with parents, siblings, peers, or other caregivers or with school behavior', clinical_significance).

specifier(ptsd_preschool, dissociative_symptoms, [present, absent], 'With dissociative symptoms: persistent or recurrent depersonalization or derealization').
specifier(ptsd_preschool, delayed_expression, [present, absent], 'With delayed expression: full diagnostic criteria not met until at least 6 months after the event').
specifier(ptsd_preschool, severity, [mild, moderate, severe], 'Current severity level').