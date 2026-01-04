%% =============================================================================
%% Posttraumatic Stress Disorder (PTSD) - Gold Standard
%% =============================================================================
%% DSM-5 Code: 309.81 (F43.10)
%% Category: Trauma- and Stressor-Related Disorders
%% 
%% Key Features:
%% - Exposure to actual or threatened death, serious injury, or sexual violence
%% - Intrusion symptoms (reexperiencing)
%% - Persistent avoidance of trauma-related stimuli
%% - Negative alterations in cognitions and mood
%% - Alterations in arousal and reactivity
%% - Duration > 1 month
%% - Clinically significant distress or impairment
%% 
%% Note: This file includes criteria for adults, adolescents, and children
%% older than 6 years. Separate criteria for children 6 years and younger
%% are defined as ptsd_preschool.
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
%% PTSD (Adults, Adolescents, Children > 6 years)
%% =============================================================================

%% Disorder Definition
disorder(ptsd, 'Posttraumatic Stress Disorder', trauma_stressor_related).

%% =============================================================================
%% Criterion A: Trauma Exposure
%% =============================================================================

symptom(ptsd, ptsd_a1, trauma_exposure, 'Directly experiencing the traumatic event(s)').
symptom(ptsd, ptsd_a2, trauma_exposure, 'Witnessing, in person, the event(s) as it occurred to others').
symptom(ptsd, ptsd_a3, trauma_exposure, 'Learning that the traumatic event(s) occurred to a close family member or close friend (event must have been violent or accidental)').
symptom(ptsd, ptsd_a4, trauma_exposure, 'Experiencing repeated or extreme exposure to aversive details of the traumatic event(s) (e.g., first responders collecting human remains; police officers repeatedly exposed to details of child abuse). Note: Does not apply to exposure through electronic media, television, movies, or pictures, unless this exposure is work related').

%% =============================================================================
%% Criterion B: Intrusion Symptoms
%% =============================================================================

symptom(ptsd, ptsd_b1, intrusion, 'Recurrent, involuntary, and intrusive distressing memories of the traumatic event(s). Note: In children older than 6 years, repetitive play may occur in which themes or aspects of the traumatic event(s) are expressed').
symptom(ptsd, ptsd_b2, intrusion, 'Recurrent distressing dreams in which the content and/or affect of the dream are related to the traumatic event(s). Note: In children, there may be frightening dreams without recognizable content').
symptom(ptsd, ptsd_b3, intrusion, 'Dissociative reactions (e.g., flashbacks) in which the individual feels or acts as if the traumatic event(s) were recurring (such reactions may occur on a continuum, with the most extreme expression being a complete loss of awareness of present surroundings). Note: In children, trauma-specific reenactment may occur in play').
symptom(ptsd, ptsd_b4, intrusion, 'Intense or prolonged psychological distress at exposure to internal or external cues that symbolize or resemble an aspect of the traumatic event(s)').
symptom(ptsd, ptsd_b5, intrusion, 'Marked physiological reactions to internal or external cues that symbolize or resemble an aspect of the traumatic event(s)').

%% =============================================================================
%% Criterion C: Avoidance
%% =============================================================================

symptom(ptsd, ptsd_c1, avoidance, 'Avoidance of or efforts to avoid distressing memories, thoughts, or feelings about or closely associated with the traumatic event(s)').
symptom(ptsd, ptsd_c2, avoidance, 'Avoidance of or efforts to avoid external reminders (people, places, conversations, activities, objects, situations) that arouse distressing memories, thoughts, or feelings about or closely associated with the traumatic event(s)').

%% =============================================================================
%% Criterion D: Negative Alterations in Cognitions and Mood
%% =============================================================================

symptom(ptsd, ptsd_d1, negative_cognition_mood, 'Inability to remember an important aspect of the traumatic event(s) (typically due to dissociative amnesia and not to other factors such as head injury, alcohol, or drugs)').
symptom(ptsd, ptsd_d2, negative_cognition_mood, 'Persistent and exaggerated negative beliefs or expectations about oneself, others, or the world (e.g., "I am bad," "No one can be trusted," "The world is completely dangerous," "My whole nervous system is permanently ruined")').
symptom(ptsd, ptsd_d3, negative_cognition_mood, 'Persistent, distorted cognitions about the cause or consequences of the traumatic event(s) that lead the individual to blame himself/herself or others').
symptom(ptsd, ptsd_d4, negative_cognition_mood, 'Persistent negative emotional state (e.g., fear, horror, anger, guilt, or shame)').
symptom(ptsd, ptsd_d5, negative_cognition_mood, 'Markedly diminished interest or participation in significant activities').
symptom(ptsd, ptsd_d6, negative_cognition_mood, 'Feelings of detachment or estrangement from others').
symptom(ptsd, ptsd_d7, negative_cognition_mood, 'Persistent inability to experience positive emotions (e.g., inability to experience happiness, satisfaction, or loving feelings)').

%% =============================================================================
%% Criterion E: Alterations in Arousal and Reactivity
%% =============================================================================

symptom(ptsd, ptsd_e1, arousal_reactivity, 'Irritable behavior and angry outbursts (with little or no provocation) typically expressed as verbal or physical aggression toward people or objects').
symptom(ptsd, ptsd_e2, arousal_reactivity, 'Reckless or self-destructive behavior').
symptom(ptsd, ptsd_e3, arousal_reactivity, 'Hypervigilance').
symptom(ptsd, ptsd_e4, arousal_reactivity, 'Exaggerated startle response').
symptom(ptsd, ptsd_e5, arousal_reactivity, 'Problems with concentration').
symptom(ptsd, ptsd_e6, arousal_reactivity, 'Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep)').

%% =============================================================================
%% Symptom Categories with Requirements
%% =============================================================================

symptom_category(ptsd, trauma_exposure, 
    [ptsd_a1, ptsd_a2, ptsd_a3, ptsd_a4], 
    1, at_least_one_of).

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

%% =============================================================================
%% Duration Requirement
%% =============================================================================

duration_requirement(ptsd, 1, months).

%% =============================================================================
%% Onset Requirement
%% =============================================================================

onset_requirement(ptsd, after_event, trauma).

%% =============================================================================
%% Exclusion Criteria
%% =============================================================================

exclusion_criterion(ptsd, ptsd_exc_substance, substance,
    'The disturbance is not attributable to the physiological effects of a substance (e.g., medication, alcohol)').

exclusion_criterion(ptsd, ptsd_exc_medical, medical,
    'The disturbance is not attributable to another medical condition').

%% =============================================================================
%% Subjective Criteria
%% =============================================================================

subjective_criterion(ptsd, ptsd_subj_distress,
    'The disturbance causes clinically significant distress or impairment in social, occupational, or other important areas of functioning',
    clinical_significance).

%% =============================================================================
%% Specifiers
%% =============================================================================

specifier(ptsd, dissociative_symptoms, [present, absent],
    'With dissociative symptoms: persistent or recurrent symptoms of either depersonalization or derealization').

specifier(ptsd, delayed_expression, [present, absent],
    'With delayed expression: if the full diagnostic criteria are not met until at least 6 months after the event (although the onset and expression of some symptoms may be immediate)').

specifier(ptsd, severity, [mild, moderate, severe],
    'Current severity level').

%% =============================================================================
%% Dissociative Symptoms (for specifier)
%% =============================================================================

symptom(ptsd, ptsd_dissoc_depersonalization, dissociative, 'Persistent or recurrent experiences of feeling detached from, and as if one were an outside observer of, one''s mental processes or body (e.g., feeling as though one were in a dream; feeling a sense of unreality of self or body or of time moving slowly)').

symptom(ptsd, ptsd_dissoc_derealization, dissociative, 'Persistent or recurrent experiences of unreality of surroundings (e.g., the world around the individual is experienced as unreal, dreamlike, distant, or distorted)').

%% =============================================================================
%% Differential Features
%% =============================================================================

differential_feature(ptsd, acute_stress_disorder, ptsd_diff_asd,
    'Acute stress disorder is distinguished from PTSD because the symptom pattern in acute stress disorder is restricted to a duration of 3 days to 1 month following exposure to the traumatic event').

differential_feature(ptsd, adjustment_disorder, ptsd_diff_adjustment,
    'In adjustment disorders, the stressor can be of any severity or type rather than that required by PTSD Criterion A').

differential_feature(ptsd, mdd, ptsd_diff_mdd,
    'Major depression may or may not be preceded by a traumatic event and should be diagnosed if other PTSD symptoms are absent. Specifically, major depressive disorder does not include any PTSD Criterion B or C symptoms').

differential_feature(ptsd, gad, ptsd_diff_gad,
    'The avoidance, irritability, and anxiety of generalized anxiety disorder are not associated with a specific traumatic event').

differential_feature(ptsd, panic_disorder, ptsd_diff_panic,
    'The arousal and dissociative symptoms of panic disorder are not associated with a specific traumatic event').

differential_feature(ptsd, ocd, ptsd_diff_ocd,
    'In OCD, there are recurrent intrusive thoughts, but these meet the definition of an obsession. In addition, the intrusive thoughts are not related to an experienced traumatic event, compulsions are usually present, and other symptoms of PTSD are typically absent').

differential_feature(ptsd, dissociative_amnesia, ptsd_diff_dissoc_amnesia,
    'Dissociative amnesia may or may not be preceded by exposure to a traumatic event or may or may not have co-occurring PTSD symptoms. When full PTSD criteria are also met, the PTSD "with dissociative symptoms" subtype should be considered').

differential_feature(ptsd, tbi, ptsd_diff_tbi,
    'Whereas reexperiencing and avoidance are characteristic of PTSD and not the effects of TBI, persistent disorientation and confusion are more specific to TBI (neurocognitive effects) than to PTSD').

%% =============================================================================
%% PTSD for Preschool Children (6 years and younger)
%% =============================================================================

disorder(ptsd_preschool, 'Posttraumatic Stress Disorder for Children 6 Years and Younger', trauma_stressor_related).

%% =============================================================================
%% Criterion A: Trauma Exposure (Preschool)
%% =============================================================================

symptom(ptsd_preschool, ptsd_pre_a1, trauma_exposure, 'Directly experiencing the traumatic event(s)').
symptom(ptsd_preschool, ptsd_pre_a2, trauma_exposure, 'Witnessing, in person, the event(s) as it occurred to others, especially primary caregivers. Note: Witnessing does not include events that are witnessed only in electronic media, television, movies, or pictures').
symptom(ptsd_preschool, ptsd_pre_a3, trauma_exposure, 'Learning that the traumatic event(s) occurred to a parent or caregiving figure').

%% =============================================================================
%% Criterion B: Intrusion Symptoms (Preschool)
%% =============================================================================

symptom(ptsd_preschool, ptsd_pre_b1, intrusion, 'Recurrent, involuntary, and intrusive distressing memories of the traumatic event(s). Note: Spontaneous and intrusive memories may not necessarily appear distressing and may be expressed as play reenactment').
symptom(ptsd_preschool, ptsd_pre_b2, intrusion, 'Recurrent distressing dreams in which the content and/or affect of the dream are related to the traumatic event(s). Note: It may not be possible to ascertain that the frightening content is related to the traumatic event').
symptom(ptsd_preschool, ptsd_pre_b3, intrusion, 'Dissociative reactions (e.g., flashbacks) in which the child feels or acts as if the traumatic event(s) were recurring (such reactions may occur on a continuum, with the most extreme expression being a complete loss of awareness of present surroundings). Such trauma-specific reenactment may occur in play').
symptom(ptsd_preschool, ptsd_pre_b4, intrusion, 'Intense or prolonged psychological distress at exposure to internal or external cues that symbolize or resemble an aspect of the traumatic event(s)').
symptom(ptsd_preschool, ptsd_pre_b5, intrusion, 'Marked physiological reactions to reminders of the traumatic event(s)').

%% =============================================================================
%% Criterion C: Avoidance OR Negative Alterations (Preschool)
%% Note: Combined into one criterion requiring 1+ from either subcategory
%% =============================================================================

symptom(ptsd_preschool, ptsd_pre_c1, avoidance, 'Avoidance of or efforts to avoid activities, places, or physical reminders that arouse recollections of the traumatic event(s)').
symptom(ptsd_preschool, ptsd_pre_c2, avoidance, 'Avoidance of or efforts to avoid people, conversations, or interpersonal situations that arouse recollections of the traumatic event(s)').
symptom(ptsd_preschool, ptsd_pre_c3, negative_alterations, 'Substantially increased frequency of negative emotional states (e.g., fear, guilt, sadness, shame, confusion)').
symptom(ptsd_preschool, ptsd_pre_c4, negative_alterations, 'Markedly diminished interest or participation in significant activities, including constriction of play').
symptom(ptsd_preschool, ptsd_pre_c5, negative_alterations, 'Socially withdrawn behavior').
symptom(ptsd_preschool, ptsd_pre_c6, negative_alterations, 'Persistent reduction in expression of positive emotions').

%% =============================================================================
%% Criterion D: Arousal and Reactivity (Preschool)
%% =============================================================================

symptom(ptsd_preschool, ptsd_pre_d1, arousal_reactivity, 'Irritable behavior and angry outbursts (with little or no provocation) typically expressed as verbal or physical aggression toward people or objects (including extreme temper tantrums)').
symptom(ptsd_preschool, ptsd_pre_d2, arousal_reactivity, 'Hypervigilance').
symptom(ptsd_preschool, ptsd_pre_d3, arousal_reactivity, 'Exaggerated startle response').
symptom(ptsd_preschool, ptsd_pre_d4, arousal_reactivity, 'Problems with concentration').
symptom(ptsd_preschool, ptsd_pre_d5, arousal_reactivity, 'Sleep disturbance (e.g., difficulty falling or staying asleep or restless sleep)').

%% =============================================================================
%% Symptom Categories (Preschool)
%% =============================================================================

symptom_category(ptsd_preschool, trauma_exposure,
    [ptsd_pre_a1, ptsd_pre_a2, ptsd_pre_a3],
    1, at_least_one_of).

symptom_category(ptsd_preschool, intrusion_symptoms,
    [ptsd_pre_b1, ptsd_pre_b2, ptsd_pre_b3, ptsd_pre_b4, ptsd_pre_b5],
    1, at_least).

symptom_category(ptsd_preschool, avoidance_or_negative_alterations,
    [ptsd_pre_c1, ptsd_pre_c2, ptsd_pre_c3, ptsd_pre_c4, ptsd_pre_c5, ptsd_pre_c6],
    1, at_least).

symptom_category(ptsd_preschool, arousal_reactivity_symptoms,
    [ptsd_pre_d1, ptsd_pre_d2, ptsd_pre_d3, ptsd_pre_d4, ptsd_pre_d5],
    2, at_least).

%% =============================================================================
%% Duration Requirement (Preschool)
%% =============================================================================

duration_requirement(ptsd_preschool, 1, months).

%% =============================================================================
%% Onset Requirement (Preschool)
%% =============================================================================

onset_requirement(ptsd_preschool, after_event, trauma).

%% =============================================================================
%% Exclusion Criteria (Preschool)
%% =============================================================================

exclusion_criterion(ptsd_preschool, ptsd_pre_exc_substance, substance,
    'The disturbance is not attributable to the physiological effects of a substance (e.g., medication or alcohol)').

exclusion_criterion(ptsd_preschool, ptsd_pre_exc_medical, medical,
    'The disturbance is not attributable to another medical condition').

%% =============================================================================
%% Subjective Criteria (Preschool)
%% =============================================================================

subjective_criterion(ptsd_preschool, ptsd_pre_subj_impairment,
    'The disturbance causes clinically significant distress or impairment in relationships with parents, siblings, peers, or other caregivers or with school behavior',
    functional_impairment).

%% =============================================================================
%% Specifiers (Preschool)
%% =============================================================================

specifier(ptsd_preschool, dissociative_symptoms, [present, absent],
    'With dissociative symptoms: persistent or recurrent symptoms of either depersonalization or derealization').

specifier(ptsd_preschool, delayed_expression, [present, absent],
    'With delayed expression: if the full diagnostic criteria are not met until at least 6 months after the event (although the onset and expression of some symptoms may be immediate)').

%% =============================================================================
%% Dissociative Symptoms for Preschool (for specifier)
%% =============================================================================

symptom(ptsd_preschool, ptsd_pre_dissoc_depersonalization, dissociative, 'Persistent or recurrent experiences of feeling detached from, and as if one were an outside observer of, one''s mental processes or body (e.g., feeling as though one were in a dream; feeling a sense of unreality of self or body or of time moving slowly)').

symptom(ptsd_preschool, ptsd_pre_dissoc_derealization, dissociative, 'Persistent or recurrent experiences of unreality of surroundings (e.g., the world around the individual is experienced as unreal, dreamlike, distant, or distorted)').