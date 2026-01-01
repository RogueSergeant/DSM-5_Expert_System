%% =============================================================================
%% AUTISM SPECTRUM DISORDER - Gold Standard
%% =============================================================================
%% DSM-5 Reference: pp. 50-59
%% ICD-10 Code: F84.0 | DSM-5 Code: 299.00
%% Key features: Persistent deficits in social communication and social
%% interaction across multiple contexts, combined with restricted, repetitive
%% patterns of behavior, interests, or activities.
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

%% =============================================================================
%% DISORDER DEFINITION
%% =============================================================================

disorder(asd, 'Autism Spectrum Disorder', neurodevelopmental).

%% =============================================================================
%% CRITERION A: SOCIAL COMMUNICATION AND SOCIAL INTERACTION DEFICITS
%% Persistent deficits across multiple contexts - ALL THREE required
%% =============================================================================

symptom(asd, asd_a1, social_communication,
    'Deficits in social-emotional reciprocity, ranging from abnormal social approach and failure of normal back-and-forth conversation; to reduced sharing of interests, emotions, or affect; to failure to initiate or respond to social interactions').

symptom(asd, asd_a2, social_communication,
    'Deficits in nonverbal communicative behaviors used for social interaction, ranging from poorly integrated verbal and nonverbal communication; to abnormalities in eye contact and body language or deficits in understanding and use of gestures; to a total lack of facial expressions and nonverbal communication').

symptom(asd, asd_a3, social_communication,
    'Deficits in developing, maintaining, and understanding relationships, ranging from difficulties adjusting behavior to suit various social contexts; to difficulties in sharing imaginative play or in making friends; to absence of interest in peers').

%% =============================================================================
%% CRITERION B: RESTRICTED, REPETITIVE PATTERNS OF BEHAVIOR
%% At least 2 of 4 required, currently or by history
%% =============================================================================

symptom(asd, asd_b1, restricted_repetitive,
    'Stereotyped or repetitive motor movements, use of objects, or speech (e.g., simple motor stereotypies, lining up toys or flipping objects, echolalia, idiosyncratic phrases)').

symptom(asd, asd_b2, restricted_repetitive,
    'Insistence on sameness, inflexible adherence to routines, or ritualized patterns of verbal or nonverbal behavior (e.g., extreme distress at small changes, difficulties with transitions, rigid thinking patterns, greeting rituals, need to take same route or eat same food every day)').

symptom(asd, asd_b3, restricted_repetitive,
    'Highly restricted, fixated interests that are abnormal in intensity or focus (e.g., strong attachment to or preoccupation with unusual objects, excessively circumscribed or perseverative interests)').

symptom(asd, asd_b4, restricted_repetitive,
    'Hyper- or hyporeactivity to sensory input or unusual interest in sensory aspects of the environment (e.g., apparent indifference to pain/temperature, adverse response to specific sounds or textures, excessive smelling or touching of objects, visual fascination with lights or movement)').

%% =============================================================================
%% SYMPTOM CATEGORIES
%% =============================================================================

%% Criterion A: All 3 social communication deficits required
symptom_category(asd, social_communication_deficits,
    [asd_a1, asd_a2, asd_a3],
    3, all).

%% Criterion B: At least 2 of 4 restricted/repetitive behaviors required
symptom_category(asd, restricted_repetitive_behaviors,
    [asd_b1, asd_b2, asd_b3, asd_b4],
    2, at_least).

%% =============================================================================
%% DURATION REQUIREMENT
%% =============================================================================
%% No specific duration - symptoms must be present from early developmental period
%% Duration is implicit in the onset requirement

%% =============================================================================
%% ONSET REQUIREMENT
%% =============================================================================
%% Criterion C: Symptoms must be present in the early developmental period
%% (but may not become fully manifest until social demands exceed limited
%% capacities, or may be masked by learned strategies in later life)

onset_requirement(asd, before_age, 18).

%% =============================================================================
%% SETTING REQUIREMENT
%% =============================================================================
%% Criterion A specifies "across multiple contexts"

setting_requirement(asd, 2).

%% =============================================================================
%% EXCLUSION CRITERIA
%% =============================================================================

%% Criterion E: Not better explained by intellectual disability or global
%% developmental delay
exclusion_criterion(asd, asd_exc_intellectual_disability, other_disorder,
    'The disturbances are not better explained by intellectual disability (intellectual developmental disorder) or global developmental delay. Note: Intellectual disability and autism spectrum disorder frequently co-occur; to make comorbid diagnoses, social communication should be below that expected for general developmental level').

%% =============================================================================
%% SUBJECTIVE CRITERIA
%% =============================================================================

%% Criterion D: Clinical significance
subjective_criterion(asd, asd_subj_impairment,
    'Symptoms cause clinically significant impairment in social, occupational, or other important areas of current functioning',
    clinical_significance).

%% Criterion C: Early developmental period presence
subjective_criterion(asd, asd_subj_early_onset,
    'Symptoms were present in the early developmental period (but may not have become fully manifest until social demands exceeded limited capacities, or may have been masked by learned strategies in later life)',
    quality).

%% Severity assessment for social communication domain
subjective_criterion(asd, asd_subj_severity_social,
    'Severity level for social communication impairments (Level 1: Requiring support, Level 2: Requiring substantial support, Level 3: Requiring very substantial support)',
    severity).

%% Severity assessment for restricted/repetitive behaviors domain
subjective_criterion(asd, asd_subj_severity_rrb,
    'Severity level for restricted, repetitive behaviors (Level 1: Requiring support, Level 2: Requiring substantial support, Level 3: Requiring very substantial support)',
    severity).

%% =============================================================================
%% SPECIFIERS
%% =============================================================================

%% Severity specifiers - rated separately for each domain
specifier(asd, severity_social_communication,
    [level_1_requiring_support, level_2_requiring_substantial_support, level_3_requiring_very_substantial_support],
    'Severity level for social communication domain based on level of support needed').

specifier(asd, severity_restricted_repetitive,
    [level_1_requiring_support, level_2_requiring_substantial_support, level_3_requiring_very_substantial_support],
    'Severity level for restricted, repetitive behaviors domain based on level of support needed').

%% Intellectual impairment specifier
specifier(asd, intellectual_impairment,
    [with_accompanying_intellectual_impairment, without_accompanying_intellectual_impairment],
    'With or without accompanying intellectual impairment').

%% Language impairment specifier
specifier(asd, language_impairment,
    [with_accompanying_language_impairment, without_accompanying_language_impairment],
    'With or without accompanying language impairment. If with, specify current level of verbal functioning (e.g., no intelligible speech, single words only, phrase speech, full sentences, fluent speech)').

%% Associated medical/genetic condition specifier
specifier(asd, associated_condition,
    [associated_with_known_medical_genetic_or_environmental_factor, not_associated],
    'Associated with a known medical or genetic condition or environmental factor (e.g., Rett syndrome, Fragile X syndrome, Down syndrome, epilepsy, valproate exposure, fetal alcohol syndrome, very low birth weight)').

%% Associated neurodevelopmental/mental/behavioral disorder specifier
specifier(asd, associated_disorder,
    [associated_with_neurodevelopmental_mental_or_behavioral_disorder, not_associated],
    'Associated with another neurodevelopmental, mental, or behavioral disorder (e.g., ADHD, developmental coordination disorder, anxiety disorders, depressive disorders, tics or Tourette disorder)').

%% Catatonia specifier
specifier(asd, catatonia,
    [with_catatonia, without_catatonia],
    'With catatonia (use additional code 293.89 [F06.1] catatonia associated with autism spectrum disorder)').

%% =============================================================================
%% DIFFERENTIAL FEATURES
%% =============================================================================

differential_feature(asd, rett_syndrome, asd_diff_rett,
    'In Rett syndrome, disruption of social interaction occurs during regressive phase (typically 1-4 years), but most individuals improve social communication skills after this period. ASD should only be diagnosed when all diagnostic criteria are met throughout development').

differential_feature(asd, selective_mutism, asd_diff_selective_mutism,
    'In selective mutism, early development is not typically disturbed, the child exhibits appropriate communication skills in certain contexts, social reciprocity is not impaired even in mute settings, and restricted/repetitive patterns are not present').

differential_feature(asd, language_disorder, asd_diff_language,
    'Language disorders may involve communication problems and secondary social difficulties, but are not usually associated with abnormal nonverbal communication or restricted, repetitive patterns of behavior, interests, or activities').

differential_feature(asd, social_pragmatic_communication_disorder, asd_diff_spcd,
    'Social (pragmatic) communication disorder involves impairment in social communication and interaction but without restricted and repetitive behavior or interests. ASD diagnosis supersedes SPCD when ASD criteria are met. Past or current restricted/repetitive behavior must be carefully assessed').

differential_feature(asd, intellectual_disability, asd_diff_id,
    'Intellectual disability without ASD may be difficult to differentiate in young children. ASD is appropriate when social communication and interaction are significantly impaired relative to the developmental level of nonverbal skills. Intellectual disability is appropriate when there is no discrepancy between social-communicative skills and other intellectual skills').

differential_feature(asd, stereotypic_movement_disorder, asd_diff_smd,
    'Motor stereotypies are among the diagnostic characteristics of ASD, so stereotypic movement disorder is not additionally diagnosed when repetitive behaviors are better explained by ASD. However, both diagnoses may be appropriate when stereotypies cause self-injury and become a focus of treatment').

differential_feature(asd, adhd, asd_diff_adhd,
    'Abnormalities of attention and hyperactivity are common in ASD. ADHD should be considered when attentional difficulties or hyperactivity exceeds that typically seen in individuals of comparable mental age. Both diagnoses can be given when criteria for both are met').

differential_feature(asd, schizophrenia, asd_diff_schizophrenia,
    'Schizophrenia with childhood onset usually develops after normal or near normal development. Prodromal social impairment and atypical interests could be confused with ASD social deficits. Hallucinations and delusions are defining features of schizophrenia but not ASD. Clinicians must account for concrete interpretation of questions in individuals with ASD').