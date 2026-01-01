from src.search.manager import SessionManager

def debug_adhd():
    m = SessionManager()
    m.start_new_session() # Clears state
    
    pid = m.state.patient_id
    print(f"Patient ID: {pid}")
    
    # 1. Set Age to 29 (Adult)
    # This should trigger age_adjusted_count for ADHD (needs 5 instead of 6)
    print("Setting Age to 29...")
    m.set_patient_data(29)
    # Inspect assertions
    print("Verifying Prolog assertions for age...")
    res = list(m.prolog.query(f"patient_context({pid}, age, Age)"))
    print(f"  patient_context age: {res}")
    # res = list(m.prolog.query(f"patient_age({pid}, Age)")) # legacy check
    # print(f"  patient_age: {res}")
    
    # 2. Assert 5 Inattention Symptoms (Sufficient for Adult, Insufficient for Child)
    # ADHD Inattention: adhd_a1a .. adhd_a1i
    print("\nAsserting 5 Inattention Symptoms...")
    symptoms = ['adhd_a1a', 'adhd_a1b', 'adhd_a1c', 'adhd_a1d', 'adhd_a1e'] # 5 Inattention
    for s in symptoms:
        m.answer_question(s, 'YES')

    print("Asserting 5 Hyperactivity Symptoms (to satisfy Combined Type logic)...")
    hsymptoms = ['adhd_a2a', 'adhd_a2b', 'adhd_a2c', 'adhd_a2d', 'adhd_a2e'] # 5 Hyperactivity
    for s in hsymptoms:
        m.answer_question(s, 'YES')
        
    # Check Symptom Criteria
    print("Checking Symptom Criteria (Expect TRUE for Adult)...")
    q = f"meets_symptom_criteria({pid}, adhd)"
    res = list(m.prolog.query(q))
    print(f"  Meets Symptoms: {bool(res)}")
    
    if not res:
        print("  FAILED. Checking Category Details:")
        q = f"check_all_symptom_categories({pid}, adhd, Detail)"
        res = list(m.prolog.query(q))
        print(f"  Details: {res}")
        
        print("  Debugging Inattention Count:")
        q = "count_present_symptoms(current_patient, [adhd_a1a, adhd_a1b, adhd_a1c, adhd_a1d, adhd_a1e, adhd_a1f, adhd_a1g, adhd_a1h, adhd_a1i], C)"
        print(f"  Count: {list(m.prolog.query(q))}")
        return

    # 3. Assert Duration
    print("\nAsserting Duration (>6 months)...")
    # adhd_duration_check -> assert 6 months
    m.answer_question('adhd_duration_check', 'YES')
    
    # 4. Assert Exclusions
    print("Asserting Exclusions...")
    # exclusions: substance, medical, psychotic, ...
    # From adhd.pl: adhd_exc_substance, adhd_exc_medical, adhd_exc_psychotic, adhd_exc_mood, adhd_exc_anxiety, adhd_exc_dissociative, adhd_exc_personality
    exclusions = ['adhd_exc_substance', 'adhd_exc_medical', 'adhd_exc_psychotic', 'adhd_exc_mood', 'adhd_exc_anxiety', 'adhd_exc_dissociative', 'adhd_exc_personality']
    for e in exclusions:
        m.answer_question(e, 'YES') # "Not attributable" -> Cleared
        
    # 5. Assert Subjective
    print("Asserting Subjective Criteria...")
    # adhd.pl defines 3 subjective criteria
    subjs = ['adhd_subj_functional_impairment', 'adhd_subj_developmental', 'adhd_subj_not_oppositional']
    for s in subjs:
        m.answer_question(s, 'YES')
    
    # 6. Assert ONSET (The Critical Fix)
    print("\nAsserting Onset (Before Age 12)...")
    # Question ID: adhd_onset_age_check
    # If YES -> asserts patient_onset_age < 12
    m.answer_question('adhd_onset_age_check', 'YES')
    
    # Verify Onset Assertion
    res = list(m.prolog.query(f"patient_onset_age({pid}, Age)"))
    print(f"  Recorded Onset Age: {res}")
    
    # Check Onset Criteria
    q = f"meets_onset_criteria({pid}, adhd)"
    res = list(m.prolog.query(q))
    print(f"  Meets Onset Criteria: {bool(res)}")
    
    # 7. Assert Settings (ADHD specific: 2+ settings)
    # setting_requirement(adhd, 2).
    # We need to assert settings in patient_context?
    # schema.pl doesn't specify how settings are asserted in manager.
    # checking schema: meets_setting_criteria? (not in standard list?)
    # Wait, diagnosis_candidate checks: symptoms, duration, onset, exclusions, subjective.
    # Does it check settings?
    # Let's check schema.pl lines 333+.
    # Line 340 (view 588): meets_subjective... calculate_confidence.
    # It does NOT check settings in diagnosis_candidate!
    # (Maybe settings are part of subjective or just missing in main rule?)
    # But schema line 958 checks settings for full_diagnosis.
    # diagnosis_candidate (line 333) vs full_diagnosis (949).
    # manager uses diagnosis_candidate?
    # If setting is ignored in candidate, we are good.
    
    # 8. Check Final Diagnosis
    print("\nChecking Final Diagnosis...")
    q = f"diagnosis_candidate({pid}, adhd, Conf)"
    res = list(m.prolog.query(q))
    print(f"  Diagnosis Candidate: {res}")
    
    if not res:
         print("  FAILED. Checking Statuses:")
         for c in ['symptoms', 'duration', 'onset', 'exclusions', 'subjective']:
            q = f"criterion_status({pid}, adhd, {c}, St)"
            r = list(m.prolog.query(q))
            print(f"  {c}: {r}")

if __name__ == "__main__":
    debug_adhd()
