from src.search.manager import SessionManager

def debug_mdd():
    m = SessionManager()
    m.start_new_session()
    
    # Enable debug prints
    pid = m.state.patient_id
    print(f"Patient ID: {pid}")
    
    # Set Age
    m.set_patient_data(34)
    
    # Assert Core Symptoms
    print("Asserting mdd_a1 (Depressed Mood)...")
    m.answer_question('mdd_a1', 'YES')
    print("Asserting mdd_a2 (Anhedonia)...")
    m.answer_question('mdd_a2', 'YES')
    
    # Assert 3 more to reach 5
    m.answer_question('mdd_a3', 'YES') # Weight
    m.answer_question('mdd_a4', 'YES') # Sleep
    m.answer_question('mdd_a6', 'YES') # Fatigue
    
    # Query check
    print("\nChecking Symptom Criteria...")
    q = f"meets_symptom_criteria({pid}, mdd)"
    res = list(m.prolog.query(q))
    print(f"Meets Symptom Criteria: {bool(res)}")
    
    if not res:
        # Debug counts directly
        print("Debugging Category Counts:")
        # Check Core
        q = "count_present_symptoms(current_patient, [mdd_a1, mdd_a2], C)"
        res = list(m.prolog.query(q))
        print(f"  Core Count (Need 1): {res}")
        
        # Check All
        q = "count_present_symptoms(current_patient, [mdd_a1, mdd_a2, mdd_a3, mdd_a4, mdd_a5, mdd_a6, mdd_a7, mdd_a8, mdd_a9], C)"
        res = list(m.prolog.query(q))
        print(f"  All Count (Need 5): {res}")

    # Assert Duration
    m.answer_question('mdd_duration_check', 'YES')
    
    # Assert Exclusions (Cleared)
    m.answer_question('mdd_exc_manic', 'YES')
    m.answer_question('mdd_exc_substance', 'YES')
    m.answer_question('mdd_exc_medical', 'YES')
    m.answer_question('mdd_exc_psychotic', 'YES') # Correct ID
    m.answer_question('mdd_exc_bipolar', 'YES') # Correct ID

    # Assert Subjective
    m.answer_question('mdd_subj_clinical_significance', 'YES')

    # Check Status
    print("\nChecking Diagnosis Candidate...")
    q = f"diagnosis_candidate({pid}, mdd, C)"
    res = list(m.prolog.query(q))
    print(f"Candidate: {res}")
    
    if not res:
        # Check reasons
        print("Checking individual criteria status:")
        for c in ['symptoms', 'duration', 'exclusions', 'subjective']:
            q = f"criterion_status({pid}, mdd, {c}, St)"
            r = list(m.prolog.query(q))
            print(f"  {c}: {r}")

if __name__ == "__main__":
    debug_mdd()
