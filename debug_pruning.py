from src.search.manager import SessionManager

def debug_pruning():
    m = SessionManager()
    m.start_new_session()
    
    pid = m.state.patient_id
    print(f"Initial Candidates: {m.state.active_candidates}")
    
    # 1. Test Exclusion Pruning
    # MDD Exclusion: mdd_exc_manic (History of Mania)
    # If we answer NO to "Is it true there has never been a manic episode?" -> NO means Exclusion Applies (Status=excluded)
    # Wait, let's allow the manager to handle the logic. 
    # Question: mdd_exc_bipolar ("There has never been a manic episode...")
    # Answer NO -> Means there WAS a manic episode -> Exclusion Applies (excluded).
    
    print("\nSimulating: User admits to Manic Episode (Ruling out MDD)...")
    m.answer_question('mdd_exc_bipolar', 'NO') 
    
    print(f"Candidates after Mania check: {m.state.active_candidates}")
    if 'mdd' not in m.state.active_candidates:
        print("  SUCCESS: MDD Pruned.")
    else:
        print("  FAILURE: MDD still active.")
        
    # 2. Test Core Symptom Pruning (GAD)
    # GAD Core: Excessive Anxiety (gad_a1?? wait, core is A. gad_c??)
    # Let's check GAD core symptoms.
    # gad.pl: symptom(gad, gad_a_anxiety, core, ...) (need to check ID)
    # Let's check GAD symptoms first.
    
    print("\nChecking GAD Core Symptoms...")
    q = "symptom(gad, S, core, _)"
    res = list(m.prolog.query(q))
    if not res:
        # Maybe category is different? 'criterion_a'?
        q = "symptom(gad, S, criterion_a, _)"
        res = list(m.prolog.query(q))
    
    print(f"  GAD Core IDs: {[str(r['S']) for r in res]}")
    gad_cores = [str(r['S']) for r in res]
    
    if gad_cores:
        print("Simulating: User denies ALL GAD core symptoms...")
        for s in gad_cores:
             m.answer_question(s, 'NO')
             
        print(f"Candidates after GAD denial: {m.state.active_candidates}")
        if 'gad' not in m.state.active_candidates:
            print("  SUCCESS: GAD Pruned.")
        else:
            print("  FAILURE: GAD still active.")

if __name__ == "__main__":
    debug_pruning()
