
import json
import argparse
from pathlib import Path
from typing import List, Dict
from src.search.manager import SessionManager
from src.search.search import DiagnosticSearch

def load_vignettes(path: Path) -> List[Dict]:
    with open(path, 'r') as f:
        return json.load(f)

from src.extraction.providers import get_provider
from src.extraction.config import Config

class ClinicalAnalyzer:
    def __init__(self, vignette: Dict, provider_name: str = "openai"):
        self.clinical_text = vignette.get('clinical_text', '')
        self.provider = get_provider(provider_name)
        
    def answer(self, question_text: str) -> str:
        """
        Determine if the clinical text supports the given question/criterion.
        Returns: "YES", "NO", "UNKNOWN"
        """
        prompt = f"""You are an expert clinical psychiatrist performing a structured assessment.
Analyze the following patient vignette:

"{self.clinical_text}"

Question: Does the patient's presentation meet the following criterion: "{question_text}"?

Instructions:
- Answer YES if the text provides clear evidence for this symptom/criterion.
- Answer NO if the text explicitly denies this, or if the opposite is true.
- Answer UNKNOWN if there is no information in the text to confirm or deny this. Do not guess.

Special Logic for Verification Questions ("Is it true that..."):
- These define conditions that must be TRUE for the diagnosis to hold.
- Example: "Is it true that the disturbance is NOT attributable to physiological effects of a substance?"
  - If text says "Patient denies drug use" -> Answer YES (The statement is true).
  - If text says "Patient uses cocaine daily" -> Answer NO (The statement is false).
  - If text mentions nothing about drugs -> Answer UNKNOWN (Cannot verify).

Output ONLY the single word: YES, NO, or UNKNOWN.
"""
        try:
            # We use the provider to simulate the patient "thinking"
            # Using low thinking budget/effort for speed if supported, or just standard
            result = self.provider.extract(
                dsm5_text=prompt,
                disorder_id="patient_sim",
                template_guide="",
                custom_prompt=prompt,
                custom_system_prompt="You are a helpful patient. Answer efficiently."
            )
            
            if not result.success:
                return "UNKNOWN"
                
            ans = result.content.strip().upper()
            if "YES" in ans:
                return "YES"
            if "NO" in ans:
                return "NO"
            return "UNKNOWN"
            
        except Exception as e:
            print(f"Simulator Error: {e}")
            return False

def run_benchmark(vignette_path: Path):
    vignettes = load_vignettes(vignette_path)
    manager = SessionManager()
    search = DiagnosticSearch(manager)
    
    # Initialize simulator with default provider
    # Ideally passed via args, defaulting to openai for now
    
    results = []
    
    print(f"Benchmarking on {len(vignettes)} cases using Clinical Expert Agent...")
    
    for case in vignettes:
        manager.start_new_session()
        sim = ClinicalAnalyzer(case)
        
        questions_asked = 0
        max_questions = 50
        
        print(f"\nProcessing Case {case.get('id')}...")
        
        while questions_asked < max_questions:
            # 1. Prediction Check (Simplified for prototype)
            # In real implementations, manager would expose 'get_current_diagnosis()'
            # Here we check if any candidate is 'Simulated Confirmed' 
            # (In reality, search loop ends when certainty threshold met)
            
            # For this prototype: we stop if we have asked enough questions or found a match
            # Let's break if we have > 5 positive core symptoms for any disorder (heuristic)
            
            # 2. Get Next Question
            next_q_id = search.get_next_best_question(manager.state.active_candidates)
            if not next_q_id:
                print("  No more questions recommended.")
                break
            
            # 3. Simulate Answer
            # Convert ID to Text
            q_text = manager.get_symptom_description(next_q_id)
            
            # Ask LLM
            ans_str = sim.answer(q_text)
            
            print(f"  Q{questions_asked+1}: {q_text[:40]}...? -> {ans_str}")
            
            # Update State
            manager.answer_question(next_q_id, ans_str)
            questions_asked += 1
            
        # Validate Final Status with Prolog and Get Confidence
        # This is where we show the "Tier C" Integration
        print("\n  Final Diagnostic Analysis:")
        best_candidate = "undetermined"
        best_conf = 0.0
        
        for d in manager.state.active_candidates:
            # Query: diagnosis_candidate(PatientID, DisorderID, Confidence)
            q = f"diagnosis_candidate('{manager.state.patient_id}', {d}, Conf)"
            res = list(manager.prolog.query(q))
            
            if res:
                conf = float(res[0]['Conf'])
                print(f"  - {d.upper()}: Confidence = {conf:.2f}")
                
                # Check criteria breakdowns
                # criterion_status(PatientID, DisorderID, Type, Status)
                met_count = 0
                required = ['symptoms', 'duration', 'onset', 'exclusions']
                status_str = []
                for r in required:
                    q_status = f"criterion_status('{manager.state.patient_id}', {d}, {r}, St)"
                    s_res = list(manager.prolog.query(q_status))
                    if s_res:
                        st = s_res[0]['St']
                        status_str.append(f"{r}={st}")
                        if st == "met": met_count += 1
                
                print(f"    Details: {', '.join(status_str)}")
                
                if conf > best_conf: # Simple max confidence selection
                    best_conf = conf
                    best_candidate = d
            else:
                 # If diagnosis_candidate failed, it means absolute criteria (exclusions/duration) definitely failed
                 pass

        print(f"  > Decision: {best_candidate.upper()}")
        
        results.append({
            "id": case.get('id'),
            "ground_truth": case.get('ground_truth'),
            "predicted": best_candidate,
            "questions": questions_asked
        })
        
    # Stats
    print("\nResults:")
    correct = 0
    total = len(results)
    
    for r in results:
        match_symbol = "✓" if r['ground_truth'] == r['predicted'] else "✗"
        if match_symbol == "✓": correct += 1
        print(f"[{match_symbol}] Case {r['id']}: GT={r['ground_truth']}, Pred={r['predicted']} (Q={r['questions']})")
        
    print(f"\nAccuracy: {correct}/{total} ({correct/total*100:.1f}%)")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("vignettes", type=Path)
    args = parser.parse_args()
    
    run_benchmark(args.vignettes)
