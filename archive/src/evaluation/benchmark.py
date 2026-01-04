
import json
import argparse
from pathlib import Path
from typing import List, Dict
from src.search.manager import SessionManager
from src.search.search import DiagnosticSearch

def load_vignettes(vignettes_dir: Path = None) -> List[Dict]:
    """Load all vignette files from the vignettes directory."""
    if vignettes_dir is None:
        # Default to data/vignettes from project root
        vignettes_dir = Path(__file__).parent.parent.parent / 'data' / 'vignettes'

    all_vignettes = []
    # Load all vignettes_*.json files
    for vfile in sorted(vignettes_dir.glob("vignettes_*.json")):
        with open(vfile, 'r') as f:
            vigs = json.load(f)
            all_vignettes.extend(vigs)
            print(f"Loaded {len(vigs)} vignettes from {vfile.name}")

    return all_vignettes

from src.extraction.providers import get_provider
from src.extraction.config import Config

class ClinicalAnalyser:
    def __init__(self, vignette: Dict, provider_name: str = "openai"):
        self.clinical_text = vignette.get('clinical_text', '')
        self.provider = get_provider(provider_name)
        
    def answer(self, question_text: str) -> str:
        """
        Determine if the clinical text supports the given question/criterion.
        Returns: "YES", "NO", "UNKNOWN"
        """
        prompt = f"""You are an expert clinical psychiatrist performing a structured assessment.
Analyse the following patient vignette:

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
  - If text says "Patient denies drug use" -> Answer YES (The statement is true).
  - If text says "Patient uses cocaine daily" -> Answer NO (The statement is false).
  - If text mentions nothing about the excluded condition -> Answer YES (Presume the condition is absent if not mentioned).

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
                print(f"LLM Failure: {result.error}")
                return "UNKNOWN"
                
            ans = result.content.strip().upper()
            # print(f"DEBUG LLM: {ans}")
            
            if "YES" in ans:
                return "YES"
            if "NO" in ans:
                return "NO"
            return "UNKNOWN"

        except Exception as e:
            print(f"Simulator Error: {e}")
            return "UNKNOWN"

    def answer_with_confidence(self, question_text: str) -> tuple:
        """
        Determine if the clinical text supports the given question/criterion.
        Returns: (answer, confidence) where answer is "YES"/"NO"/"UNKNOWN" and confidence is 0.0-1.0
        """
        prompt = f"""You are an expert clinical psychiatrist performing a structured assessment.
Analyse the following patient vignette:

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
  - If text mentions nothing about the excluded condition -> Answer YES (Presume the condition is absent if not mentioned).

Output your response in exactly this format:
ANSWER: [YES/NO/UNKNOWN]
CONFIDENCE: [0.0-1.0]

Where CONFIDENCE is your certainty in the answer:
- 1.0 = Absolutely certain, explicit clear evidence
- 0.8-0.9 = Very confident, strong evidence
- 0.6-0.7 = Moderately confident, reasonable inference
- 0.4-0.5 = Uncertain, weak evidence
- 0.1-0.3 = Very uncertain, mostly guessing
"""
        try:
            result = self.provider.extract(
                dsm5_text=prompt,
                disorder_id="patient_sim",
                template_guide="",
                custom_prompt=prompt,
                custom_system_prompt="You are a helpful clinical expert. Answer efficiently."
            )

            if not result.success:
                print(f"LLM Failure: {result.error}")
                return ("UNKNOWN", 0.5)

            content = result.content.strip().upper()

            # Parse answer
            answer = "UNKNOWN"
            if "ANSWER:" in content:
                answer_line = [l for l in content.split('\n') if "ANSWER:" in l]
                if answer_line:
                    ans_part = answer_line[0].split("ANSWER:")[-1].strip()
                    if "YES" in ans_part:
                        answer = "YES"
                    elif "NO" in ans_part:
                        answer = "NO"
            elif "YES" in content:
                answer = "YES"
            elif "NO" in content:
                answer = "NO"

            # Parse confidence
            confidence = 0.7  # Default if not found
            if "CONFIDENCE:" in content:
                conf_line = [l for l in content.split('\n') if "CONFIDENCE:" in l]
                if conf_line:
                    conf_part = conf_line[0].split("CONFIDENCE:")[-1].strip()
                    try:
                        # Extract first number found
                        import re
                        conf_match = re.search(r'(\d+\.?\d*)', conf_part)
                        if conf_match:
                            confidence = float(conf_match.group(1))
                            # Ensure in range 0-1
                            if confidence > 1.0:
                                confidence = confidence / 100.0  # Handle if given as percentage
                            confidence = max(0.0, min(1.0, confidence))
                    except ValueError:
                        pass

            return (answer, confidence)

        except Exception as e:
            print(f"Simulator Error: {e}")
            return ("UNKNOWN", 0.5)

def run_benchmark(vignettes_dir: Path = None, provider_name: str = "ollama"):
    vignettes = load_vignettes(vignettes_dir)
    manager = SessionManager()
    search = DiagnosticSearch(manager)

    results = []

    print(f"Benchmarking on {len(vignettes)} cases using Clinical Expert Agent (Provider: {provider_name})...")

    for case in vignettes:
        manager.start_new_session()

        # Extract and Set Age
        import re
        demo = case.get('demographics', '')
        age_match = re.search(r'(\d+)-year-old', demo)
        if age_match:
            age = int(age_match.group(1))
            manager.set_patient_data(age)
            print(f"  [Info] Patient Age set to {age}")
        else:
            # Don't default to 30 - let threshold questions determine age
            print("  [Info] Age not found in vignette, will ask threshold questions")
            manager.set_patient_data(age=None)

        sim = ClinicalAnalyser(case, provider_name=provider_name)
        
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
                print(f"  - {d.upper()}: FAILED")
                reasons = []
                for c_type in ['symptoms', 'duration', 'onset', 'exclusions', 'subjective']:
                    # Query status
                    q_status = f"criterion_status('{manager.state.patient_id}', {d}, {c_type}, St)"
                    s_res = list(manager.prolog.query(q_status))
                    if s_res:
                        st = s_res[0]['St']
                        if st != 'met':
                            reasons.append(f"{c_type}={st}")
                    else:
                        reasons.append(f"{c_type}=query_failed")
                
                if reasons:
                    print(f"    (Reasons: {', '.join(reasons)})")

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
    parser.add_argument("--vignettes-dir", type=Path, default=None,
                        help="Directory containing vignette files (default: data/vignettes)")
    parser.add_argument("--provider", type=str, default="ollama",
                        help="LLM provider to use (ollama, openai, anthropic)")
    args = parser.parse_args()

    run_benchmark(args.vignettes_dir, provider_name=args.provider)
