
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Set
from pyswip import Prolog

# Manual mapping of semantically equivalent symptoms across disorders
# In a real system, this would be computed via LLM semantic similarity or ontology
SYMPTOM_ALIASES = {
    # Sleep disturbance
    'mdd_a4': ['gad_c4', 'ptsd_e6'], 
    'gad_c4': ['mdd_a4', 'ptsd_e6'],
    'ptsd_e6': ['mdd_a4', 'gad_c4'],
    
    # Concentration
    'mdd_a8': ['gad_c3', 'ptsd_e5', 'adhd_a1f'],
    'gad_c3': ['mdd_a8', 'ptsd_e5', 'adhd_a1f'],
    'ptsd_e5': ['mdd_a8', 'gad_c3', 'adhd_a1f'],
    'adhd_a1f': ['mdd_a8', 'gad_c3', 'ptsd_e5'],
    
    # Fatigue
    'mdd_a6': ['gad_c2'],
    'gad_c2': ['mdd_a6'],
    
    # Irritability
    'gad_c1': ['mdd_a1', 'ptsd_e1'], # MDD A1 'irritable' in children note
    'ptsd_e1': ['gad_c1'],
    
    # Restlessness
    'mdd_a5': ['gad_c1'], # Psychomotor agitation ~= restlessness
    'gad_c1': ['mdd_a5'],
}

@dataclass
class DiagnosticState:
    patient_id: str = "current_patient"
    answered_questions: Dict[str, str] = field(default_factory=dict) # symptom_id -> answer (yes/no)
    confirmed_diagnoses: List[str] = field(default_factory=list)
    excluded_diagnoses: List[str] = field(default_factory=list)
    active_candidates: List[str] = field(default_factory=list)

class SessionManager:
    def __init__(self):
        self.prolog = Prolog()
        self.state = DiagnosticState()
        self._load_knowledge_base()

    def _load_knowledge_base(self):
        """Load Prolog schema and gold standard data."""
        # Assuming run from project root
        self.prolog.consult("src/prolog/schema.pl")
        self.prolog.consult("src/prolog/gold_standard/loader.pl")
        
    def set_patient_data(self, age: Optional[int] = None, gender: str = 'unknown'):
        """
        Set patient demographic data.

        Args:
            age: Patient's age in years. If None, age will be determined via
                 threshold questions during the diagnostic session.
            gender: Patient's gender (default: 'unknown')
        """
        self.state.gender = gender
        list(self.prolog.query(f"assertz(patient_context({self.state.patient_id}, gender, '{gender}'))"))

        if age is not None:
            self.state.age = age
            list(self.prolog.query(f"assertz(patient_context({self.state.patient_id}, age, {age}))"))
            # Filter candidates by age (e.g., ptsd_preschool only for children ≤6)
            self._prune_candidates_by_age(age)
        else:
            # Age unknown - will be determined via threshold questions
            self.state.age = None

    def _prune_candidates_by_age(self, age: int):
        """Remove age-inappropriate disorders from active candidates."""
        filtered = []
        for disorder in self.state.active_candidates:
            # Check if disorder has age restriction
            query = f"disorder_age_range({disorder}, MinAge, MaxAge)"
            res = list(self.prolog.query(query))
            if res:
                min_age = int(res[0]['MinAge'])
                max_age = int(res[0]['MaxAge'])
                if min_age <= age <= max_age:
                    filtered.append(disorder)
                else:
                    print(f"  [Age Filter] Removing {disorder} (age {age} not in {min_age}-{max_age})")
            else:
                # No age restriction defined, keep it
                filtered.append(disorder)
        self.state.active_candidates = filtered
    
    def start_new_session(self):
        """Reset state for a new patient."""
        self.state = DiagnosticState()
        # Clear dynamic facts in Prolog
        list(self.prolog.query("clear_all_patient_facts"))
        
        # Initial candidates = all disorders
        candidates = [sol['X'] for sol in self.prolog.query("disorder(X, _, _)")]
        self.state.active_candidates = candidates

    def answer_question(self, question_id: str, answer_str: str, confidence: float = 0.9):
        """
        Register a user's answer.
        answer_str: "YES", "NO", "UNKNOWN" (or similar variations)
        confidence: LLM confidence in the answer (0.0-1.0), defaults to 0.9
        """
        # Normalize input
        ans = answer_str.strip().upper()
        
        # Record raw answer for internal state tracking
        self.state.answered_questions[question_id] = ans
        
        # Determine Prolog Status based on Schema Logic
        # 1. Symptoms: present, absent, unknown
        # 2. Exclusions: cleared (if YES to 'not attributable' or NO to 'is attributable'), excluded, unknown
        # 3. subjective: met, not_met, unclear
        # 4. duration: (numeric assertion)
        
        if "_duration_check" in question_id:
            disorder = question_id.replace("_duration_check", "")
            if ans == "YES":
                self._assert_duration(disorder)
            # If NO or UNKNOWN, we just don't assert duration (stays undefined/missing)
        
        elif "_exc_" in question_id:
            # Logic: Input is likely "Is it true X is NOT Y?" -> YES = Cleared
            status = 'unknown'
            if ans == 'YES': status = 'cleared'
            elif ans == 'NO': status = 'excluded'
            elif ans == 'UNKNOWN': status = 'unknown'
            
            q_ass = f"assertz(patient_exclusion_status({self.state.patient_id}, {question_id}, {status}))"
            list(self.prolog.query(q_ass))
            
        elif "_subj_" in question_id:
            status = 'unclear'
            if ans == 'YES': status = 'met'
            elif ans == 'NO': status = 'not_met'

            q_ass = f"assertz(subjective_assessment({self.state.patient_id}, {question_id}, {status}, {confidence}))"
            list(self.prolog.query(q_ass))
        
        elif "_onset_age_check" in question_id:
            # Onset Age Check
            # Question: "Did symptoms appear before age X?"
            disorder = question_id.replace("_onset_age_check", "")
            q_req = f"onset_requirement({disorder}, before_age, MaxAge)"
            res = list(self.prolog.query(q_req))
            if res:
                max_age = int(res[0]['MaxAge'])
                if ans == 'YES':
                    # Assert an age that meets criteria (e.g. MaxAge - 1)
                    q_ass = f"assertz(patient_onset_age({self.state.patient_id}, {max_age - 1}))"
                    list(self.prolog.query(q_ass))
                elif ans == 'NO':
                    # Assert an age that fails criteria (e.g. MaxAge + 1)
                    q_ass = f"assertz(patient_onset_age({self.state.patient_id}, {max_age + 5}))"
                    list(self.prolog.query(q_ass))

        elif "_onset_event_check" in question_id:
            # Onset Event Check
            # Question: "Did this occur after [Event]?"
            disorder = question_id.replace("_onset_event_check", "")
            q_req = f"onset_requirement({disorder}, after_event, Type)"
            res = list(self.prolog.query(q_req))
            if res:
                event_type = str(res[0]['Type'])
                status = 'present' if ans == 'YES' else 'absent'
                q_ass = f"assertz(patient_context({self.state.patient_id}, {event_type}, {status}))"
                list(self.prolog.query(q_ass))

        elif "_settings_check" in question_id:
            # Settings Check (DSM-5 Criterion C for ADHD)
            # Question: "Are symptoms present in 2+ settings?"
            if ans == 'YES':
                # Assert 2 settings to meet requirement
                list(self.prolog.query(f"assertz(patient_context({self.state.patient_id}, setting, home))"))
                list(self.prolog.query(f"assertz(patient_context({self.state.patient_id}, setting, school))"))
            elif ans == 'NO':
                # Only 1 setting - fails requirement
                list(self.prolog.query(f"assertz(patient_context({self.state.patient_id}, setting, home))"))
            # UNKNOWN leaves it as missing_data (no assertion)

        elif "_age_threshold_check" in question_id:
            # Age Threshold Check (e.g., adhd_age_threshold_check)
            # Question: "Is the patient 17 years or older?"
            disorder = question_id.replace("_age_threshold_check", "")
            q_threshold = f"age_adjusted_count({disorder}, _, Threshold, _)"
            res = list(self.prolog.query(q_threshold))
            if res:
                threshold = int(res[0]['Threshold'])
                if ans == 'YES':
                    inferred_age = threshold  # At or above threshold
                elif ans == 'NO':
                    inferred_age = threshold - 1  # Below threshold
                else:
                    inferred_age = None

                if inferred_age is not None:
                    self.state.age = inferred_age
                    list(self.prolog.query(f"assertz(patient_context({self.state.patient_id}, age, {inferred_age}))"))

        elif "_age_range_check" in question_id:
            # Age Range Check (e.g., ptsd_preschool_age_range_check)
            # Question: "Is the patient 6 years or younger?"
            disorder = question_id.replace("_age_range_check", "")
            q_range = f"disorder_age_range({disorder}, _, MaxAge)"
            res = list(self.prolog.query(q_range))
            if res:
                max_age = int(res[0]['MaxAge'])
                if ans == 'YES':
                    inferred_age = max_age  # Within range
                elif ans == 'NO':
                    inferred_age = max_age + 1  # Outside range
                else:
                    inferred_age = None

                if inferred_age is not None:
                    self.state.age = inferred_age
                    list(self.prolog.query(f"assertz(patient_context({self.state.patient_id}, age, {inferred_age}))"))
                    self._prune_candidates_by_age(inferred_age)

        else:
            # Standard Symptom
            status = 'unknown'
            if ans == 'YES': status = 'present'
            elif ans == 'NO': status = 'absent'
            
            self._record_answer(question_id, status)
            
            # Answer aliases (Only if definite YES/NO)
            if status in ['present', 'absent'] and question_id in SYMPTOM_ALIASES:
                for alias in SYMPTOM_ALIASES[question_id]:
                    if alias not in self.state.answered_questions:
                        print(f"    (Auto-answering alias {alias} as {status})")
                        self._record_answer(alias, status)
        
        self._update_diagnostic_status()

    def _assert_duration(self, disorder):
        q_req = f"duration_requirement({disorder}, Min, Unit)"
        res = list(self.prolog.query(q_req))
        if res:
            unit = str(res[0]['Unit'])
            val = int(res[0]['Min'])
            days = val * 30 if unit == 'months' else val * 7 if unit == 'weeks' else val
            q_ass = f"assertz(patient_duration({self.state.patient_id}, {disorder}, {days}))"
            list(self.prolog.query(q_ass))

    def _record_answer(self, symptom_id: str, status: str):
        self.state.answered_questions[symptom_id] = status
        query = f"assertz(patient_symptom({self.state.patient_id}, {symptom_id}, {status}, 'User reported via CLI'))"
        list(self.prolog.query(query))

    def _update_diagnostic_status(self):
        """
        Prune candidates that are strictly ruled out.
        - Exclusions met (status='excluded')
        - Core symptoms definitively absent (all cores = absent)
        """
        # We must iterate over a copy since we modify the list
        current_candidates = list(self.state.active_candidates)
        remaining_candidates = []
        
        for d in current_candidates:
            keep = True
            
            # 1. Check Exclusion Status
            # If any exclusion is 'excluded' (meaning the exclusion criteria applies), drop it.
            # e.g. patient_exclusion_status(P, ExcID, excluded).
            q_ex = f"exclusion_criterion({d}, ExcID, _, _), patient_exclusion_status({self.state.patient_id}, ExcID, excluded)"
            if list(self.prolog.query(q_ex)):
                 print(f"  [Pruning] Dropping {d}: Exclusion met.")
                 keep = False
                 
            # 2. Check Core Symptom Failure
            # Only prune if ALL core symptoms are ABSENT.
            # Get core symptoms for this disorder
            if keep:
                q_core = f"symptom({d}, SID, Category, _), member(Category, [core, criterion_a, essential])"
                core_symptoms = [str(sol['SID']) for sol in self.prolog.query(q_core)]
                
                if core_symptoms:
                    # Check if ALL are absent
                    all_absent = True
                    for s in core_symptoms:
                        # Check absence
                        q_abs = f"patient_symptom({self.state.patient_id}, {s}, absent, _)"
                        if not list(self.prolog.query(q_abs)):
                            all_absent = False # At least one is not absent (present or unknown)
                            break
                    
                    if all_absent:
                        print(f"  [Pruning] Dropping {d}: All core symptoms absent.")
                        keep = False

            if keep:
                remaining_candidates.append(d)
                
        self.state.active_candidates = remaining_candidates

    def get_candidate_symptoms(self, disorder_id: str) -> List[str]:
        """Get all symptoms for a disorder."""
        q = f"symptoms_for_disorder({disorder_id}, Symptoms)"
        res = list(self.prolog.query(q))
        if res:
             # PySwip returns atoms/dicts
             symptoms_struct = res[0]['Symptoms']
             # Extract IDs. This depends on how pyswip returns the struct list
             # Usually a list of dicts if structured
             return [str(s['id']) for s in symptoms_struct]
        return []

    def get_symptom_description(self, question_id: str) -> str:
        """Fetch description for UI."""
        
        if "_duration_check" in question_id:
            disorder = question_id.replace("_duration_check", "")
            return f"Have these symptoms persisted for the required duration (usually > 2 weeks or 6 months depending on disorder)?"
            
        if "_exc_" in question_id:
            # Exclusion
            q = f"exclusion_criterion(_, {question_id}, _, Desc)"
            res = list(self.prolog.query(q))
            if res:
                return f"Is it true that: {res[0]['Desc']}?"
            return f"Is this explained by exclusion {question_id}?"
            
        if "_subj_" in question_id:
            q = f"subjective_criterion(_, {question_id}, Desc, _)"
            res = list(self.prolog.query(q))
            if res:
                return f"Do you adhere to this criterion: {res[0]['Desc']}?"
            return f"Do you meet subjective criterion {question_id}?"
            
        if "_onset_age_check" in question_id:
            disorder = question_id.replace("_onset_age_check", "")
            q = f"onset_requirement({disorder}, before_age, MaxAge)"
            res = list(self.prolog.query(q))
            if res:
                return f"Is it true that: Several symptoms were present prior to age {res[0]['MaxAge']}?"
            return f"Did symptoms start early?"

        if "_onset_event_check" in question_id:
            disorder = question_id.replace("_onset_event_check", "")
            q = f"onset_requirement({disorder}, after_event, Type)"
            res = list(self.prolog.query(q))
            if res:
                return f"Is it true that: The disturbance occurred after a {res[0]['Type']}?"
            return f"Did this follow a specific event?"

        if "_settings_check" in question_id:
            disorder = question_id.replace("_settings_check", "")
            q = f"setting_requirement({disorder}, MinSettings)"
            res = list(self.prolog.query(q))
            if res:
                min_settings = int(res[0]['MinSettings'])
                return f"Is it true that: Symptoms are present in {min_settings} or more settings (e.g., at home, school/work, with friends)?"
            return f"Are symptoms present in multiple settings?"

        if "_age_threshold_check" in question_id:
            disorder = question_id.replace("_age_threshold_check", "")
            q = f"age_adjusted_count({disorder}, _, Threshold, _)"
            res = list(self.prolog.query(q))
            if res:
                threshold = int(res[0]['Threshold'])
                return f"Is the patient {threshold} years or older?"
            return f"Is the patient an adult?"

        if "_age_range_check" in question_id:
            disorder = question_id.replace("_age_range_check", "")
            q = f"disorder_age_range({disorder}, _, MaxAge)"
            res = list(self.prolog.query(q))
            if res:
                max_age = int(res[0]['MaxAge'])
                return f"Is the patient {max_age} years or younger?"
            return f"Is the patient within the applicable age range?"

        # Standard Symptom
        q = f"symptom(_, {question_id}, _, Desc)"
        res = list(self.prolog.query(q))
        if res:
            return str(res[0]['Desc'])
        return f"Do you have {question_id}?"
