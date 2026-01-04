
import heapq
from typing import List, Dict, Tuple, Set
from .manager import SessionManager

class Node:
    def __init__(self, answered: Set[str], cost: int, heuristic: float):
        self.answered = answered # Set of symptom IDs
        self.cost = cost         # g(n): Questions asked
        self.heuristic = heuristic # h(n): Est. remaining
        
    def __lt__(self, other):
        return (self.cost + self.heuristic) < (other.cost + other.heuristic)

class DiagnosticSearch:
    def __init__(self, manager: SessionManager):
        self.manager = manager
        
    def get_next_best_question(self, current_candidates: List[str]) -> str:
        """
        Run A* (or a simplified greedy version for response time) to find best next symptom.
        
        Full A* on the symptom space is O(2^N). We need a heuristic-guided greedy approach
        or limited-depth A* for real-time interaction.
        """
        
        # 1. Identify all potential questions
        potential_questions = self._get_all_potential_questions(current_candidates)
        
        if not potential_questions:
            return None
            
        # 2. Heuristic Scoring
        # Score = Information Gain Potential / Cost
        # Simple clinical heuristic:
        # - High priority: Core symptoms of most likely candidates
        # - Medium priority: Exclusions if diagnosis is close
        # - Low: Differentiators
        
        best_q = None
        best_score = -float('inf')
        
        for q in potential_questions:
            score = self.calculate_heuristic_score(q, current_candidates)
            if score > best_score:
                best_score = score
                best_q = q
                
        return best_q

    def calculate_heuristic_score(self, question: str, candidates: List[str]) -> float:
        """
        Estimate utility of asking 'question'.
        Higher is better.
        """
        score = 0

        # Age Threshold/Range Questions - high priority if age unknown
        if "_age_threshold_check" in question or "_age_range_check" in question:
            if not hasattr(self.manager.state, 'age') or self.manager.state.age is None:
                return 500  # High priority - need age for correct symptom counts
            else:
                return 0  # Already know age, not needed

        # Check if Verification Question (Exclusion, Duration, Subjective, or Onset)
        is_verification = "_exc_" in question or "_duration_" in question or "_subj_" in question or "_onset_" in question
        
        if is_verification:
            # Find which disorder this belongs to
            # Heuristic: Match prefix or lookup
            relevant_d = None
            for d in candidates:
                if d in question: # simple string match for prototype {d}_exc...
                    relevant_d = d
                    break
            
            if relevant_d:
                # Check how many symptoms met for this disorder
                met_count = 0
                syms = self.manager.get_candidate_symptoms(relevant_d)
                for s in syms:
                    if self.manager.state.answered_questions.get(s) == 'present':
                        met_count += 1
                
                # If we have gathered significant evidence (>4 symptoms), prioritise verification
                # (GAD requires 3 associated + 1 core = 4 total)
                if met_count >= 4:
                    return 1000 + met_count # Massive boost to finish the diagnosis
                else:
                    return 0 # varied low priority
            
            return 0

        # Standard Symptom Scoring
        query = f"symptom(D, {question}, Cat, _)"
        res = list(self.manager.prolog.query(query))
        
        relevant_count = 0
        is_core = False
        
        for sol in res:
            d_id = str(sol['D'])
            cat = str(sol['Cat'])
            
            if d_id in candidates:
                relevant_count += 1
                if cat in ['core', 'criterion_a', 'essential']:
                    is_core = True
        
        score += relevant_count * 10 
        if is_core:
            score += 50
            
        return score
        
    def _get_all_potential_questions(self, candidates: List[str]) -> Set[str]:
        """
        Expand search space to include symptoms, exclusions, and duration checks.
        """
        potential = set()
        
        # 1. Symptoms
        for d in candidates:
            # Symptoms
            syms = self.manager.get_candidate_symptoms(d)
            for s in syms:
                if s not in self.manager.state.answered_questions:
                    potential.add(s)
            
            # 2. Exclusions (Need to query schema)
            # exclusion_criterion(DisorderID, ExclusionID, Type, Desc)
            q_ex = f"exclusion_criterion({d}, ExID, _, _)"
            for sol in self.manager.prolog.query(q_ex):
                ex_id = str(sol['ExID'])
                # Check if answered (in exclusion status)
                # We need a unified 'answered_questions' tracking for this
                # For now, check if we have a fact for it?
                # Simpler: check manager.state.answered_questions using ID
                if ex_id not in self.manager.state.answered_questions:
                    potential.add(ex_id)
            
            # 3. Duration
            # duration_requirement(DisorderID, _, _)
            # map to pseudo-id: {d}_duration
            dur_id = f"{d}_duration_check"
            if dur_id not in self.manager.state.answered_questions:
                # Only add if we have core symptoms?
                potential.add(dur_id)
                
            # 4. Subjective Criteria
            # subjective_criterion(DisorderID, CritID, Desc, Type)
            q_subj = f"subjective_criterion({d}, SubjID, _, _)"
            for sol in self.manager.prolog.query(q_subj):
                subj_id = str(sol['SubjID'])
                if subj_id not in self.manager.state.answered_questions:
                    potential.add(subj_id)

            # 5. Onset Age Requirements
            # onset_requirement(DisorderID, before_age, MaxAge)
            onset_age_id = f"{d}_onset_age_check"
            if onset_age_id not in self.manager.state.answered_questions:
                q_onset = f"onset_requirement({d}, before_age, _)"
                if list(self.manager.prolog.query(q_onset)):
                    potential.add(onset_age_id)

            # 6. Onset Event Requirements
            # onset_requirement(DisorderID, after_event, EventType)
            onset_event_id = f"{d}_onset_event_check"
            if onset_event_id not in self.manager.state.answered_questions:
                q_onset = f"onset_requirement({d}, after_event, _)"
                if list(self.manager.prolog.query(q_onset)):
                    potential.add(onset_event_id)

            # 7. Age Threshold Questions (for age_adjusted_count disorders like ADHD)
            # Only ask if patient age is not yet known
            if not hasattr(self.manager.state, 'age') or self.manager.state.age is None:
                age_threshold_id = f"{d}_age_threshold_check"
                if age_threshold_id not in self.manager.state.answered_questions:
                    q_age = f"age_adjusted_count({d}, _, _, _)"
                    if list(self.manager.prolog.query(q_age)):
                        potential.add(age_threshold_id)

            # 8. Age Range Questions (for disorder_age_range like PTSD preschool)
            # Only ask if patient age is not yet known AND max age is meaningful (< 100)
            if not hasattr(self.manager.state, 'age') or self.manager.state.age is None:
                age_range_id = f"{d}_age_range_check"
                if age_range_id not in self.manager.state.answered_questions:
                    q_range = f"disorder_age_range({d}, _, MaxAge)"
                    res = list(self.manager.prolog.query(q_range))
                    if res:
                        max_age = int(res[0]['MaxAge'])
                        # Only generate question if max age is meaningful (< 100)
                        if max_age < 100:
                            potential.add(age_range_id)

        return potential
