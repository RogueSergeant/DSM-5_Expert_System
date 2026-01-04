"""
Batch Experiment - Orchestrates batch vs sequential question-answering comparison.

This module implements the experimental framework for testing batch processing
against sequential question answering. It supports both adaptive (with A* search)
and non-adaptive (all-at-once) batching strategies.

Author: Alfie Roberts
Date: January 2026
"""

import time
import re
from pathlib import Path
from typing import List, Dict, Optional
from tqdm import tqdm
from src.evaluation.benchmark import load_vignettes, ClinicalAnalyser
from src.evaluation.batch_analyser import BatchClinicalAnalyser
from src.search.manager import SessionManager
from src.search.search import DiagnosticSearch


class BatchExperiment:
    """
    Orchestrates experiments comparing batch vs sequential question answering.

    Tests multiple batch sizes (adaptive and non-adaptive) against sequential
    baseline to measure accuracy, speed, and question efficiency trade-offs.
    """

    def __init__(
        self,
        providers: List[str] = None,
        num_vignettes: int = 10,
        vignettes_file: Optional[str] = None
    ):
        """
        Initialise experiment.

        Args:
            providers: List of LLM providers to test ("ollama", "openai", "anthropic")
            num_vignettes: Number of vignettes to test on
            vignettes_file: Specific vignette file to load, or None for all
        """
        self.providers = providers or ["ollama", "openai"]
        self.num_vignettes = num_vignettes
        self.vignettes = self._load_vignettes(vignettes_file)

    def _load_vignettes(self, vignettes_file: Optional[str]) -> List[Dict]:
        """
        Load vignettes for testing.

        Args:
            vignettes_file: Specific file path or None for all

        Returns:
            List of vignette dicts
        """
        if vignettes_file:
            vignettes_dir = Path(vignettes_file).parent
        else:
            vignettes_dir = Path(__file__).parent.parent.parent / 'data' / 'vignettes'

        vignettes = load_vignettes(vignettes_dir)
        print(f"Loaded {len(vignettes)} vignettes")
        return vignettes

    def run_full_experiment(self) -> Dict:
        """
        Run complete experiment across all providers and batch sizes.

        Returns:
            {
                "metadata": {...},
                "results": {
                    "provider_name": {
                        "sequential": [...],
                        "batch_2": [...],
                        "batch_5": [...],
                        "batch_all": [...]
                    },
                    ...
                }
            }
        """
        results = {
            "metadata": self._get_metadata(),
            "results": {}
        }

        for provider in self.providers:
            print(f"\n{'='*60}")
            print(f"Testing Provider: {provider.upper()}")
            print(f"{'='*60}")

            provider_results = {}

            # Mode 1: Sequential (baseline)
            print(f"\nMode 1: Sequential (baseline) [ADAPTIVE]")
            provider_results["sequential"] = self._run_sequential_mode(provider)

            # Mode 2: Small batches (adaptive with A* search)
            print(f"\nMode 2: Small Batches (batch_size=5) [ADAPTIVE]")
            provider_results["batch_5"] = self._run_batch_mode(
                provider, batch_size=5, adaptive=True
            )

            # Mode 3: Large batches (adaptive with A* search)
            print(f"\nMode 3: Large Batches (batch_size=10) [ADAPTIVE]")
            provider_results["batch_10"] = self._run_batch_mode(
                provider, batch_size=10, adaptive=True
            )

            # Mode 4: All-at-once (non-adaptive)
            print(f"\nMode 4: All-at-Once [NON-ADAPTIVE]")
            provider_results["batch_all"] = self._run_batch_mode(
                provider, batch_size=None, adaptive=False
            )

            results["results"][provider] = provider_results

        return results

    def _get_metadata(self) -> Dict:
        """Get experiment metadata."""
        import datetime
        return {
            "timestamp": datetime.datetime.now().isoformat(),
            "num_vignettes": self.num_vignettes,
            "providers": self.providers,
            "vignette_ids": [v["id"] for v in self.vignettes[:self.num_vignettes]]
        }

    def _run_sequential_mode(self, provider: str) -> List[Dict]:
        """
        Run baseline sequential mode (1 question per LLM call).

        Args:
            provider: LLM provider name

        Returns:
            List of per-vignette results
        """
        mode_results = []

        vignettes_subset = self.vignettes[:self.num_vignettes]
        pbar = tqdm(
            vignettes_subset,
            desc=f"  Sequential ({provider})",
            unit="vignette",
            leave=True
        )

        for vignette in pbar:
            result = self._process_vignette_sequential(vignette, provider)
            mode_results.append(result)

            # Update progress bar with latest stats
            pbar.set_postfix({
                'questions': result['num_questions'],
                'time': f"{result['duration_seconds']:.1f}s"
            })

        total_time = sum(r["duration_seconds"] for r in mode_results)
        total_questions = sum(r["num_questions"] for r in mode_results)
        total_calls = sum(r["num_llm_calls"] for r in mode_results)

        print(f"\n  Total: {total_questions} questions, {total_calls} LLM calls, "
              f"{total_time:.1f}s ({total_time/60:.1f} min)")

        return mode_results

    def _run_batch_mode(
        self,
        provider: str,
        batch_size: Optional[int],
        adaptive: bool
    ) -> List[Dict]:
        """
        Run batch mode with specified batch size.

        Args:
            provider: LLM provider name
            batch_size: Batch size (None = all-at-once)
            adaptive: True = use A* search, False = ask all questions upfront

        Returns:
            List of per-vignette results
        """
        mode_results = []

        batch_str = str(batch_size) if batch_size else "all"
        mode_type = "adaptive" if adaptive else "non-adaptive"

        vignettes_subset = self.vignettes[:self.num_vignettes]
        pbar = tqdm(
            vignettes_subset,
            desc=f"  Batch-{batch_str} ({provider})",
            unit="vignette",
            leave=True
        )

        for vignette in pbar:
            if adaptive:
                result = self._process_vignette_batch_adaptive(
                    vignette, provider, batch_size
                )
            else:
                result = self._process_vignette_batch_nonadaptive(
                    vignette, provider
                )

            mode_results.append(result)

            # Update progress bar with latest stats
            pbar.set_postfix({
                'questions': result['num_questions'],
                'batches': result['num_llm_calls'],
                'time': f"{result['duration_seconds']:.1f}s"
            })

        total_time = sum(r["duration_seconds"] for r in mode_results)
        total_questions = sum(r["num_questions"] for r in mode_results)
        total_calls = sum(r["num_llm_calls"] for r in mode_results)

        print(f"\n  Total: {total_questions} questions, {total_calls} LLM calls, "
              f"{total_time:.1f}s ({total_time/60:.1f} min)")

        return mode_results

    def _process_vignette_sequential(
        self,
        vignette: Dict,
        provider: str
    ) -> Dict:
        """
        Process one vignette using sequential questioning (baseline).

        Implements the current benchmark.py logic: 1 question at a time
        with A* search selecting next best question after each answer.

        Args:
            vignette: Vignette dict
            provider: LLM provider name

        Returns:
            Result dict with questions, answers, timing
        """
        start_time = time.time()

        manager = SessionManager()
        search = DiagnosticSearch(manager)
        analyser = ClinicalAnalyser(vignette, provider_name=provider)

        manager.start_new_session()
        self._set_patient_age(manager, vignette)

        questions_asked = []
        answers = {}

        while True:
            # A* search selects next best question
            next_q_id = search.get_next_best_question(
                manager.state.active_candidates
            )
            if not next_q_id:
                break

            # Get question text
            q_text = manager.get_symptom_description(next_q_id)

            # LLM answers (1 call per question)
            answer = analyser.answer(q_text)

            questions_asked.append({
                "id": next_q_id,
                "text": q_text,
                "answer": answer
            })
            answers[q_text] = answer

            # Update state for next iteration
            manager.answer_question(next_q_id, answer)

        duration = time.time() - start_time

        return {
            "vignette_id": vignette["id"],
            "ground_truth": vignette["ground_truth"],
            "num_questions": len(questions_asked),
            "num_llm_calls": len(questions_asked),  # 1 call per question
            "duration_seconds": duration,
            "questions": questions_asked,
            "answers": answers
        }

    def _process_vignette_batch_adaptive(
        self,
        vignette: Dict,
        provider: str,
        batch_size: int
    ) -> Dict:
        """
        Process vignette using adaptive batching with A* search.

        Key difference from sequential: Get next N best questions via A* search,
        answer them in a batch, update state, repeat.

        Args:
            vignette: Vignette dict
            provider: LLM provider name
            batch_size: Number of questions per batch

        Returns:
            Result dict
        """
        start_time = time.time()

        manager = SessionManager()
        search = DiagnosticSearch(manager)
        analyser = BatchClinicalAnalyser(vignette, provider_name=provider)

        manager.start_new_session()
        self._set_patient_age(manager, vignette)

        all_questions = []
        all_answers = {}
        num_batches = 0

        while True:
            # Get next N best questions via A* search
            next_questions = self._get_next_n_questions(
                search, manager, batch_size
            )

            if not next_questions:
                break

            # Get question texts
            question_texts = [
                manager.get_symptom_description(q_id) for q_id in next_questions
            ]

            # Answer batch via LLM
            try:
                batch_answers = analyser.answer_batch(
                    question_texts,
                    batch_size=None  # Process all N questions in one call
                )
                num_batches += 1
            except Exception as e:
                print(f"    [Warning] Batch failed: {e}, falling back to sequential")
                # Fallback to sequential for this batch
                batch_answers = {}
                for q_text in question_texts:
                    batch_answers[q_text] = analyser.answer(q_text)
                    num_batches += 1  # Count each sequential call

            # Record answers
            for q_id, q_text in zip(next_questions, question_texts):
                answer = batch_answers.get(q_text, "UNKNOWN")
                all_questions.append({
                    "id": q_id,
                    "text": q_text,
                    "answer": answer
                })
                all_answers[q_text] = answer

                # Update state after each answer for next A* iteration
                manager.answer_question(q_id, answer)

        duration = time.time() - start_time

        return {
            "vignette_id": vignette["id"],
            "ground_truth": vignette["ground_truth"],
            "num_questions": len(all_questions),
            "num_llm_calls": num_batches,
            "duration_seconds": duration,
            "questions": all_questions,
            "answers": all_answers,
            "batch_size": batch_size
        }

    def _process_vignette_batch_nonadaptive(
        self,
        vignette: Dict,
        provider: str
    ) -> Dict:
        """
        Process vignette using non-adaptive all-at-once batching.

        Generates all possible questions upfront (no A* search adaptation),
        answers them in a single batch call.

        Args:
            vignette: Vignette dict
            provider: LLM provider name

        Returns:
            Result dict
        """
        start_time = time.time()

        # Generate all questions upfront
        question_texts = self._get_all_questions_for_vignette(vignette)

        analyser = BatchClinicalAnalyser(vignette, provider_name=provider)

        # Single batch call
        try:
            batch_answers = analyser.answer_batch(
                question_texts,
                batch_size=None  # All at once
            )
            num_batches = 1
        except Exception as e:
            print(f"    [Warning] Batch failed: {e}, falling back to sequential")
            batch_answers = {}
            for q_text in question_texts:
                batch_answers[q_text] = analyser.answer(q_text)
            num_batches = len(question_texts)

        duration = time.time() - start_time

        # Convert to questions list format
        questions = [
            {"id": f"q_{i}", "text": q, "answer": batch_answers.get(q, "UNKNOWN")}
            for i, q in enumerate(question_texts)
        ]

        return {
            "vignette_id": vignette["id"],
            "ground_truth": vignette["ground_truth"],
            "num_questions": len(questions),
            "num_llm_calls": num_batches,
            "duration_seconds": duration,
            "questions": questions,
            "answers": batch_answers,
            "batch_size": "all"
        }

    def _get_next_n_questions(
        self,
        search: DiagnosticSearch,
        manager: SessionManager,
        n: int
    ) -> List[str]:
        """
        Get next N best questions via A* search.

        Args:
            search: DiagnosticSearch instance
            manager: SessionManager instance
            n: Number of questions to get

        Returns:
            List of question IDs
        """
        questions = []
        candidates = manager.state.active_candidates.copy()

        for _ in range(n):
            next_q = search.get_next_best_question(candidates)
            if not next_q:
                break
            questions.append(next_q)

            # Temporarily mark as "asked" to avoid selecting again
            # (We'll actually ask all N together, but A* needs to know
            # we're planning to ask this one)
            if next_q not in manager.state.answered_questions:
                manager.state.answered_questions[next_q] = "PENDING"

        # Clean up temporary markers
        for q in questions:
            if manager.state.answered_questions.get(q) == "PENDING":
                del manager.state.answered_questions[q]

        return questions

    def _get_all_questions_for_vignette(self, vignette: Dict) -> List[str]:
        """
        Generate ALL question types for non-adaptive batch mode.

        Includes all 6 question types:
        1. Symptoms
        2. Exclusion criteria
        3. Subjective criteria
        4. Duration requirements
        5. Onset age requirements
        6. Onset event requirements

        Args:
            vignette: Vignette dict

        Returns:
            List of question texts
        """
        manager = SessionManager()
        manager.start_new_session()
        self._set_patient_age(manager, vignette)

        questions = set()

        # Get all questions for all active disorders
        for disorder in manager.state.active_candidates:
            # 1. SYMPTOMS (already working)
            symptoms = manager.get_candidate_symptoms(disorder)
            for symptom_id in symptoms:
                try:
                    q_text = manager.get_symptom_description(symptom_id)
                    if q_text:
                        questions.add(q_text)
                except Exception:
                    pass

            # 2. EXCLUSION CRITERIA (NEW)
            try:
                exclusions = list(manager.prolog.query(
                    f"exclusion_criterion({disorder}, ExcID, _, _)"
                ))
                for exc in exclusions:
                    exc_id = str(exc['ExcID'])
                    q_text = manager.get_symptom_description(exc_id)
                    if q_text:
                        questions.add(q_text)
            except Exception:
                pass

            # 3. SUBJECTIVE CRITERIA (NEW)
            try:
                subjective = list(manager.prolog.query(
                    f"subjective_criterion({disorder}, SubjID, _, _)"
                ))
                for subj in subjective:
                    subj_id = str(subj['SubjID'])
                    q_text = manager.get_symptom_description(subj_id)
                    if q_text:
                        questions.add(q_text)
            except Exception:
                pass

            # 4. DURATION REQUIREMENTS (NEW)
            try:
                has_duration = list(manager.prolog.query(
                    f"duration_requirement({disorder}, _, _)"
                ))
                if has_duration:
                    dur_id = f"{disorder}_duration_check"
                    q_text = manager.get_symptom_description(dur_id)
                    if q_text:
                        questions.add(q_text)
            except Exception:
                pass

            # 5. ONSET AGE REQUIREMENTS (NEW)
            try:
                onset_age = list(manager.prolog.query(
                    f"onset_requirement({disorder}, before_age, _)"
                ))
                if onset_age:
                    onset_id = f"{disorder}_onset_age_check"
                    q_text = manager.get_symptom_description(onset_id)
                    if q_text:
                        questions.add(q_text)
            except Exception:
                pass

            # 6. ONSET EVENT REQUIREMENTS (NEW)
            try:
                onset_event = list(manager.prolog.query(
                    f"onset_requirement({disorder}, after_event, _)"
                ))
                if onset_event:
                    onset_id = f"{disorder}_onset_event_check"
                    q_text = manager.get_symptom_description(onset_id)
                    if q_text:
                        questions.add(q_text)
            except Exception:
                pass

        return list(questions)

    def _set_patient_age(self, manager: SessionManager, vignette: Dict):
        """
        Extract and set patient age from vignette demographics.

        Args:
            manager: SessionManager instance
            vignette: Vignette dict with 'demographics' key
        """
        demo = vignette.get('demographics', '')
        age_match = re.search(r'(\d+)-year-old', demo)

        if age_match:
            age = int(age_match.group(1))
            manager.set_patient_data(age)
        else:
            # Default to adult if not found
            manager.set_patient_data(30)
