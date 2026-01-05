"""
Diagnostic Driver - orchestrates Prolog reasoning with Python glue.

Supports three answer modes:
1. Interactive: User answers via prompts
2. LLM-led: Callback uses LLM to interpret clinical text
3. Vignette data: Callback looks up pre-extracted answers
"""

import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Optional
from src.reasoning.engine import PrologEngine

logger = logging.getLogger(__name__)


@dataclass
class DiagnosticItem:
    """Item to evaluate: symptom, exclusion, subjective, duration, onset, or settings."""
    item_type: str
    item_id: str
    category: str
    description: str
    disorder_id: str

    @property
    def priority(self) -> int:
        return {'symptom': 1, 'exclusion': 2, 'duration': 3,
                'onset': 4, 'subjective': 5, 'settings': 6}.get(self.item_type, 99)


@dataclass
class DiagnosisResult:
    """Final diagnosis outcome."""
    disorder_id: str
    disorder_name: str
    status: str
    confidence: float
    questions_asked: int
    missing_items: int


class DiagnosticDriver:
    """Orchestrates Prolog-based diagnostic reasoning."""

    def __init__(self, prolog_dir: Optional[Path] = None):
        self.prolog_dir = prolog_dir or Path(__file__).parent.parent / 'prolog'
        self.engine: Optional[PrologEngine] = None

    def load(self) -> bool:
        """Load Prolog schema and disorders."""
        self.engine = PrologEngine(self.prolog_dir)
        return (self.engine.load_file('schema.pl') and
                self.engine.load_file('gold_standard/loader.pl'))

    def get_missing_items(self, disorder_id: str, patient_id: str) -> list[DiagnosticItem]:
        """Get unevaluated items ordered by clinical priority."""
        results = self.engine.query(f"get_missing_items({patient_id}, {disorder_id}, Items)")
        if not results:
            return []

        items = []
        for raw in results[0].get('Items', []):
            item_type = str(raw.get('type', ''))
            item_id = str(raw.get('id', ''))
            category = str(raw.get('category', 'none'))

            desc_result = self.engine.query_one(
                f"get_question_text({disorder_id}, {item_type}, {item_id}, Desc)"
            )
            description = str(desc_result.get('Desc', '')) if desc_result else ''

            items.append(DiagnosticItem(item_type, item_id, category, description, disorder_id))

        return sorted(items, key=lambda x: (x.priority, x.item_id))

    def assert_answer(self, patient_id: str, item: DiagnosticItem,
                      status: str, evidence: str = '', confidence: float = 1.0,
                      value: Optional[int] = None) -> bool:
        """Assert patient fact based on answer."""
        esc_evidence = evidence.replace("'", "\\'")

        if item.item_type == 'symptom':
            return self.engine.assert_fact(
                f"patient_symptom({patient_id}, {item.item_id}, {status}, '{esc_evidence}')"
            )
        elif item.item_type == 'exclusion':
            return self.engine.assert_fact(
                f"patient_exclusion_status({patient_id}, {item.item_id}, {status})"
            )
        elif item.item_type == 'subjective':
            return self.engine.assert_fact(
                f"subjective_assessment({patient_id}, {item.item_id}, {status}, {confidence})"
            )
        elif item.item_type == 'duration' and value is not None:
            return self.engine.assert_fact(
                f"patient_duration({patient_id}, {item.disorder_id}, {value})"
            )
        elif item.item_type == 'onset':
            # Check if age-based or event-based onset
            onset_req = self.engine.query_one(f"onset_requirement({item.disorder_id}, Type, Val)")
            if onset_req:
                onset_type = str(onset_req.get('Type', ''))
                if onset_type == 'before_age' and value is not None:
                    return self.engine.assert_fact(f"patient_onset_age({patient_id}, {value})")
                elif onset_type == 'after_event':
                    event_type = str(onset_req.get('Val', ''))
                    return self.engine.assert_fact(
                        f"patient_context({patient_id}, {event_type}, {status})"
                    )
            return False
        elif item.item_type == 'settings' and evidence:
            # Assert each setting as separate fact (e.g., "home,school" -> two facts)
            settings = [s.strip() for s in evidence.split(',')]
            for setting in settings:
                self.engine.assert_fact(f"patient_context({patient_id}, setting, {setting})")
            return True
        return False

    def get_diagnosis(self, disorder_id: str, patient_id: str) -> Optional[dict]:
        """Get current diagnosis status."""
        result = self.engine.query_one(f"full_diagnosis({patient_id}, {disorder_id}, Result)")
        return result.get('Result') if result else None

    def clear_patient(self, patient_id: str) -> bool:
        """Clear all patient facts."""
        return bool(self.engine.query(f"clear_patient_facts({patient_id})"))

    def is_pruned(self, disorder_id: str, patient_id: str) -> bool:
        """Check if disorder is ruled out."""
        return len(self.engine.query(f"disorder_pruned({patient_id}, {disorder_id})")) > 0

    def get_active_candidates(self, patient_id: str) -> list[str]:
        """Get all disorders not yet pruned."""
        results = self.engine.query(f"active_candidates({patient_id}, Candidates)")
        if not results:
            return []
        candidates = results[0].get('Candidates', [])
        return [str(c) for c in candidates] if candidates else []

    def get_all_missing_items(self, patient_id: str) -> list[DiagnosticItem]:
        """Get unevaluated items across all active candidates (deduplicated)."""
        candidates = self.get_active_candidates(patient_id)

        all_items = []
        seen_ids = set()

        for disorder_id in candidates:
            items = self.get_missing_items(disorder_id, patient_id)
            for item in items:
                if item.item_id not in seen_ids:
                    all_items.append(item)
                    seen_ids.add(item.item_id)

        return sorted(all_items, key=lambda x: (x.priority, x.item_id))

    def run_diagnosis(
        self,
        disorder_id: str,
        patient_id: str = 'patient',
        answer_fn: Optional[Callable[[DiagnosticItem], tuple[str, str, float, Optional[int]]]] = None,
        verbose: bool = False
    ) -> DiagnosisResult:
        """
        Run diagnostic process.

        Args:
            disorder_id: Disorder to diagnose
            patient_id: Patient identifier
            answer_fn: Callback returning (status, evidence, confidence, value).
                      If None, uses interactive prompts.
            verbose: Print progress
        """
        self.clear_patient(patient_id)
        questions_asked = 0

        while True:
            if self.is_pruned(disorder_id, patient_id):
                if verbose:
                    print(f"{disorder_id} ruled out")
                break

            diagnosis = self.get_diagnosis(disorder_id, patient_id)
            if diagnosis and diagnosis.get('overall_status') in ['met', 'not_met']:
                if verbose:
                    print(f"Diagnosis: {diagnosis.get('overall_status')}")
                break

            missing = self.get_missing_items(disorder_id, patient_id)
            if not missing:
                break

            item = missing[0]
            if verbose:
                print(f"[{questions_asked + 1}] {item.item_type}: {item.description[:60]}...")

            if answer_fn:
                status, evidence, confidence, value = answer_fn(item)
            else:
                status, evidence, confidence, value = self._prompt(item)

            self.assert_answer(patient_id, item, status, evidence, confidence, value)
            questions_asked += 1

        diagnosis = self.get_diagnosis(disorder_id, patient_id)
        missing = self.get_missing_items(disorder_id, patient_id)
        disorder_info = self.engine.query_one(f"disorder({disorder_id}, Name, _)")

        return DiagnosisResult(
            disorder_id=disorder_id,
            disorder_name=str(disorder_info.get('Name', disorder_id)) if disorder_info else disorder_id,
            status=diagnosis.get('overall_status', 'incomplete') if diagnosis else 'incomplete',
            confidence=float(diagnosis.get('confidence', 0.0)) if diagnosis else 0.0,
            questions_asked=questions_asked,
            missing_items=len(missing)
        )

    def run_differential_diagnosis(
        self,
        patient_id: str = 'patient',
        answer_fn: Optional[Callable[[DiagnosticItem], tuple[str, str, float, Optional[int]]]] = None,
        verbose: bool = False
    ) -> dict[str, DiagnosisResult]:
        """
        Run differential diagnosis across all disorders.

        Returns: dict mapping disorder_id -> DiagnosisResult for all non-pruned disorders
        """
        self.clear_patient(patient_id)
        questions_asked = 0
        prev_candidates = set()

        while True:
            candidates = self.get_active_candidates(patient_id)
            candidates_set = set(candidates)

            # Log when disorders are pruned
            pruned = prev_candidates - candidates_set
            if pruned:
                logger.info(f"  PRUNED | {'+'.join(sorted(pruned))} | remaining={len(candidates)}")

            prev_candidates = candidates_set

            if not candidates:
                logger.debug(f"  DIFFERENTIAL | all candidates pruned")
                break

            # Check if all remaining candidates have final diagnosis
            all_resolved = True
            for disorder_id in candidates:
                diagnosis = self.get_diagnosis(disorder_id, patient_id)
                if not diagnosis or diagnosis.get('overall_status') not in ['met', 'not_met']:
                    all_resolved = False
                    break

            if all_resolved:
                logger.debug(f"  DIFFERENTIAL | all candidates resolved")
                break

            missing = self.get_all_missing_items(patient_id)
            if not missing:
                logger.debug(f"  DIFFERENTIAL | no more missing items")
                break

            item = missing[0]
            if verbose:
                print(f"[{questions_asked + 1}] {item.disorder_id}.{item.item_type}: {item.description[:50]}...")

            if answer_fn:
                status, evidence, confidence, value = answer_fn(item)
            else:
                status, evidence, confidence, value = self._prompt(item)

            self.assert_answer(patient_id, item, status, evidence, confidence, value)
            questions_asked += 1

        # Build results for all non-pruned disorders
        results = {}
        all_disorders = self.engine.query("disorder(D, _, _)")
        for d in all_disorders:
            disorder_id = str(d.get('D', ''))
            if not self.is_pruned(disorder_id, patient_id):
                diagnosis = self.get_diagnosis(disorder_id, patient_id)
                disorder_info = self.engine.query_one(f"disorder({disorder_id}, Name, _)")
                results[disorder_id] = DiagnosisResult(
                    disorder_id=disorder_id,
                    disorder_name=str(disorder_info.get('Name', disorder_id)) if disorder_info else disorder_id,
                    status=diagnosis.get('overall_status', 'incomplete') if diagnosis else 'incomplete',
                    confidence=float(diagnosis.get('confidence', 0.0)) if diagnosis else 0.0,
                    questions_asked=questions_asked,
                    missing_items=len(self.get_missing_items(disorder_id, patient_id))
                )

        logger.info(f"  DIFFERENTIAL | complete | questions={questions_asked} | results={list(results.keys())}")
        return results

    def _prompt(self, item: DiagnosticItem) -> tuple[str, str, float, Optional[int]]:
        """Interactive prompt for answer."""
        print(f"\n{item.item_type.upper()}: {item.description}")

        if item.item_type == 'symptom':
            r = input("[p]resent/[a]bsent/[u]nclear: ").strip().lower()
            if r.startswith('p'):
                return 'present', input("Evidence: ").strip(), 1.0, None
            return 'absent' if r.startswith('a') else 'unclear', '', 1.0, None

        elif item.item_type == 'exclusion':
            r = input("[c]leared/[e]xcluded/[u]nknown: ").strip().lower()
            return ('cleared' if r.startswith('c') else
                    'excluded' if r.startswith('e') else 'unknown'), '', 1.0, None

        elif item.item_type == 'subjective':
            r = input("[m]et/[n]ot_met/[u]nclear: ").strip().lower()
            return ('met' if r.startswith('m') else
                    'not_met' if r.startswith('n') else 'unclear'), '', 1.0, None

        elif item.item_type == 'duration':
            try:
                return 'present', '', 1.0, int(input("Days: ").strip())
            except ValueError:
                return 'unclear', '', 0.5, None

        elif item.item_type == 'onset':
            try:
                return 'present', '', 1.0, int(input("Onset age: ").strip())
            except ValueError:
                return 'unclear', '', 0.5, None

        elif item.item_type == 'settings':
            s = input("Setting: ").strip()
            return ('present', s, 1.0, None) if s else ('unclear', '', 0.5, None)

        return 'unclear', '', 0.5, None


def run_interactive(disorder_id: str = 'mdd') -> DiagnosisResult:
    """Run interactive diagnostic session."""
    driver = DiagnosticDriver()
    print("Loading...")
    if not driver.load():
        print("Failed to load")
        return None

    print(f"\nDiagnosis: {disorder_id}\n")
    result = driver.run_diagnosis(disorder_id, verbose=True)

    print(f"\nResult: {result.disorder_name}")
    print(f"  Status: {result.status}")
    print(f"  Confidence: {result.confidence:.1%}")
    print(f"  Questions: {result.questions_asked}")
    print(f"  Remaining: {result.missing_items}")
    return result


if __name__ == '__main__':
    import sys
    run_interactive(sys.argv[1] if len(sys.argv) > 1 else 'mdd')
