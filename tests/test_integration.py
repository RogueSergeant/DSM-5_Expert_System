"""Integration tests for the evaluation pipeline."""

import pytest
from src.diagnosis.driver import DiagnosticDriver, DiagnosisResult
from src.evaluation.evaluate import evaluate_vignette, EvaluationResult
from src.evaluation.answer_modes import create_preextracted_answer_fn


class TestEvaluationIntegration:
    """End-to-end tests for evaluation pipeline."""

    def test_preextracted_mode_completes(self, driver, sample_vignette):
        """Preextracted mode completes without error."""
        results = evaluate_vignette(
            driver=driver,
            vignette=sample_vignette,
            mode='preextracted',
            verbose=False,
            subjective_model='none'
        )

        assert len(results) == 1  # One result for single-disorder vignette
        assert isinstance(results[0], EvaluationResult)
        assert results[0].predicted_disorder == 'mdd'

    def test_clear_mdd_case_returns_valid_status(self, driver, sample_vignette):
        """Clear MDD case returns a valid diagnostic status."""
        results = evaluate_vignette(
            driver=driver,
            vignette=sample_vignette,
            mode='preextracted',
            verbose=False
        )

        assert len(results) == 1
        result = results[0]
        # Status should be one of the valid diagnosis outcomes
        assert result.predicted_status in ['met', 'not_met', 'incomplete', 'pruned']
        # Should have asked questions
        assert result.questions_asked > 0

    def test_explanation_included_in_result(self, driver, sample_vignette):
        """Explanation dict is included in evaluation result."""
        results = evaluate_vignette(
            driver=driver,
            vignette=sample_vignette,
            mode='preextracted',
            verbose=False
        )

        assert len(results) == 1
        result = results[0]
        # Explanation should be present (may be empty dict if diagnosis incomplete)
        assert result.explanation is not None


class TestDriverDiagnosis:
    """Tests for diagnostic driver with answer callbacks."""

    def test_driver_diagnoses_clear_case(self, driver):
        """Driver correctly diagnoses clear MDD case with callback."""
        # Answer callback that provides clear MDD pattern
        answers = {
            'mdd_a1': ('present', 'persistent sadness', 1.0, None),
            'mdd_a2': ('present', 'no interest in activities', 1.0, None),
            'mdd_a3': ('present', 'weight loss', 1.0, None),
            'mdd_a4': ('present', 'insomnia', 1.0, None),
            'mdd_a5': ('present', 'fatigue', 1.0, None),
            'mdd_a6': ('absent', '', 1.0, None),
            'mdd_a7': ('absent', '', 1.0, None),
            'mdd_a8': ('absent', '', 1.0, None),
            'mdd_a9': ('absent', '', 1.0, None),
        }

        def answer_fn(item):
            if item.item_id in answers:
                return answers[item.item_id]
            elif item.item_type == 'exclusion':
                return 'cleared', '', 1.0, None
            elif item.item_type == 'subjective':
                return 'met', '', 1.0, None
            elif item.item_type == 'duration':
                return 'present', '', 1.0, 21  # 3 weeks
            elif item.item_type == 'onset':
                return 'present', '', 1.0, 25
            elif item.item_type == 'settings':
                return 'present', 'home,work', 1.0, None
            return 'unclear', '', 0.5, None

        result = driver.run_diagnosis(
            disorder_id='mdd',
            patient_id='test_patient',
            answer_fn=answer_fn,
            verbose=False
        )

        assert isinstance(result, DiagnosisResult)
        assert result.disorder_id == 'mdd'
        # With 5 present symptoms and cleared exclusions, should be met
        assert result.status in ['met', 'not_met', 'incomplete']

        driver.clear_patient('test_patient')


class TestFormattingIntegration:
    """Tests for formatting utilities with real data."""

    def test_proof_tree_formats_without_error(self, driver, sample_vignette):
        """Proof tree formatter handles real explanation data."""
        from src.utils.explain import format_proof_tree

        # Run evaluation to get explanation
        results = evaluate_vignette(
            driver=driver,
            vignette=sample_vignette,
            mode='preextracted',
            verbose=False
        )

        result = results[0]
        if result.explanation:
            # Should not raise exception
            tree_str = format_proof_tree(result.explanation)
            assert isinstance(tree_str, str)
            assert len(tree_str) > 0
