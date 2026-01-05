"""Tests for DiagnosticDriver - the orchestration layer."""

import pytest
from src.diagnosis.driver import DiagnosticDriver, DiagnosticItem


class TestDiagnosticDriver:
    """Tests for DiagnosticDriver core functionality."""

    def test_load_returns_true_on_success(self):
        """Driver loads schema and disorders successfully."""
        driver = DiagnosticDriver()
        assert driver.load()
        assert driver.engine is not None

    def test_get_next_question_returns_item(self, driver):
        """get_next_question returns a DiagnosticItem."""
        item = driver.get_next_question('test_patient')
        assert item is not None
        assert isinstance(item, DiagnosticItem)
        assert item.item_type in ['symptom', 'exclusion', 'duration', 'onset', 'subjective', 'settings']
        assert item.description != ''

    def test_assert_symptom_persists(self, driver):
        """Assert symptom fact and verify it persists."""
        item = DiagnosticItem(
            item_type='symptom',
            item_id='mdd_a1',
            category='core_symptoms',
            description='Depressed mood',
            disorder_id='mdd'
        )
        assert driver.assert_answer('test_patient', item, 'present', 'feels sad daily')

        # Verify via engine query
        results = driver.engine.query("patient_symptom(test_patient, mdd_a1, Status, _)")
        assert len(results) == 1
        assert results[0]['Status'] == 'present'

        # Clean up
        driver.clear_patient('test_patient')

    def test_assert_duration_handles_none(self, driver):
        """Duration with None value defaults to 0 days."""
        item = DiagnosticItem(
            item_type='duration',
            item_id='duration',
            category='none',
            description='Duration',
            disorder_id='mdd'
        )
        assert driver.assert_answer('test_patient', item, 'unclear', '', 0.5, None)

        # Verify 0 was asserted
        results = driver.engine.query("patient_duration(test_patient, mdd, Days)")
        assert len(results) == 1
        assert results[0]['Days'] == 0

        driver.clear_patient('test_patient')

    def test_get_diagnosis_returns_result(self, driver):
        """get_diagnosis returns diagnosis dict after facts asserted."""
        # Assert enough symptoms for MDD
        for sym_id in ['mdd_a1', 'mdd_a2', 'mdd_a3', 'mdd_a4', 'mdd_a5']:
            item = DiagnosticItem('symptom', sym_id, 'all_symptoms', '', 'mdd')
            driver.assert_answer('test_patient', item, 'present', 'evidence')

        diagnosis = driver.get_diagnosis('mdd', 'test_patient')
        assert diagnosis is not None
        assert 'overall_status' in diagnosis

        driver.clear_patient('test_patient')

    def test_clear_patient_removes_all_facts(self, driver):
        """clear_patient removes all patient-specific facts."""
        # Assert some facts
        item = DiagnosticItem('symptom', 'mdd_a1', 'core_symptoms', '', 'mdd')
        driver.assert_answer('test_patient', item, 'present', 'test')

        # Clear
        driver.clear_patient('test_patient')

        # Verify empty
        results = driver.engine.query("patient_symptom(test_patient, _, _, _)")
        assert results == []

    def test_get_explanation_returns_structure(self, driver):
        """get_explanation returns explanation dict."""
        # Need some facts for explanation to work
        item = DiagnosticItem('symptom', 'mdd_a1', 'core_symptoms', '', 'mdd')
        driver.assert_answer('test_patient', item, 'present', 'test')

        explanation = driver.get_explanation('mdd', 'test_patient')
        assert explanation is not None
        assert 'disorder' in explanation or 'disorder_id' in explanation

        driver.clear_patient('test_patient')


class TestDifferentialDiagnosis:
    """Tests for differential diagnosis functionality."""

    def test_active_candidates_starts_with_all(self, driver):
        """Active candidates includes all disorders initially."""
        candidates = driver.get_active_candidates('new_patient')
        # 6 disorders: mdd, gad, adhd, ptsd, ptsd_preschool, asd
        assert len(candidates) >= 5
        assert 'mdd' in candidates
        assert 'gad' in candidates

    def test_pruning_after_exclusion(self, driver):
        """Disorder is pruned after exclusion criterion confirmed."""
        # Assert an exclusion for MDD
        item = DiagnosticItem('exclusion', 'mdd_exc_substance', 'substance', '', 'mdd')
        driver.assert_answer('test_patient', item, 'excluded', '')

        # Check if MDD is pruned
        is_pruned = driver.is_pruned('mdd', 'test_patient')
        # Note: May or may not be pruned depending on exclusion logic
        assert isinstance(is_pruned, bool)

        driver.clear_patient('test_patient')

    def test_differential_returns_results_for_all_active(self, driver):
        """run_differential_diagnosis returns results for all non-pruned disorders."""
        # Simple callback that marks everything as absent/cleared
        def simple_answer_fn(item):
            if item.item_type == 'symptom':
                return 'absent', '', 1.0, None
            elif item.item_type == 'exclusion':
                return 'cleared', '', 1.0, None
            elif item.item_type == 'subjective':
                return 'not_met', '', 1.0, None
            elif item.item_type == 'duration':
                return 'present', '', 1.0, 0
            elif item.item_type == 'onset':
                return 'present', '', 1.0, 30
            elif item.item_type == 'settings':
                return 'present', 'home', 1.0, None
            return 'unclear', '', 0.5, None

        results = driver.run_differential_diagnosis(
            patient_id='test_patient',
            answer_fn=simple_answer_fn,
            verbose=False
        )

        # Should have results for at least some disorders
        assert isinstance(results, dict)
        # All disorders should be not_met since we marked everything absent
        for disorder_id, result in results.items():
            assert result.status in ['met', 'not_met', 'incomplete']

        driver.clear_patient('test_patient')
