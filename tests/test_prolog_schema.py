"""
Tests for the Prolog diagnostic schema.

Run with: pytest tests/test_prolog_schema.py -v
"""

import subprocess
from pathlib import Path

import pytest


# Project root
PROJECT_ROOT = Path(__file__).parent.parent


class TestSchemaLoading:
    """Test that schema and gold standards load correctly."""

    def test_schema_loads(self):
        """Schema.pl should load without errors."""
        result = subprocess.run(
            ["swipl", "-g", "[src/prolog/schema], halt(0)", "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        assert result.returncode == 0, f"Schema failed to load: {result.stderr}"

    def test_gold_standards_load(self):
        """All gold standard disorder files should load."""
        result = subprocess.run(
            ["swipl", "-g",
             "[src/prolog/schema], ['src/prolog/gold_standard/loader'], halt(0)",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        assert result.returncode == 0, f"Gold standards failed to load: {result.stderr}"
        # Check all disorders are loaded
        assert "Loaded: mdd" in result.stdout
        assert "Loaded: gad" in result.stdout
        assert "Loaded: adhd" in result.stdout
        assert "Loaded: ptsd" in result.stdout
        assert "Loaded: asd" in result.stdout


class TestDisorderDefinitions:
    """Test that all disorders are properly defined."""

    @pytest.fixture(autouse=True)
    def setup(self):
        """Set up query helper."""
        self.disorders = ["mdd", "gad", "adhd", "ptsd", "asd"]

    def _run_query(self, query: str) -> tuple[bool, str]:
        """Run a Prolog query and return (success, output)."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], ({query} -> halt(0) ; halt(1))",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0, result.stdout + result.stderr

    def test_all_disorders_exist(self):
        """All five target disorders should be defined."""
        for disorder in self.disorders:
            success, output = self._run_query(f"disorder({disorder}, _, _)")
            assert success, f"Disorder {disorder} not found"

    def test_all_disorders_have_symptoms(self):
        """Each disorder should have at least one symptom."""
        for disorder in self.disorders:
            success, output = self._run_query(f"symptom({disorder}, _, _, _)")
            assert success, f"No symptoms found for {disorder}"

    def test_all_disorders_have_categories(self):
        """Each disorder should have at least one symptom category."""
        for disorder in self.disorders:
            success, output = self._run_query(f"symptom_category({disorder}, _, _, _, _)")
            assert success, f"No symptom categories found for {disorder}"

    def test_all_disorders_have_subjective_criteria(self):
        """Each disorder should have subjective criteria."""
        for disorder in self.disorders:
            success, output = self._run_query(f"subjective_criterion({disorder}, _, _, _)")
            assert success, f"No subjective criteria found for {disorder}"


class TestSymptomLogic:
    """Test data-driven symptom logic (OR vs AND)."""

    def _run_query(self, query: str) -> bool:
        """Run a Prolog query and return success."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], ({query} -> halt(0) ; halt(1))",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0

    def test_adhd_uses_or_logic(self):
        """ADHD should use OR logic (either category can satisfy criteria)."""
        assert self._run_query("symptom_logic(adhd, or_any)")

    def test_mdd_uses_default_and_logic(self):
        """MDD should not have explicit symptom_logic (defaults to AND)."""
        assert not self._run_query("symptom_logic(mdd, _)")

    def test_gad_uses_default_and_logic(self):
        """GAD should not have explicit symptom_logic (defaults to AND)."""
        assert not self._run_query("symptom_logic(gad, _)")

    def test_ptsd_uses_default_and_logic(self):
        """PTSD should not have explicit symptom_logic (defaults to AND)."""
        assert not self._run_query("symptom_logic(ptsd, _)")


class TestAgeAdjustments:
    """Test age-adjusted symptom counts."""

    def _run_query(self, query: str) -> bool:
        """Run a Prolog query and return success."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], ({query} -> halt(0) ; halt(1))",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0

    def test_adhd_inattention_age_adjustment(self):
        """ADHD inattention should require 5 symptoms for adults (17+)."""
        assert self._run_query("age_adjusted_count(adhd, inattention_symptoms, 17, 5)")

    def test_adhd_hyperactivity_age_adjustment(self):
        """ADHD hyperactivity should require 5 symptoms for adults (17+)."""
        assert self._run_query("age_adjusted_count(adhd, hyperactivity_impulsivity_symptoms, 17, 5)")


class TestSettingRequirements:
    """Test setting/context requirements."""

    def _run_query(self, query: str) -> bool:
        """Run a Prolog query and return success."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], ({query} -> halt(0) ; halt(1))",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0

    def test_adhd_requires_two_settings(self):
        """ADHD requires symptoms in 2+ settings."""
        assert self._run_query("setting_requirement(adhd, 2)")

    def test_asd_requires_multiple_contexts(self):
        """ASD requires symptoms across multiple contexts."""
        assert self._run_query("setting_requirement(asd, 2)")


class TestDisorderAgeRanges:
    """Test disorder age range definitions."""

    def _run_query(self, query: str) -> bool:
        """Run a Prolog query and return success."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], ({query} -> halt(0) ; halt(1))",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0

    def test_ptsd_age_range(self):
        """PTSD should apply to ages 7-999."""
        assert self._run_query("disorder_age_range(ptsd, 7, 999)")

    def test_ptsd_preschool_age_range(self):
        """PTSD preschool subtype should apply to ages 0-6."""
        assert self._run_query("disorder_age_range(ptsd_preschool, 0, 6)")


class TestSchemaValidation:
    """Test that all disorders pass validation."""

    def _run_query(self, query: str) -> tuple[bool, str]:
        """Run a Prolog query and return (success, output)."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], {query}",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0, result.stdout + result.stderr

    @pytest.mark.parametrize("disorder", ["mdd", "gad", "adhd", "ptsd", "asd"])
    def test_disorder_validates(self, disorder):
        """Each disorder should pass validation with no issues."""
        success, output = self._run_query(
            f"(validate_disorder({disorder}, Issues), Issues = [] -> halt(0) ; halt(1))"
        )
        assert success, f"Validation failed for {disorder}: {output}"


class TestCriterionCheck:
    """Test criterion_check/5 predicate."""

    def _run_query(self, query: str) -> bool:
        """Run a Prolog query and return success."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], ({query} -> halt(0) ; halt(1))",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0

    def test_symptoms_missing_data_without_patient_facts(self):
        """Without patient facts, symptoms should return missing_data."""
        assert self._run_query(
            "criterion_check(test_patient, mdd, symptoms, missing_data, _)"
        )

    def test_duration_missing_data_without_patient_facts(self):
        """Without patient facts, duration should return missing_data."""
        assert self._run_query(
            "criterion_check(test_patient, mdd, duration, missing_data, _)"
        )

    def test_no_duration_requirement_returns_not_applicable(self):
        """ASD has no duration requirement - should return not_applicable."""
        assert self._run_query(
            "criterion_check(test_patient, asd, duration, not_applicable, _)"
        )


class TestMissingItems:
    """Test get_missing_items/3 predicate."""

    def _run_query(self, query: str) -> bool:
        """Run a Prolog query and return success."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], ({query} -> halt(0) ; halt(1))",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0

    def test_returns_symptom_items(self):
        """Should return symptom items for unevaluated symptoms."""
        assert self._run_query(
            "get_missing_items(test_patient, mdd, Items), "
            "member(I, Items), get_dict(type, I, symptom)"
        )

    def test_returns_duration_item(self):
        """Should return duration item when not assessed."""
        assert self._run_query(
            "get_missing_items(test_patient, mdd, Items), "
            "member(I, Items), get_dict(type, I, duration)"
        )

    def test_returns_exclusion_items(self):
        """Should return exclusion items when not assessed."""
        assert self._run_query(
            "get_missing_items(test_patient, mdd, Items), "
            "member(I, Items), get_dict(type, I, exclusion)"
        )

    def test_returns_subjective_items(self):
        """Should return subjective items when not assessed."""
        assert self._run_query(
            "get_missing_items(test_patient, mdd, Items), "
            "member(I, Items), get_dict(type, I, subjective)"
        )


class TestFullDiagnosis:
    """Test full_diagnosis/3 predicate."""

    def _run_query(self, query: str) -> bool:
        """Run a Prolog query and return success."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], ({query} -> halt(0) ; halt(1))",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0

    def test_returns_result_dict(self):
        """full_diagnosis should return a dict with disorder_id."""
        assert self._run_query(
            "full_diagnosis(test_patient, mdd, Result), "
            "is_dict(Result), get_dict(disorder_id, Result, mdd)"
        )

    def test_incomplete_without_data(self):
        """Without patient data, status should be incomplete."""
        assert self._run_query(
            "full_diagnosis(test_patient, mdd, Result), "
            "get_dict(overall_status, Result, incomplete)"
        )


class TestUtilities:
    """Test utility predicates."""

    def _run_query(self, query: str) -> bool:
        """Run a Prolog query and return success."""
        result = subprocess.run(
            ["swipl", "-g",
             f"[src/prolog/schema], ['src/prolog/gold_standard/loader'], ({query} -> halt(0) ; halt(1))",
             "-t", "halt(1)"],
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True
        )
        return result.returncode == 0

    def test_normalise_weeks_to_days(self):
        """2 weeks should normalise to 14 days."""
        assert self._run_query("normalise_to_days(2, weeks, 14)")

    def test_normalise_months_to_days(self):
        """6 months should normalise to 180 days."""
        assert self._run_query("normalise_to_days(6, months, 180)")

    def test_all_disorders_returns_list(self):
        """all_disorders/1 should return a list including mdd."""
        assert self._run_query(
            "all_disorders(D), is_list(D), member(mdd, D)"
        )

    def test_symptoms_for_disorder_returns_list(self):
        """symptoms_for_disorder/2 should return a non-empty list."""
        assert self._run_query(
            "symptoms_for_disorder(mdd, S), is_list(S), length(S, L), L > 0"
        )
