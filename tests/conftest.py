"""Shared pytest fixtures for diagnostic system tests."""

import pytest
from pathlib import Path

from src.reasoning.engine import PrologEngine
from src.diagnosis.driver import DiagnosticDriver


@pytest.fixture
def prolog_dir():
    """Path to Prolog source directory."""
    return Path(__file__).parent.parent / 'src' / 'prolog'


@pytest.fixture
def engine(prolog_dir):
    """Loaded PrologEngine with schema and gold standards."""
    eng = PrologEngine(prolog_dir)
    assert eng.load_file('schema.pl'), "Failed to load schema.pl"
    assert eng.load_file('gold_standard/loader.pl'), "Failed to load gold standards"
    return eng


@pytest.fixture
def driver():
    """Loaded DiagnosticDriver ready for diagnosis."""
    drv = DiagnosticDriver()
    assert drv.load(), "Failed to load driver"
    return drv


@pytest.fixture
def sample_vignette():
    """Minimal valid vignette dict for testing."""
    return {
        'id': 'test_vig_001',
        'ground_truth': ['mdd'],
        'difficulty': 'CLEAR',
        'meets_criteria': True,
        'clinical_text': 'A 35-year-old presents with persistent sadness lasting 3 weeks.',
        'answers': {
            'mdd_a1': 'present',
            'mdd_a2': 'present',
            'mdd_a3': 'present',
            'mdd_a4': 'present',
            'mdd_a5': 'present',
            'mdd_a6': 'absent',
            'mdd_a7': 'absent',
            'mdd_a8': 'absent',
            'mdd_a9': 'absent',
            'mdd_exc_substance': 'cleared',
            'mdd_exc_medical': 'cleared',
            'mdd_exc_bereavement': 'cleared',
            'mdd_subj_clinical_significance': 'met',
            'duration_days': 21
        }
    }


@pytest.fixture
def clear_patient(engine):
    """Fixture that clears patient facts after test."""
    yield
    engine.query("clear_all_patient_facts")
