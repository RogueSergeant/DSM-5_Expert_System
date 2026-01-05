"""Tests for PrologEngine - the Python-Prolog bridge."""

import pytest
from pathlib import Path
from src.reasoning.engine import PrologEngine


class TestPrologEngine:
    """Tests for PrologEngine core functionality."""

    def test_load_schema_succeeds(self, prolog_dir):
        """Schema loads without error."""
        engine = PrologEngine(prolog_dir)
        assert engine.load_file('schema.pl')

    def test_load_gold_standards_succeeds(self, prolog_dir):
        """Gold standard disorders load without error."""
        engine = PrologEngine(prolog_dir)
        engine.load_file('schema.pl')
        assert engine.load_file('gold_standard/loader.pl')

    def test_load_missing_file_returns_false(self, prolog_dir):
        """Loading nonexistent file returns False."""
        engine = PrologEngine(prolog_dir)
        assert not engine.load_file('nonexistent.pl')

    def test_query_returns_list_of_dicts(self, engine):
        """Query returns list of dict results."""
        results = engine.query("disorder(D, Name, Category)")
        assert isinstance(results, list)
        assert len(results) > 0
        assert isinstance(results[0], dict)
        assert 'D' in results[0]

    def test_query_one_returns_first_match(self, engine):
        """query_one returns first matching result."""
        result = engine.query_one("disorder(mdd, Name, Category)")
        assert result is not None
        assert 'Name' in result
        assert 'Category' in result

    def test_query_no_match_returns_empty(self, engine):
        """Query with no matches returns empty list."""
        results = engine.query("disorder(nonexistent, _, _)")
        assert results == []

    def test_query_one_no_match_returns_none(self, engine):
        """query_one with no match returns None."""
        result = engine.query_one("disorder(nonexistent, _, _)")
        assert result is None

    def test_assert_and_query_fact(self, engine, clear_patient):
        """Assert a fact and query it back."""
        # Assert a patient symptom
        assert engine.assert_fact("patient_symptom(test_patient, mdd_a1, present, 'test evidence')")

        # Query it back
        results = engine.query("patient_symptom(test_patient, mdd_a1, Status, Evidence)")
        assert len(results) == 1
        assert results[0]['Status'] == 'present'

    def test_retract_removes_facts(self, engine, clear_patient):
        """Retract removes asserted facts."""
        # Assert a fact
        engine.assert_fact("patient_symptom(test_patient, mdd_a1, present, 'test')")

        # Clear patient facts
        engine.query("clear_patient_facts(test_patient)")

        # Verify it's gone
        results = engine.query("patient_symptom(test_patient, mdd_a1, _, _)")
        assert results == []


class TestPrologDisorderQueries:
    """Tests for querying disorder definitions."""

    def test_all_five_disorders_loaded(self, engine):
        """All five gold standard disorders are loaded."""
        results = engine.query("disorder(D, _, _)")
        disorder_ids = [r['D'] for r in results]
        assert 'mdd' in disorder_ids
        assert 'gad' in disorder_ids
        assert 'adhd' in disorder_ids
        assert 'ptsd' in disorder_ids
        assert 'asd' in disorder_ids

    def test_disorder_has_symptoms(self, engine):
        """Each disorder has defined symptoms."""
        for disorder_id in ['mdd', 'gad', 'adhd', 'ptsd', 'asd']:
            results = engine.query(f"symptom({disorder_id}, SID, _, _)")
            assert len(results) > 0, f"{disorder_id} has no symptoms"

    def test_mdd_has_nine_symptoms(self, engine):
        """MDD has 9 symptoms (A1-A9)."""
        results = engine.query("symptom(mdd, SID, _, _)")
        assert len(results) == 9
