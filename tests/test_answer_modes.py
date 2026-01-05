"""
Tests for answer mode implementations.

Run with: pytest tests/test_answer_modes.py -v
"""

from unittest.mock import MagicMock, patch
import pytest

from src.diagnosis.driver import DiagnosticItem
from src.evaluation.answer_modes import (
    create_preextracted_answer_fn,
    create_hybrid_answer_fn,
    _display_llm_suggestion,
    _validate_status,
    _parse_json_response,
)


class TestValidateStatus:
    """Test status validation for different item types."""

    def test_valid_symptom_status(self):
        """Valid symptom statuses should pass through."""
        assert _validate_status('present', 'symptom') == 'present'
        assert _validate_status('absent', 'symptom') == 'absent'
        assert _validate_status('unclear', 'symptom') == 'unclear'

    def test_invalid_symptom_status_defaults_to_unclear(self):
        """Invalid symptom status should default to unclear."""
        assert _validate_status('met', 'symptom') == 'unclear'
        assert _validate_status('invalid', 'symptom') == 'unclear'

    def test_valid_subjective_status(self):
        """Valid subjective statuses should pass through."""
        assert _validate_status('met', 'subjective') == 'met'
        assert _validate_status('not_met', 'subjective') == 'not_met'
        assert _validate_status('unclear', 'subjective') == 'unclear'

    def test_invalid_subjective_status_defaults_to_unclear(self):
        """Invalid subjective status should default to unclear."""
        assert _validate_status('present', 'subjective') == 'unclear'
        assert _validate_status('invalid', 'subjective') == 'unclear'

    def test_valid_exclusion_status(self):
        """Valid exclusion statuses should pass through."""
        assert _validate_status('cleared', 'exclusion') == 'cleared'
        assert _validate_status('excluded', 'exclusion') == 'excluded'
        assert _validate_status('unknown', 'exclusion') == 'unknown'


class TestParseJsonResponse:
    """Test JSON response parsing from LLM."""

    def test_parses_valid_json(self):
        """Valid JSON should parse correctly."""
        content = '{"status": "met", "confidence": 0.85, "evidence": "quote here"}'
        status, evidence, confidence, value = _parse_json_response(content, 'subjective')
        assert status == 'met'
        assert confidence == 0.85
        assert evidence == 'quote here'
        assert value is None

    def test_parses_markdown_wrapped_json(self):
        """JSON wrapped in markdown code blocks should parse."""
        content = '```json\n{"status": "present", "confidence": 0.9}\n```'
        status, evidence, confidence, value = _parse_json_response(content, 'symptom')
        assert status == 'present'
        assert confidence == 0.9

    def test_invalid_json_returns_unclear(self):
        """Invalid JSON should return unclear status."""
        content = 'not valid json at all'
        status, evidence, confidence, value = _parse_json_response(content, 'subjective')
        assert status == 'unclear'
        assert confidence == 0.5

    def test_clamps_confidence_to_valid_range(self):
        """Confidence should be clamped to 0.0-1.0."""
        content = '{"status": "met", "confidence": 1.5}'
        _, _, confidence, _ = _parse_json_response(content, 'subjective')
        assert confidence == 1.0

        content = '{"status": "met", "confidence": -0.5}'
        _, _, confidence, _ = _parse_json_response(content, 'subjective')
        assert confidence == 0.0

    def test_extracts_value_for_duration(self):
        """Duration should extract numeric value."""
        content = '{"status": "present", "confidence": 1.0, "value": 14}'
        _, _, _, value = _parse_json_response(content, 'duration')
        assert value == 14

    def test_ignores_value_for_non_duration_types(self):
        """Non-duration types should ignore value."""
        content = '{"status": "met", "confidence": 1.0, "value": 14}'
        _, _, _, value = _parse_json_response(content, 'subjective')
        assert value is None


class TestPreextractedAnswerFn:
    """Test pre-extracted answer function."""

    def test_looks_up_symptom(self):
        """Should look up symptom status from answers dict."""
        answers = {'mdd_a1': 'present', 'mdd_a2': 'absent'}
        answer_fn = create_preextracted_answer_fn(answers)

        item = DiagnosticItem('symptom', 'mdd_a1', 'core', 'Depressed mood', 'mdd')
        status, evidence, confidence, value = answer_fn(item)
        assert status == 'present'
        assert confidence == 1.0

    def test_returns_unclear_for_missing_symptom(self):
        """Missing symptom should return unclear."""
        answers = {}
        answer_fn = create_preextracted_answer_fn(answers)

        item = DiagnosticItem('symptom', 'mdd_a1', 'core', 'Depressed mood', 'mdd')
        status, _, _, _ = answer_fn(item)
        assert status == 'unclear'

    def test_looks_up_subjective(self):
        """Should look up subjective status from answers dict."""
        answers = {'mdd_subj_clinical_significance': 'met'}
        answer_fn = create_preextracted_answer_fn(answers)

        item = DiagnosticItem('subjective', 'mdd_subj_clinical_significance',
                              'clinical_significance', 'Clinically significant', 'mdd')
        status, _, _, _ = answer_fn(item)
        assert status == 'met'

    def test_handles_duration(self):
        """Should handle duration with numeric value."""
        answers = {'duration_days': 21}
        answer_fn = create_preextracted_answer_fn(answers)

        item = DiagnosticItem('duration', 'mdd', 'none', 'Duration', 'mdd')
        status, _, confidence, value = answer_fn(item)
        assert status == 'present'
        assert value == 21
        assert confidence == 1.0


class TestHybridAnswerFn:
    """Test hybrid answer function that routes subjective to LLM."""

    def test_routes_subjective_to_llm(self):
        """Subjective items should be routed to LLM function."""
        base_fn = MagicMock(return_value=('present', '', 1.0, None))
        llm_fn = MagicMock(return_value=('met', 'evidence here', 0.85, None))

        hybrid_fn = create_hybrid_answer_fn(base_fn, llm_fn, interactive_override=False)

        item = DiagnosticItem('subjective', 'mdd_subj_01', 'clinical_significance',
                              'Clinically significant', 'mdd')
        status, evidence, confidence, value = hybrid_fn(item)

        assert status == 'met'
        assert confidence == 0.85
        assert evidence == 'evidence here'
        llm_fn.assert_called_once_with(item)
        base_fn.assert_not_called()

    def test_routes_symptom_to_base(self):
        """Symptom items should be routed to base function."""
        base_fn = MagicMock(return_value=('present', 'evidence', 1.0, None))
        llm_fn = MagicMock(return_value=('met', '', 0.85, None))

        hybrid_fn = create_hybrid_answer_fn(base_fn, llm_fn, interactive_override=False)

        item = DiagnosticItem('symptom', 'mdd_a1', 'core', 'Depressed mood', 'mdd')
        status, _, _, _ = hybrid_fn(item)

        assert status == 'present'
        base_fn.assert_called_once_with(item)
        llm_fn.assert_not_called()

    def test_routes_exclusion_to_base(self):
        """Exclusion items should be routed to base function."""
        base_fn = MagicMock(return_value=('cleared', '', 1.0, None))
        llm_fn = MagicMock()

        hybrid_fn = create_hybrid_answer_fn(base_fn, llm_fn, interactive_override=False)

        item = DiagnosticItem('exclusion', 'mdd_exc_01', 'substance',
                              'Not substance-induced', 'mdd')
        status, _, _, _ = hybrid_fn(item)

        assert status == 'cleared'
        base_fn.assert_called_once_with(item)
        llm_fn.assert_not_called()

    def test_routes_duration_to_base(self):
        """Duration items should be routed to base function."""
        base_fn = MagicMock(return_value=('present', '', 1.0, 14))
        llm_fn = MagicMock()

        hybrid_fn = create_hybrid_answer_fn(base_fn, llm_fn, interactive_override=False)

        item = DiagnosticItem('duration', 'mdd', 'none', 'Duration', 'mdd')
        status, _, _, value = hybrid_fn(item)

        assert status == 'present'
        assert value == 14
        base_fn.assert_called_once_with(item)
        llm_fn.assert_not_called()


class TestHybridInteractiveOverride:
    """Test hybrid function with interactive override enabled."""

    @patch('builtins.input', return_value='a')
    @patch('builtins.print')
    def test_accept_returns_llm_result(self, mock_print, mock_input):
        """Accepting should return LLM result unchanged."""
        base_fn = MagicMock()
        llm_fn = MagicMock(return_value=('met', 'evidence', 0.85, None))

        hybrid_fn = create_hybrid_answer_fn(base_fn, llm_fn, interactive_override=True)

        item = DiagnosticItem('subjective', 'mdd_subj_01', 'clinical_significance',
                              'Clinically significant', 'mdd')
        status, evidence, confidence, value = hybrid_fn(item)

        assert status == 'met'
        assert confidence == 0.85
        assert evidence == 'evidence'

    @patch('builtins.input', side_effect=['o', 'm'])
    @patch('builtins.print')
    def test_override_met_returns_clinician_assessment(self, mock_print, mock_input):
        """Override with 'met' should return clinician's assessment."""
        base_fn = MagicMock()
        llm_fn = MagicMock(return_value=('not_met', '', 0.6, None))

        hybrid_fn = create_hybrid_answer_fn(base_fn, llm_fn, interactive_override=True)

        item = DiagnosticItem('subjective', 'mdd_subj_01', 'clinical_significance',
                              'Clinically significant', 'mdd')
        status, _, confidence, _ = hybrid_fn(item)

        assert status == 'met'
        assert confidence == 1.0  # Clinician override has full confidence

    @patch('builtins.input', side_effect=['o', 'n'])
    @patch('builtins.print')
    def test_override_not_met_returns_clinician_assessment(self, mock_print, mock_input):
        """Override with 'not_met' should return clinician's assessment."""
        base_fn = MagicMock()
        llm_fn = MagicMock(return_value=('met', '', 0.9, None))

        hybrid_fn = create_hybrid_answer_fn(base_fn, llm_fn, interactive_override=True)

        item = DiagnosticItem('subjective', 'mdd_subj_01', 'clinical_significance',
                              'Clinically significant', 'mdd')
        status, _, confidence, _ = hybrid_fn(item)

        assert status == 'not_met'
        assert confidence == 1.0

    @patch('builtins.input', return_value='u')
    @patch('builtins.print')
    def test_unclear_returns_low_confidence(self, mock_print, mock_input):
        """Marking unclear should return 0.5 confidence."""
        base_fn = MagicMock()
        llm_fn = MagicMock(return_value=('met', '', 0.9, None))

        hybrid_fn = create_hybrid_answer_fn(base_fn, llm_fn, interactive_override=True)

        item = DiagnosticItem('subjective', 'mdd_subj_01', 'clinical_significance',
                              'Clinically significant', 'mdd')
        status, _, confidence, _ = hybrid_fn(item)

        assert status == 'unclear'
        assert confidence == 0.5


class TestDisplayLlmSuggestion:
    """Test LLM suggestion display for interactive mode."""

    @patch('builtins.print')
    def test_displays_criterion_and_status(self, mock_print):
        """Should display criterion description and LLM status."""
        item = DiagnosticItem('subjective', 'mdd_subj_01', 'clinical_significance',
                              'Clinically significant distress', 'mdd')
        result = ('met', 'shows significant impairment', 0.85, None)

        _display_llm_suggestion(item, result)

        # Check print was called with relevant info
        calls = [str(call) for call in mock_print.call_args_list]
        call_str = ' '.join(calls)
        assert 'Clinically significant distress' in call_str
        assert 'MET' in call_str
        assert '85%' in call_str
        assert 'significant impairment' in call_str

    @patch('builtins.print')
    def test_handles_empty_evidence(self, mock_print):
        """Should handle cases with no evidence."""
        item = DiagnosticItem('subjective', 'mdd_subj_01', 'clinical_significance',
                              'Clinically significant', 'mdd')
        result = ('met', '', 0.9, None)

        _display_llm_suggestion(item, result)

        # Should not raise, should complete
        assert mock_print.called
