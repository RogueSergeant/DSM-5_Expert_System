"""Proof tree explanation formatter for diagnostic reasoning."""

from src.utils.formatting import status_badge, _c, Colours


def format_proof_tree(explanation: dict) -> str:
    """Format Prolog explanation as readable proof tree.

    Args:
        explanation: Dict from explain_diagnosis/3 predicate

    Returns:
        Formatted proof tree string
    """
    lines = []

    # Header
    disorder = explanation.get('disorder', 'Unknown')
    disorder_id = explanation.get('disorder_id', '')
    status = explanation.get('overall_status', 'unknown')
    confidence = explanation.get('confidence', 0.0)

    lines.append(_c(f"{disorder} ({disorder_id.upper()})", Colours.BOLD))
    lines.append(f"Status: {status_badge(status)}  Confidence: {confidence:.0%}")
    lines.append("")

    criteria = explanation.get('criteria', {})

    # Symptoms section
    symptoms = criteria.get('symptoms', {})
    lines.extend(_format_symptoms_section(symptoms))
    lines.append("")

    # Duration section
    duration = criteria.get('duration', {})
    lines.extend(_format_duration_section(duration))
    lines.append("")

    # Onset section
    onset = criteria.get('onset', {})
    lines.extend(_format_onset_section(onset))
    lines.append("")

    # Exclusions section
    exclusions = criteria.get('exclusions', {})
    lines.extend(_format_exclusions_section(exclusions))
    lines.append("")

    # Subjective section
    subjective = criteria.get('subjective', {})
    lines.extend(_format_subjective_section(subjective))

    return '\n'.join(lines)


def _format_symptoms_section(symptoms: dict) -> list[str]:
    """Format symptoms criterion section."""
    lines = []
    status = symptoms.get('status', 'unknown')
    lines.append(f"Symptoms {status_badge(status)}")

    # Category results
    category_results = symptoms.get('category_results', [])
    if category_results:
        for cat in category_results:
            cat_id = cat.get('category_id', 'unknown')
            cat_status = cat.get('status', 'unknown')
            present = cat.get('present_count', 0)
            required = cat.get('required_count', 0)
            lines.append(f"  {cat_id}: {present}/{required} {status_badge(cat_status)}")

    # Symptom evidence
    evidence = symptoms.get('evidence', [])
    if evidence:
        lines.append("  Evidence:")
        for ev in evidence:
            sym_id = ev.get('symptom_id', '')
            sym_status = ev.get('status', 'unknown')
            desc = ev.get('description', '')[:50]
            if sym_status == 'present':
                marker = _c("[+]", Colours.GREEN)
            elif sym_status == 'absent':
                marker = _c("[-]", Colours.RED)
            else:
                marker = _c("[?]", Colours.YELLOW)
            lines.append(f"    {marker} {sym_id}: {desc}")

    return lines


def _format_duration_section(duration: dict) -> list[str]:
    """Format duration criterion section."""
    lines = []
    status = duration.get('status', 'unknown')
    lines.append(f"Duration {status_badge(status)}")

    details = duration.get('details', [])
    if details:
        actual = None
        required = None
        for d in details:
            if isinstance(d, str):
                if d.startswith('actual_days('):
                    actual = d.split('(')[1].rstrip(')')
                elif d.startswith('required_days('):
                    required = d.split('(')[1].rstrip(')')
            elif hasattr(d, 'functor'):
                # Handle Prolog term structure
                if str(d.functor) == 'actual_days':
                    actual = str(d.args[0]) if d.args else None
                elif str(d.functor) == 'required_days':
                    required = str(d.args[0]) if d.args else None

        if actual and required:
            lines.append(f"  Required: {required} days | Actual: {actual} days")
        elif required:
            lines.append(f"  Required: {required} days | Actual: (missing)")

    return lines


def _format_onset_section(onset: dict) -> list[str]:
    """Format onset criterion section."""
    lines = []
    status = onset.get('status', 'unknown')

    if status == 'not_applicable':
        lines.append(f"Onset {status_badge('not_applicable')}")
        return lines

    lines.append(f"Onset {status_badge(status)}")

    details = onset.get('details', [])
    if details:
        onset_age = None
        max_age = None
        for d in details:
            if isinstance(d, str):
                if d.startswith('onset_age('):
                    onset_age = d.split('(')[1].rstrip(')')
                elif d.startswith('max_age('):
                    max_age = d.split('(')[1].rstrip(')')
            elif hasattr(d, 'functor'):
                if str(d.functor) == 'onset_age':
                    onset_age = str(d.args[0]) if d.args else None
                elif str(d.functor) == 'max_age':
                    max_age = str(d.args[0]) if d.args else None

        if onset_age and max_age:
            lines.append(f"  Required: before age {max_age} | Actual: age {onset_age}")
        elif max_age:
            lines.append(f"  Required: before age {max_age} | Actual: (missing)")

    return lines


def _format_exclusions_section(exclusions: dict) -> list[str]:
    """Format exclusions criterion section."""
    lines = []
    status = exclusions.get('status', 'unknown')

    if status == 'not_applicable':
        lines.append(f"Exclusions {status_badge('not_applicable')}")
        return lines

    lines.append(f"Exclusions {status_badge(status)}")

    evidence = exclusions.get('evidence', [])
    if evidence:
        for ev in evidence:
            exc_id = ev.get('exclusion_id', '')
            exc_status = ev.get('status', 'unknown')
            desc = ev.get('description', '')[:40]
            if exc_status == 'cleared':
                marker = _c("[ok]", Colours.GREEN)
            elif exc_status == 'excluded':
                marker = _c("[X]", Colours.RED)
            else:
                marker = _c("[?]", Colours.YELLOW)
            lines.append(f"  {marker} {exc_id}: {desc}")

    return lines


def _format_subjective_section(subjective: dict) -> list[str]:
    """Format subjective criterion section."""
    lines = []
    status = subjective.get('status', 'unknown')

    if status == 'not_applicable':
        lines.append(f"Subjective {status_badge('not_applicable')}")
        return lines

    lines.append(f"Subjective {status_badge(status)}")

    evidence = subjective.get('evidence', [])
    if evidence:
        for ev in evidence:
            crit_id = ev.get('criterion_id', '')
            crit_status = ev.get('status', 'unknown')
            conf = ev.get('confidence', 1.0)
            desc = ev.get('description', '')[:40]
            if crit_status == 'met':
                marker = _c("[+]", Colours.GREEN)
            elif crit_status == 'not_met':
                marker = _c("[-]", Colours.RED)
            else:
                marker = _c("[?]", Colours.YELLOW)
            lines.append(f"  {marker} {crit_id}: {desc} ({conf:.0%})")

    return lines
