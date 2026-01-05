"""Terminal formatting utilities with ANSI colours."""

import sys


class Colours:
    """ANSI colour codes for terminal output."""
    GREEN = '\033[92m'
    RED = '\033[91m'
    YELLOW = '\033[93m'
    CYAN = '\033[96m'
    BOLD = '\033[1m'
    RESET = '\033[0m'

    @classmethod
    def enabled(cls) -> bool:
        """Check if colours should be enabled (TTY check)."""
        return hasattr(sys.stdout, 'isatty') and sys.stdout.isatty()


def _c(text: str, colour: str) -> str:
    """Apply colour if terminal supports it."""
    if Colours.enabled():
        return f"{colour}{text}{Colours.RESET}"
    return text


def status_badge(status: str) -> str:
    """Return coloured status badge.

    Args:
        status: One of met, not_met, cleared, excluded, missing_data,
                unclear, pruned, correct, incorrect, present, absent

    Returns:
        Coloured badge like [MET] or [NOT MET]
    """
    status_lower = status.lower().replace('_', ' ')

    # Green statuses
    if status_lower in ('met', 'cleared', 'correct', 'present'):
        return _c(f"[{status_lower.upper()}]", Colours.GREEN)

    # Red statuses
    if status_lower in ('not met', 'excluded', 'incorrect', 'absent'):
        return _c(f"[{status_lower.upper()}]", Colours.RED)

    # Yellow statuses
    if status_lower in ('missing data', 'unclear', 'pruned', 'incomplete'):
        return _c(f"[{status_lower.upper()}]", Colours.YELLOW)

    # Default: cyan
    return _c(f"[{status_lower.upper()}]", Colours.CYAN)


def format_header(title: str, width: int = 60) -> str:
    """Format a section header with box drawing.

    Args:
        title: Header text
        width: Total width of header box

    Returns:
        Boxed header string
    """
    title = title.upper()
    padding = (width - len(title) - 2) // 2
    header_line = '+' + '-' * (width - 2) + '+'
    title_line = '|' + ' ' * padding + title + ' ' * (width - padding - len(title) - 2) + '|'

    return _c(f"{header_line}\n{title_line}\n{header_line}", Colours.CYAN)


def format_metric(label: str, value: float, total: int = None, as_percent: bool = True) -> str:
    """Format a metric with optional ratio.

    Args:
        label: Metric name
        value: Metric value (0.0-1.0 if percent, or raw count)
        total: Optional denominator for ratio display
        as_percent: Whether to display as percentage

    Returns:
        Formatted metric string
    """
    if as_percent:
        pct_str = f"{value * 100:.1f}%"
        if total is not None:
            correct = int(value * total)
            return f"{label}: {_c(pct_str, Colours.GREEN if value >= 0.9 else Colours.YELLOW)} ({correct}/{total})"
        return f"{label}: {_c(pct_str, Colours.GREEN if value >= 0.9 else Colours.YELLOW)}"
    else:
        return f"{label}: {value:.1f}" + (f" (n={total})" if total else "")


def format_table(headers: list[str], rows: list[list[str]], col_widths: list[int] = None) -> str:
    """Format data as an ASCII table.

    Args:
        headers: Column header names
        rows: List of row data (each row is a list of strings)
        col_widths: Optional fixed column widths

    Returns:
        Formatted ASCII table string
    """
    if not rows:
        return "(no data)"

    # Calculate column widths
    if col_widths is None:
        col_widths = []
        for i, h in enumerate(headers):
            max_width = len(h)
            for row in rows:
                if i < len(row):
                    max_width = max(max_width, len(str(row[i])))
            col_widths.append(max_width + 2)

    # Build separator line
    sep = '+' + '+'.join('-' * w for w in col_widths) + '+'

    # Build header row
    header_cells = []
    for i, h in enumerate(headers):
        header_cells.append(h.center(col_widths[i]))
    header_row = '|' + '|'.join(header_cells) + '|'

    # Build data rows
    data_rows = []
    for row in rows:
        cells = []
        for i, cell in enumerate(row):
            if i < len(col_widths):
                cells.append(str(cell).center(col_widths[i]))
        data_rows.append('|' + '|'.join(cells) + '|')

    # Assemble table
    lines = [sep, _c(header_row, Colours.BOLD), sep]
    lines.extend(data_rows)
    lines.append(sep)

    return '\n'.join(lines)


def format_run_header(
    mode: str,
    vignette_count: int,
    disorder_filter: str = None,
    difficulty_filter: str = None,
    subjective_model: str = 'none'
) -> str:
    """Format evaluation run metadata header.

    Args:
        mode: Answer mode (preextracted, interactive, llm)
        vignette_count: Number of vignettes loaded
        disorder_filter: Optional disorder filter applied
        difficulty_filter: Optional difficulty filter applied
        subjective_model: Subjective criteria LLM provider

    Returns:
        Formatted run header string
    """
    from datetime import datetime

    lines = [format_header("Evaluation Run")]
    lines.append(f"Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    mode_str = mode
    if subjective_model != 'none':
        mode_str += f" | Subjective: {subjective_model}"
    lines.append(f"Mode: {mode_str}")

    filter_parts = []
    if disorder_filter:
        filter_parts.append(f"disorder={disorder_filter}")
    if difficulty_filter:
        filter_parts.append(f"difficulty={difficulty_filter}")

    filter_str = f" (filtered: {', '.join(filter_parts)})" if filter_parts else ""
    lines.append(f"Vignettes: {vignette_count}{filter_str}")

    return '\n'.join(lines)


def format_vignette_result(
    vignette_id: str,
    difficulty: str,
    ground_truth: list[str],
    predicted: str,
    status: str,
    correct: bool,
    questions: int
) -> str:
    """Format a single vignette evaluation result.

    Args:
        vignette_id: Vignette identifier
        difficulty: Difficulty level
        ground_truth: Expected disorder(s)
        predicted: Predicted disorder
        status: Prediction status (met/not_met/etc)
        correct: Whether prediction matches ground truth
        questions: Number of questions asked

    Returns:
        Formatted result line
    """
    badge = status_badge('correct' if correct else 'incorrect')
    gt_str = '+'.join(ground_truth)
    return f"{badge} {vignette_id} | {difficulty:10s} | GT: {gt_str:8s} | Pred: {predicted} ({status}) | Qs: {questions}"
