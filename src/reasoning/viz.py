"""
Diagnostic Visualization Utilities

Functions for visualizing the diagnostic reasoning process as flowcharts.
These visualizations help understand the decision flow implemented by the
Prolog expert system.

Usage:
    from pathlib import Path
    from src.reasoning.engine import PrologEngine
    from src.reasoning.utils import explore_disorder
    from src.reasoning.viz import visualise_diagnostic_flowchart

    engine = PrologEngine(Path('src/prolog'))
    engine.load_file('schema.pl')
    engine.load_file('gold_standard/loader.pl')

    mdd_data = explore_disorder(engine, 'mdd')
    visualise_diagnostic_flowchart('mdd', mdd_data, save_to_file=Path('docs/images/mdd_flowchart.png'))

Extracted from notebooks/main.ipynb (Cell 12)
"""

from pathlib import Path
from typing import Dict, Optional

# Conditional matplotlib import with helpful error message
try:
    import matplotlib.pyplot as plt
    from matplotlib.patches import FancyBboxPatch
    MATPLOTLIB_AVAILABLE = True
except ImportError:
    MATPLOTLIB_AVAILABLE = False
    _MATPLOTLIB_ERROR_MSG = """
    Matplotlib is required for visualization but not installed.

    Install with: pip install matplotlib

    Or install all optional dependencies: pip install -r requirements.txt
    """


def visualise_diagnostic_flowchart(
    disorder_id: str,
    disorder_data: Dict,
    save_to_file: Optional[Path] = None
):
    """
    Visualise the diagnostic process as a decision flowchart.

    Creates a professional flowchart showing the sequential evaluation
    of diagnostic criteria (symptoms, duration, onset, exclusions) and
    the integration of LLM-based subjective assessment (Tier B).

    Args:
        disorder_id: Disorder identifier (e.g., 'mdd', 'gad')
        disorder_data: Dictionary from explore_disorder() containing:
            - disorder: Basic info (ID, Name, Category)
            - symptoms: List of symptoms
            - duration: Duration requirement
            - exclusions: Exclusion criteria
        save_to_file: Optional path to save PNG output. If None, displays interactively.

    Raises:
        ImportError: If matplotlib is not installed

    Example:
        >>> from src.reasoning.engine import PrologEngine
        >>> from src.reasoning.utils import explore_disorder
        >>> from pathlib import Path
        >>>
        >>> engine = PrologEngine(Path('src/prolog'))
        >>> engine.load_file('schema.pl')
        >>> engine.load_file('gold_standard/loader.pl')
        >>>
        >>> mdd_data = explore_disorder(engine, 'mdd')
        >>> visualise_diagnostic_flowchart('mdd', mdd_data, save_to_file=Path('docs/images/mdd_flowchart.png'))
    """
    if not MATPLOTLIB_AVAILABLE:
        raise ImportError(_MATPLOTLIB_ERROR_MSG)

    fig, ax = plt.subplots(figsize=(14, 18))
    ax.set_xlim(0, 14)
    ax.set_ylim(-4, 16)
    ax.axis('off')

    # Professional color palette
    colors = {
        'input': '#4a90d9',
        'decision': '#5dade2',
        'process': '#85c1e9',
        'success': '#27ae60',
        'failure': '#e74c3c',
        'llm': '#8e44ad',
        'arrow_yes': '#27ae60',
        'arrow_no': '#e74c3c',
    }

    def draw_box(x, y, text, color, width=3, height=0.8, fontsize=10):
        """Draw a rounded rectangle box."""
        box = FancyBboxPatch(
            (x - width/2, y - height/2), width, height,
            boxstyle="round,pad=0.05,rounding_size=0.15",
            facecolor=color, edgecolor='#2c3e50', linewidth=2
        )
        ax.add_patch(box)
        ax.text(x, y, text, ha='center', va='center', fontsize=fontsize,
                fontweight='bold', color='white', zorder=3)

    def draw_diamond(x, y, text, color, size=1.0, fontsize=9):
        """Draw a diamond-shaped decision node."""
        diamond = plt.Polygon(
            [(x, y+size), (x+size*1.3, y), (x, y-size), (x-size*1.3, y)],
            facecolor=color, edgecolor='#2c3e50', linewidth=2
        )
        ax.add_patch(diamond)
        ax.text(x, y, text, ha='center', va='center', fontsize=fontsize,
                fontweight='bold', color='white', zorder=3)

    def draw_arrow(x1, y1, x2, y2, color='#2c3e50', label=None):
        """Draw an arrow between two points with optional label."""
        ax.annotate('', xy=(x2, y2), xytext=(x1, y1),
                   arrowprops=dict(arrowstyle='->', color=color, lw=2))
        if label:
            mid_x = (x1 + x2) / 2 + 0.2
            mid_y = (y1 + y2) / 2
            ax.text(mid_x, mid_y, label, fontsize=9, color=color, fontweight='bold')

    # Center x for main flow
    cx = 7

    # Title
    disorder_name = disorder_data['disorder']['Name']
    ax.text(7, 15.3, f"{disorder_name} Diagnostic Algorithm",
            ha='center', fontsize=16, fontweight='bold', color='#2c3e50')
    ax.text(7, 14.8, "(Prolog Expert System + LLM Hybrid)",
            ha='center', fontsize=11, fontstyle='italic', color='#7f8c8d')

    # ===== LEGEND (top left) =====
    lx, ly = 1.8, 14.5
    ax.text(lx, ly + 0.5, "LEGEND", fontsize=10, fontweight='bold', color='#2c3e50', ha='center')

    legend_items = [
        ("Tier A: Prolog", colors['decision']),
        ("Tier B: LLM", colors['llm']),
        ("Pass", colors['success']),
        ("Fail", colors['failure']),
    ]
    for i, (label, color) in enumerate(legend_items):
        ax.add_patch(plt.Rectangle((lx - 0.8, ly - i*0.5 - 0.1), 0.35, 0.3,
                                   facecolor=color, edgecolor='#2c3e50'))
        ax.text(lx - 0.3, ly - i*0.5 + 0.05, label, fontsize=9, va='center', color='#2c3e50')

    # 1. Patient Presentation
    draw_box(cx, 13.8, "Patient Presentation", colors['input'], width=3.5)
    draw_arrow(cx, 13.4, cx, 12.6, '#2c3e50')

    # 2. Core symptom check
    draw_diamond(cx, 11.8, "Core symptom\npresent?\n(A1 or A2)", colors['decision'])
    draw_arrow(cx, 10.8, cx, 10.0, colors['arrow_yes'], "YES")
    draw_arrow(cx + 1.3, 11.8, 11.5, 11.8, colors['arrow_no'], "NO")
    draw_box(12.5, 11.8, "Rule Out\nMDD", colors['failure'], width=1.8, height=0.7, fontsize=9)

    # 3. Symptom count
    draw_diamond(cx, 9.2, "≥5 of 9\nsymptoms?", colors['decision'])
    draw_arrow(cx, 8.2, cx, 7.4, colors['arrow_yes'], "YES")
    draw_arrow(cx + 1.3, 9.2, 11.5, 9.2, colors['arrow_no'], "NO")
    draw_box(12.5, 9.2, "Rule Out\nMDD", colors['failure'], width=1.8, height=0.7, fontsize=9)

    # 4. Duration
    dur = disorder_data['duration']
    dur_text = f"Duration\n≥{dur['Dur']} {dur['Unit']}?" if dur else "Duration\nmet?"
    draw_diamond(cx, 6.6, dur_text, colors['decision'])
    draw_arrow(cx, 5.6, cx, 4.8, colors['arrow_yes'], "YES")
    draw_arrow(cx + 1.3, 6.6, 11.5, 6.6, colors['arrow_no'], "NO")
    draw_box(12.5, 6.6, "Rule Out\nMDD", colors['failure'], width=1.8, height=0.7, fontsize=9)

    # 5. Exclusion screening
    draw_box(cx, 4.3, "Exclusion Screening", colors['process'], width=4, height=0.7)
    excl_text = "• Substance-induced  • Medical condition\n• Psychotic disorder  • Bipolar history"
    ax.text(cx, 3.5, excl_text, ha='center', va='top', fontsize=8, color='#2c3e50',
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#eaf2f8', edgecolor='#85c1e9'))
    draw_arrow(cx, 3.0, cx, 2.4, '#2c3e50')

    # 6. Exclusions cleared
    draw_diamond(cx, 1.6, "All exclusions\ncleared?", colors['decision'])
    draw_arrow(cx, 0.6, cx, -0.1, colors['arrow_yes'], "YES")
    draw_arrow(cx + 1.3, 1.6, 11.5, 1.6, colors['arrow_no'], "NO")
    draw_box(12.5, 1.6, "Alternative\nDiagnosis", colors['failure'], width=1.8, height=0.7, fontsize=9)

    # 7. LLM Assessment (Tier B)
    draw_box(cx, -0.7, "Tier B: LLM Assessment\n(Clinical Significance)",
             colors['llm'], width=4, height=1, fontsize=10)
    draw_arrow(cx, -1.2, cx, -2.0, colors['arrow_yes'], "MET")

    # NOT MET path - consistent red box style
    draw_arrow(cx + 2, -0.7, 11.5, -0.7, colors['arrow_no'], "NO")
    draw_box(12.5, -0.7, "Subclinical\nSymptoms", colors['failure'], width=1.8, height=0.7, fontsize=9)

    # 8. Final Diagnosis
    draw_box(cx, -2.8, "MDD DIAGNOSIS CONFIRMED", colors['success'], width=4.5, height=0.9, fontsize=12)

    plt.tight_layout()

    # Save or display
    if save_to_file:
        save_to_file.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(save_to_file, dpi=150, bbox_inches='tight')
        print(f"Flowchart saved to: {save_to_file}")
        plt.close(fig)
    else:
        plt.show()


if __name__ == '__main__':
    """Demo usage of diagnostic visualization."""
    from pathlib import Path
    from .engine import PrologEngine
    from .utils import explore_disorder, list_disorders

    # Setup paths
    project_root = Path(__file__).parent.parent.parent
    prolog_dir = project_root / 'src' / 'prolog'
    output_dir = project_root / 'docs' / 'images'

    print("Diagnostic Flowchart Visualization Demo")
    print("=" * 60)

    # Initialize engine
    engine = PrologEngine(prolog_dir)
    engine.load_file('schema.pl')
    engine.load_file('gold_standard/loader.pl')
    print()

    # List all disorders
    print("Available disorders:")
    disorders = list_disorders(engine)
    for d in disorders:
        print(f"  - {d}")
    print()

    # Visualize each disorder
    for disorder_id in disorders:
        print(f"Generating flowchart for {disorder_id}...")
        disorder_data = explore_disorder(engine, disorder_id)

        output_path = output_dir / f"{disorder_id}_flowchart.png"
        visualise_diagnostic_flowchart(disorder_id, disorder_data, save_to_file=output_path)

    print()
    print("All flowcharts generated successfully!")
