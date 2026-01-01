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

    # Calculate dynamic figure height based on number of symptom categories
    num_categories = len(disorder_data.get('symptom_categories', []))

    # Each category needs ~2.6 units of vertical space
    # Base space needed: ~15 units for non-category elements
    # (patient presentation, duration, exclusion screening, exclusions cleared, LLM assessment, final diagnosis)
    category_space = num_categories * 2.6
    base_space = 15
    total_y_range = category_space + base_space

    # Scale figure height proportionally (baseline: 18 inches for 20 units)
    fig_height = max(18, int(18 * (total_y_range / 20)))

    fig, ax = plt.subplots(figsize=(14, fig_height))
    ax.set_xlim(0, 14)
    ax.set_ylim(-4, total_y_range - 4)
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

    # Title (positioned relative to top of canvas)
    title_y = total_y_range - 4.7
    subtitle_y = total_y_range - 5.2
    disorder_name = disorder_data['disorder']['Name']
    ax.text(7, title_y, f"{disorder_name} Diagnostic Algorithm",
            ha='center', fontsize=16, fontweight='bold', color='#2c3e50')
    ax.text(7, subtitle_y, "(Prolog Expert System + LLM Hybrid)",
            ha='center', fontsize=11, fontstyle='italic', color='#7f8c8d')

    # ===== LEGEND (top left, positioned relative to top) =====
    legend_top_y = total_y_range - 5.5
    lx, ly = 1.8, legend_top_y
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

    # Extract disorder metadata for dynamic generation
    disorder_abbrev = disorder_id.upper()
    symptom_categories = disorder_data.get('symptom_categories', [])

    # 2-N. Dynamic symptom category checks
    # Calculate dynamic Y positions based on number of categories
    start_y = total_y_range - 8.2
    spacing = 2.6  # Vertical space between decision nodes

    # 1. Patient Presentation (positioned relative to top)
    presentation_y = total_y_range - 6.2
    draw_box(cx, presentation_y, "Patient Presentation", colors['input'], width=3.5)
    # Arrow to first symptom category (calculated position)
    draw_arrow(cx, presentation_y - 0.4, cx, start_y + 1, '#2c3e50')

    for i, category in enumerate(symptom_categories):
        y_pos = start_y - (i * spacing)

        # Generate dynamic label based on category type
        symptom_count = len(category.get('Symptoms', []))
        required_count = category.get('Count', 0)
        category_type = category.get('Type', 'at_least')
        category_id = category.get('CatID', f'category_{i}')

        # Create human-readable category name from CatID
        category_name = category_id.replace('_', ' ').replace(' symptoms', '').title()

        # Generate label based on category type and whether there are multiple categories
        if len(symptom_categories) > 1:
            # Multiple categories - add category name to distinguish them
            if category_type == 'at_least_one_of':
                label = f"{category_name}:\nAt least 1\npresent?"
            elif category_type == 'at_least':
                label = f"{category_name}:\n≥{required_count} of {symptom_count}?"
            elif category_type == 'exactly':
                label = f"{category_name}:\nExactly {required_count}?"
            elif category_type == 'all':
                label = f"{category_name}:\nAll {symptom_count}?"
            else:
                label = f"{category_name}:\n{required_count} of {symptom_count}?"
        else:
            # Single category - no need for category name
            if category_type == 'at_least_one_of':
                label = f"At least 1\ncore symptom\npresent?"
            elif category_type == 'at_least':
                label = f"≥{required_count} of {symptom_count}\nsymptoms?"
            elif category_type == 'exactly':
                label = f"Exactly {required_count}\nsymptoms?"
            elif category_type == 'all':
                label = f"All {symptom_count}\nsymptoms?"
            else:
                label = f"{required_count} of {symptom_count}\nsymptoms?"

        draw_diamond(cx, y_pos, label, colors['decision'])

        # Calculate next element position for arrow endpoint
        next_y = y_pos - spacing
        draw_arrow(cx, y_pos - 1, cx, next_y + 1, colors['arrow_yes'], "YES")
        draw_arrow(cx + 1.3, y_pos, 11.5, y_pos, colors['arrow_no'], "NO")
        draw_box(12.5, y_pos, f"Rule Out\n{disorder_abbrev}", colors['failure'], width=1.8, height=0.7, fontsize=9)

    # Duration check (comes after all symptom categories)
    num_categories = len(symptom_categories)
    duration_y = start_y - (num_categories * spacing)

    dur = disorder_data['duration']
    # Handle disorders with quantitative duration (MDD, GAD, ADHD, PTSD)
    if dur:
        dur_text = f"Duration\n≥{dur['Dur']} {dur['Unit']}?"
    # Handle disorders with qualitative temporal requirement (ASD)
    elif disorder_id == 'asd':
        dur_text = "Present in\nearly dev.\nperiod?"
    # Generic fallback
    else:
        dur_text = "Duration\nmet?"

    draw_diamond(cx, duration_y, dur_text, colors['decision'])

    # Arrow to exclusion screening
    excl_y = duration_y - spacing
    draw_arrow(cx, duration_y - 1, cx, excl_y + 0.35, colors['arrow_yes'], "YES")
    draw_arrow(cx + 1.3, duration_y, 11.5, duration_y, colors['arrow_no'], "NO")
    draw_box(12.5, duration_y, f"Rule Out\n{disorder_abbrev}", colors['failure'], width=1.8, height=0.7, fontsize=9)

    # Exclusion screening (dynamic text from disorder_data)
    draw_box(cx, excl_y, "Exclusion Screening", colors['process'], width=4, height=0.7)

    # Generate exclusion text from first 4 exclusions
    exclusions = disorder_data.get('exclusions', [])[:4]
    if exclusions:
        excl_items = []
        for excl in exclusions:
            desc = excl.get('Desc', '')
            # Truncate long descriptions to fit
            if len(desc) > 30:
                desc = desc[:27] + '...'
            excl_items.append(f"• {desc}")

        # Format as 2 columns if we have 4 items
        if len(excl_items) >= 4:
            excl_text = f"{excl_items[0]}  {excl_items[1]}\n{excl_items[2]}  {excl_items[3]}"
        elif len(excl_items) >= 2:
            excl_text = f"{excl_items[0]}  {excl_items[1]}"
            if len(excl_items) > 2:
                excl_text += f"\n{excl_items[2]}"
        else:
            excl_text = "\n".join(excl_items)
    else:
        excl_text = "• Check exclusion criteria"

    ax.text(cx, excl_y - 0.5, excl_text, ha='center', va='top', fontsize=7, color='#2c3e50',
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#eaf2f8', edgecolor='#85c1e9'))

    # Exclusions cleared
    excl_cleared_y = excl_y - 2.4
    draw_arrow(cx, excl_y - 0.9, cx, excl_cleared_y + 1, '#2c3e50')

    draw_diamond(cx, excl_cleared_y, "All exclusions\ncleared?", colors['decision'])

    # LLM Assessment (Tier B)
    llm_y = excl_cleared_y - 2.6
    draw_arrow(cx, excl_cleared_y - 1, cx, llm_y + 0.5, colors['arrow_yes'], "YES")
    draw_arrow(cx + 1.3, excl_cleared_y, 11.5, excl_cleared_y, colors['arrow_no'], "NO")
    draw_box(12.5, excl_cleared_y, "Alternative\nDiagnosis", colors['failure'], width=1.8, height=0.7, fontsize=9)

    draw_box(cx, llm_y, "Tier B: LLM Assessment\n(Clinical Significance)",
             colors['llm'], width=4, height=1, fontsize=10)

    # Final Diagnosis (dynamic disorder name)
    final_y = llm_y - 2.1
    draw_arrow(cx, llm_y - 0.5, cx, final_y + 0.45, colors['arrow_yes'], "MET")

    # NOT MET path - consistent red box style
    draw_arrow(cx + 2, llm_y, 11.5, llm_y, colors['arrow_no'], "NO")
    draw_box(12.5, llm_y, "Subclinical\nSymptoms", colors['failure'], width=1.8, height=0.7, fontsize=9)

    draw_box(cx, final_y, f"{disorder_abbrev} DIAGNOSIS CONFIRMED", colors['success'], width=4.5, height=0.9, fontsize=12)

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
