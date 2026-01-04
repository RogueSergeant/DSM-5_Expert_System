"""
Knowledge Base Exploration Utilities

Utility functions for inspecting and exploring the Prolog knowledge base.
These are useful for debugging, testing, and understanding disorder structures.

Usage:
    from pathlib import Path
    from src.reasoning.engine import PrologEngine
    from src.reasoning.utils import explore_disorder, list_disorders

    engine = PrologEngine(Path('src/prolog'))
    engine.load_file('schema.pl')
    engine.load_file('gold_standard/loader.pl')

    # List all disorders
    disorders = list_disorders(engine)

    # Explore a specific disorder
    mdd_kb = explore_disorder(engine, 'mdd')
    print(f"MDD has {len(mdd_kb['symptoms'])} symptoms")

Extracted from notebooks/main.ipynb (Cells 10-11)
"""

from typing import Dict, List, Optional
from .engine import PrologEngine


def explore_disorder(engine: PrologEngine, disorder_id: str) -> Dict:
    """
    Extract all knowledge about a disorder from the KB.

    This function queries all major predicate types for a given disorder
    and returns a comprehensive summary of its diagnostic criteria.

    Args:
        engine: Initialized PrologEngine instance
        disorder_id: Disorder identifier (e.g., 'mdd', 'gad', 'adhd')

    Returns:
        Dictionary containing all disorder knowledge:
            - disorder: Basic disorder info (ID, Name, Category)
            - symptoms: List of all symptoms
            - symptom_categories: Symptom groupings with count requirements
            - duration: Duration requirement (if any)
            - onset: Onset requirement (if any)
            - exclusions: Exclusion criteria
            - subjective: Subjective criteria requiring clinical judgment
            - specifiers: Diagnosis qualifiers (severity, features, etc.)

    Example:
        >>> mdd_kb = explore_disorder(engine, 'mdd')
        >>> print(f"MDD has {len(mdd_kb['symptoms'])} symptoms")
        MDD has 9 symptoms
        >>> print(f"Duration requirement: {mdd_kb['duration']}")
        Duration requirement: {'Dur': 2, 'Unit': 'weeks'}
    """
    return {
        'disorder': engine.query_one(f"disorder({disorder_id}, Name, Category)"),
        'symptoms': engine.query(f"symptom({disorder_id}, SID, Cat, Desc)"),
        'symptom_categories': engine.query(f"symptom_category({disorder_id}, CatID, Symptoms, Count, Type)"),
        'duration': engine.query_one(f"duration_requirement({disorder_id}, Dur, Unit)"),
        'onset': engine.query_one(f"onset_requirement({disorder_id}, Type, Value)"),
        'exclusions': engine.query(f"exclusion_criterion({disorder_id}, ExcID, Type, Desc)"),
        'subjective': engine.query(f"subjective_criterion({disorder_id}, CritID, Desc, Type)"),
        'specifiers': engine.query(f"specifier({disorder_id}, Type, Options, Desc)")
    }


def list_disorders(engine: PrologEngine) -> List[str]:
    """
    List all disorder IDs in the knowledge base.

    Args:
        engine: Initialized PrologEngine instance

    Returns:
        List of disorder ID strings (e.g., ['mdd', 'gad', 'adhd', 'ptsd'])

    Example:
        >>> disorders = list_disorders(engine)
        >>> print(f"Loaded {len(disorders)} disorders")
        Loaded 4 disorders
    """
    results = engine.query("disorder(ID, _, _)")
    return [str(r['ID']) for r in results]


def count_symptoms(engine: PrologEngine, disorder_id: str) -> int:
    """
    Count the number of symptoms defined for a disorder.

    Args:
        engine: Initialized PrologEngine instance
        disorder_id: Disorder identifier (e.g., 'mdd')

    Returns:
        Number of symptoms defined for the disorder

    Example:
        >>> count = count_symptoms(engine, 'mdd')
        >>> print(f"MDD has {count} symptoms")
        MDD has 9 symptoms
    """
    symptoms = engine.query(f"symptom({disorder_id}, SID, Cat, Desc)")
    return len(symptoms)


def get_disorder_info(engine: PrologEngine, disorder_id: str) -> Optional[Dict]:
    """
    Get basic disorder information.

    Args:
        engine: Initialized PrologEngine instance
        disorder_id: Disorder identifier

    Returns:
        Dictionary with disorder ID, Name, and Category, or None if not found

    Example:
        >>> info = get_disorder_info(engine, 'mdd')
        >>> print(f"{info['ID']}: {info['Name']}")
        mdd: Major Depressive Disorder
    """
    return engine.query_one(f"disorder({disorder_id}, Name, Category)")


def validate_disorder(engine: PrologEngine, disorder_id: str) -> List[str]:
    """
    Validate a disorder's knowledge base completeness.

    Checks for common issues:
    - Missing symptoms
    - Missing symptom categories
    - Missing clinical significance criterion
    - Undefined symptoms referenced in categories

    Args:
        engine: Initialized PrologEngine instance
        disorder_id: Disorder identifier

    Returns:
        List of validation issue strings (empty list if no issues)

    Example:
        >>> issues = validate_disorder(engine, 'mdd')
        >>> if not issues:
        ...     print("MDD knowledge base is valid")
        MDD knowledge base is valid
    """
    issues = engine.query(f"validate_disorder({disorder_id}, Issues)")
    if issues and 'Issues' in issues[0]:
        # Prolog returns issues as a list
        return issues[0]['Issues'] if isinstance(issues[0]['Issues'], list) else [issues[0]['Issues']]
    return []


def print_disorder_summary(engine: PrologEngine, disorder_id: str):
    """
    Print a human-readable summary of a disorder's knowledge base.

    Convenience function for quick inspection during development/debugging.

    Args:
        engine: Initialized PrologEngine instance
        disorder_id: Disorder identifier

    Example:
        >>> print_disorder_summary(engine, 'mdd')
        ============================================================
        Disorder: Major Depressive Disorder (depressive_disorders)
        ============================================================
        Symptoms: 9
        Symptom Categories: 2
        Duration Requirement: 2 weeks
        Onset Requirement: any
        Exclusions: 4
        Subjective Criteria: 1
        Specifiers: 5
        ============================================================
    """
    kb = explore_disorder(engine, disorder_id)

    if not kb['disorder']:
        print(f"Error: Disorder '{disorder_id}' not found in knowledge base")
        return

    print("=" * 60)
    print(f"Disorder: {kb['disorder']['Name']} ({kb['disorder']['Category']})")
    print("=" * 60)
    print(f"Symptoms: {len(kb['symptoms'])}")
    print(f"Symptom Categories: {len(kb['symptom_categories'])}")

    if kb['duration']:
        print(f"Duration Requirement: {kb['duration']['Dur']} {kb['duration']['Unit']}")
    else:
        print("Duration Requirement: None")

    if kb['onset']:
        print(f"Onset Requirement: {kb['onset']['Type']} - {kb['onset']['Value']}")
    else:
        print("Onset Requirement: None")

    print(f"Exclusions: {len(kb['exclusions'])}")
    print(f"Subjective Criteria: {len(kb['subjective'])}")
    print(f"Specifiers: {len(kb['specifiers'])}")
    print("=" * 60)


if __name__ == '__main__':
    """Demo usage of KB exploration utilities."""
    from pathlib import Path

    # Setup paths
    project_root = Path(__file__).parent.parent.parent
    prolog_dir = project_root / 'src' / 'prolog'

    print("Knowledge Base Exploration Demo")
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

    # Explore each disorder
    for disorder_id in disorders:
        print_disorder_summary(engine, disorder_id)
        print()

    # Validate disorders
    print("Validation checks:")
    for disorder_id in disorders:
        issues = validate_disorder(engine, disorder_id)
        if issues:
            print(f"  {disorder_id}: {len(issues)} issue(s) found")
            for issue in issues:
                print(f"    - {issue}")
        else:
            print(f"  {disorder_id}: Valid ✓")
