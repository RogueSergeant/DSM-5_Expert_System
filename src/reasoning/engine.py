"""
Prolog Engine Wrapper

This module provides a clean Python interface to SWI-Prolog via pyswip.
It abstracts file loading, query execution, and fact assertion/retraction.

Design Rationale:
- Wrapping pyswip provides consistent error handling
- Easier testing and mocking compared to direct pyswip usage
- Clean separation between Python and Prolog concerns

Usage:
    from pathlib import Path
    from src.reasoning.engine import PrologEngine

    engine = PrologEngine(Path('src/prolog'))
    engine.load_file('schema.pl')
    engine.load_file('gold_standard/loader.pl')

    results = engine.query("disorder(X, Name, Category)")
    for result in results:
        print(f"{result['X']}: {result['Name']}")

Extracted from notebooks/main.ipynb (Cell 4)
"""

from pathlib import Path
from typing import Dict, List, Optional
from pyswip import Prolog


class PrologEngine:
    """Wrapper around pyswip Prolog for diagnostic reasoning."""

    def __init__(self, prolog_dir: Path):
        """
        Initialize Prolog engine.

        Args:
            prolog_dir: Path to directory containing Prolog files
        """
        self.prolog = Prolog()
        self.prolog_dir = prolog_dir
        self._loaded_files = set()

    def load_file(self, filename: str) -> bool:
        """
        Load a Prolog file from the prolog directory.

        Args:
            filename: Relative path to Prolog file (e.g., 'schema.pl' or 'gold_standard/mdd.pl')

        Returns:
            True if file loaded successfully, False otherwise

        Example:
            engine.load_file('schema.pl')
            engine.load_file('gold_standard/loader.pl')
        """
        filepath = self.prolog_dir / filename
        if not filepath.exists():
            print(f"Error: File not found: {filepath}")
            return False

        try:
            # pyswip requires forward slashes even on Windows
            prolog_path = str(filepath).replace('\\', '/')
            self.prolog.consult(prolog_path)
            self._loaded_files.add(filename)
            print(f"Loaded: {filename}")
            return True
        except Exception as e:
            print(f"Error loading {filename}: {e}")
            return False

    def query(self, query_str: str) -> List[Dict]:
        """
        Execute a Prolog query and return results as list of dicts.

        Args:
            query_str: Prolog query string (e.g., "disorder(X, Name, _)")

        Returns:
            List of result dictionaries, where keys are variable names from query
            Empty list if query fails or returns no results

        Example:
            results = engine.query("disorder(X, Name, Category)")
            # Returns: [{'X': 'mdd', 'Name': 'Major Depressive Disorder', 'Category': 'depressive_disorders'}, ...]
        """
        try:
            results = list(self.prolog.query(query_str))
            return results
        except Exception as e:
            print(f"Query error: {e}")
            return []

    def query_one(self, query_str: str) -> Optional[Dict]:
        """
        Execute a query and return first result or None.

        Convenience method for queries expected to return at most one result.

        Args:
            query_str: Prolog query string

        Returns:
            First result dict if query succeeds, None otherwise

        Example:
            result = engine.query_one("disorder(mdd, Name, Category)")
            # Returns: {'Name': 'Major Depressive Disorder', 'Category': 'depressive_disorders'}
        """
        results = self.query(query_str)
        return results[0] if results else None

    def assert_fact(self, fact: str) -> bool:
        """
        Assert a fact into the Prolog knowledge base.

        Args:
            fact: Prolog fact to assert (e.g., "patient_symptom(pt001, mdd_a1, present, 'Reports sadness')")

        Returns:
            True if assertion succeeded, False otherwise

        Example:
            engine.assert_fact("patient_symptom(pt001, mdd_a1, present, 'Patient reports feeling sad')")
        """
        try:
            list(self.prolog.query(f"assertz({fact})"))
            return True
        except Exception as e:
            print(f"Assert error: {e}")
            return False

    def retract_all(self, pattern: str) -> bool:
        """
        Retract all facts matching a pattern.

        Args:
            pattern: Prolog pattern to match (e.g., "patient_symptom(pt001, _, _, _)")

        Returns:
            True if retraction succeeded, False otherwise

        Example:
            engine.retract_all("patient_symptom(pt001, _, _, _)")  # Clear all patient symptoms
        """
        try:
            list(self.prolog.query(f"retractall({pattern})"))
            return True
        except Exception as e:
            print(f"Retract error: {e}")
            return False


if __name__ == '__main__':
    """Demo usage of PrologEngine."""
    import sys

    # Setup paths
    project_root = Path(__file__).parent.parent.parent
    prolog_dir = project_root / 'src' / 'prolog'

    print("PrologEngine Demo")
    print("=" * 60)
    print(f"Project root: {project_root}")
    print(f"Prolog directory: {prolog_dir}")
    print()

    # Initialize engine
    engine = PrologEngine(prolog_dir)

    # Load files
    print("Loading Prolog files...")
    engine.load_file('schema.pl')
    engine.load_file('gold_standard/loader.pl')
    print()

    # Query disorders
    print("Querying disorders:")
    disorders = engine.query("disorder(ID, Name, Category)")
    for d in disorders:
        print(f"  - {d['ID']}: {d['Name']} ({d['Category']})")
    print()

    # Count symptoms per disorder
    print("Symptom counts:")
    for d in disorders:
        disorder_id = d['ID']
        symptoms = engine.query(f"symptom({disorder_id}, SID, Cat, Desc)")
        print(f"  - {disorder_id}: {len(symptoms)} symptoms")
    print()

    # Demo patient fact assertion
    print("Demo patient fact assertion:")
    engine.assert_fact("patient_symptom(demo_patient, mdd_a1, present, 'Demo evidence')")
    result = engine.query_one("patient_symptom(demo_patient, mdd_a1, Status, Evidence)")
    print(f"  Asserted: {result}")
    engine.retract_all("patient_symptom(demo_patient, _, _, _)")
    print("  Retracted demo facts")
