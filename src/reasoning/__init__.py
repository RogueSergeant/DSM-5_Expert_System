"""
Reasoning Module

This module provides Python interfaces for Prolog-based diagnostic reasoning,
knowledge base exploration utilities, and visualization tools.

Extracted from notebooks/main.ipynb for production use.

Components:
    - PrologEngine: Python wrapper for pyswip (SWI-Prolog interface)
    - KB utilities: Functions to explore and validate disorder knowledge
    - Visualization: Diagnostic flowchart generation

Usage:
    from src.reasoning import PrologEngine, explore_disorder
    from pathlib import Path

    engine = PrologEngine(Path('src/prolog'))
    engine.load_file('schema.pl')
    engine.load_file('gold_standard/loader.pl')

    mdd_data = explore_disorder(engine, 'mdd')
    print(f"MDD has {len(mdd_data['symptoms'])} symptoms")
"""

from .engine import PrologEngine
from .utils import (
    explore_disorder,
    list_disorders,
    count_symptoms,
    get_disorder_info,
    validate_disorder,
    print_disorder_summary
)
from .viz import visualise_diagnostic_flowchart

__all__ = [
    'PrologEngine',
    'explore_disorder',
    'list_disorders',
    'count_symptoms',
    'get_disorder_info',
    'validate_disorder',
    'print_disorder_summary',
    'visualise_diagnostic_flowchart',
]
