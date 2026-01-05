"""Evaluation module for vignette-based testing."""

# Answer modes are safe to import eagerly
from src.evaluation.answer_modes import (
    create_preextracted_answer_fn,
    create_interactive_answer_fn,
    create_llm_answer_fn,
)

# Lazy import evaluate.py functions to avoid RuntimeWarning when running as __main__
def __getattr__(name):
    if name in ('evaluate_on_vignettes', 'evaluate_vignette', 'load_vignettes', 'EvaluationResult'):
        from src.evaluation import evaluate
        return getattr(evaluate, name)
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")

__all__ = [
    'create_preextracted_answer_fn',
    'create_interactive_answer_fn',
    'create_llm_answer_fn',
    'evaluate_on_vignettes',
    'evaluate_vignette',
    'load_vignettes',
    'EvaluationResult',
]
