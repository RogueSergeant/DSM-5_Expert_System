"""
DSM-5 Criteria Extraction Module

Automates extraction of DSM-5 diagnostic criteria from text into structured
Prolog (.pl) files using various LLM providers (OpenAI, Anthropic, Ollama).

Usage from notebook:
    >>> from src.extraction import (
    ...     OllamaProvider, AnthropicProvider, OpenAIProvider,
    ...     get_dsm5_text, get_template_guide, get_schema_reference
    ... )
    >>> provider = AnthropicProvider(thinking_budget=15000)
    >>> result = provider.extract(
    ...     dsm5_text=get_dsm5_text('ptsd'),
    ...     disorder_id='ptsd',
    ...     template_guide=get_template_guide(),
    ...     custom_prompt="Your custom prompt here..."
    ... )
"""

from .base import (
    ExtractionProvider,
    ExtractionResult,
    get_dsm5_text,
    get_template_guide,
    get_schema_reference,
    get_project_root,
    load_file,
)
from .config import Config
from .providers.ollama_provider import OllamaProvider
from .providers.anthropic_provider import AnthropicProvider
from .providers.openai_provider import OpenAIProvider
from .evaluate import (
    ValidationResult,
    validate_syntax,
    validate_with_schema,
    evaluate_extraction,
    compare_with_gold_standard,
    print_validation_report,
)

__all__ = [
    # Base classes
    'ExtractionProvider',
    'ExtractionResult',
    # Providers
    'OllamaProvider',
    'AnthropicProvider',
    'OpenAIProvider',
    # Configuration
    'Config',
    # Helper functions
    'get_dsm5_text',
    'get_template_guide',
    'get_schema_reference',
    'get_project_root',
    'load_file',
    # Validation
    'ValidationResult',
    'validate_syntax',
    'validate_with_schema',
    'evaluate_extraction',
    'compare_with_gold_standard',
    'print_validation_report',
]
