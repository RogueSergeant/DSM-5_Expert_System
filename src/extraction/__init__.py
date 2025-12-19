"""
DSM-5 Criteria Extraction Module

Automates extraction of DSM-5 diagnostic criteria from text into structured
Prolog (.pl) files using various LLM providers (OpenAI, Anthropic, Ollama).
"""

from .base import ExtractionProvider, ExtractionResult
from .config import Config

__all__ = ['ExtractionProvider', 'ExtractionResult', 'Config']
