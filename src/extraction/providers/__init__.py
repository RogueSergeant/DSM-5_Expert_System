"""
LLM Provider implementations for extraction.
"""

from .openai_provider import OpenAIProvider
from .anthropic_provider import AnthropicProvider
from .ollama_provider import OllamaProvider

__all__ = ['OpenAIProvider', 'AnthropicProvider', 'OllamaProvider']


def get_provider(name: str, model: str = None, **kwargs):
    """
    Factory function to get a provider by name.

    Args:
        name: Provider name ('openai', 'anthropic', 'ollama')
        model: Optional model override
        **kwargs: Additional provider-specific arguments

    Returns:
        ExtractionProvider instance
    """
    providers = {
        'openai': OpenAIProvider,
        'anthropic': AnthropicProvider,
        'ollama': OllamaProvider,
    }

    if name not in providers:
        raise ValueError(f"Unknown provider: {name}. Available: {list(providers.keys())}")

    return providers[name](model=model, **kwargs)
