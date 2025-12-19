"""
Configuration management for DSM-5 extraction module.

This module handles loading and validating configuration from environment
variables. It supports API keys for cloud providers and settings for
local Ollama instances.

Environment Variables:
    OPENAI_API_KEY: API key for OpenAI GPT-5 models
    ANTHROPIC_API_KEY: API key for Anthropic Claude models
    OLLAMA_BASE_URL: URL for local Ollama server (default: http://localhost:11434)
    OPENAI_MODEL: Default OpenAI model (default: gpt-5.2)
    ANTHROPIC_MODEL: Default Anthropic model (default: claude-sonnet-4-5)
    OLLAMA_MODEL: Default Ollama model (default: llama3.1:8b)
    MAX_TOKENS: Maximum tokens in response (default: 16384)
    TEMPERATURE: Sampling temperature (default: 0.1)

Example:
    >>> from src.extraction.config import Config
    >>> config = Config.from_env()
    >>> if config.validate("openai"):
    ...     print(f"Using model: {config.default_openai_model}")
"""

import os
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional
from dotenv import load_dotenv


@dataclass
class Config:
    """
    Configuration for extraction providers.

    This dataclass holds all configuration needed for the extraction module,
    including API keys, default models, and generation settings.

    Attributes:
        openai_api_key: API key for OpenAI (from OPENAI_API_KEY env var)
        anthropic_api_key: API key for Anthropic (from ANTHROPIC_API_KEY env var)
        ollama_base_url: URL for Ollama server (default: http://localhost:11434)
        default_openai_model: Default OpenAI model (default: gpt-5.2)
        default_anthropic_model: Default Anthropic model (default: claude-sonnet-4-5)
        default_ollama_model: Default Ollama model (default: llama3.1:8b)
        max_tokens: Maximum tokens in response (default: 16384)
        temperature: Sampling temperature (default: 0.1)
        output_dir: Directory for saved outputs (default: outputs)

    Example:
        >>> config = Config.from_env()
        >>> print(config.default_openai_model)
        'gpt-5.2'
    """

    # API Keys (loaded from environment)
    openai_api_key: Optional[str] = None
    anthropic_api_key: Optional[str] = None

    # Ollama settings
    ollama_base_url: str = "http://localhost:11434"

    # Default models (as of December 2025)
    # OpenAI: gpt-5.2 is the latest with xhigh reasoning support
    # Anthropic: claude-sonnet-4-5 balances speed and quality
    # Ollama: llama3.1:8b is a good default for local use
    default_openai_model: str = "gpt-5.2"
    default_anthropic_model: str = "claude-sonnet-4-5"
    default_ollama_model: str = "llama3.1:8b"

    # Generation settings
    max_tokens: int = 16384  # Increased for complex Prolog output
    temperature: float = 0.1  # Low temperature for consistent code generation

    # Output directory
    output_dir: Path = field(default_factory=lambda: Path("outputs"))

    @classmethod
    def from_env(cls, env_file: Optional[Path] = None) -> "Config":
        """
        Load configuration from environment variables.

        This method loads a .env file if present and reads configuration
        from environment variables. Any missing values use defaults.

        Args:
            env_file: Optional path to .env file. If not specified,
                      searches current directory and parents.

        Returns:
            Config instance with values from environment.

        Example:
            >>> config = Config.from_env()
            >>> print(config.openai_api_key is not None)
            True
        """
        if env_file and env_file.exists():
            load_dotenv(env_file)
        else:
            load_dotenv()  # Load from .env in current directory or parents

        return cls(
            openai_api_key=os.getenv("OPENAI_API_KEY"),
            anthropic_api_key=os.getenv("ANTHROPIC_API_KEY"),
            ollama_base_url=os.getenv("OLLAMA_BASE_URL", "http://localhost:11434"),
            default_openai_model=os.getenv("OPENAI_MODEL", "gpt-5.2"),
            default_anthropic_model=os.getenv("ANTHROPIC_MODEL", "claude-sonnet-4-5"),
            default_ollama_model=os.getenv("OLLAMA_MODEL", "llama3.1:8b"),
            max_tokens=int(os.getenv("MAX_TOKENS", "16384")),
            temperature=float(os.getenv("TEMPERATURE", "0.1")),
        )

    def validate(self, provider: str) -> bool:
        """
        Check if required config is present for a provider.

        Args:
            provider: Provider name ("openai", "anthropic", or "ollama")

        Returns:
            True if provider is properly configured, False otherwise.

        Example:
            >>> config = Config.from_env()
            >>> if config.validate("openai"):
            ...     provider = OpenAIProvider()
        """
        if provider == "openai":
            return self.openai_api_key is not None
        elif provider == "anthropic":
            return self.anthropic_api_key is not None
        elif provider == "ollama":
            return True  # Ollama runs locally, no API key needed
        return False

    def get_missing_config(self, provider: str) -> Optional[str]:
        """
        Return description of missing config, or None if valid.

        Args:
            provider: Provider name ("openai", "anthropic", or "ollama")

        Returns:
            String describing missing configuration, or None if valid.

        Example:
            >>> config = Config.from_env()
            >>> missing = config.get_missing_config("openai")
            >>> if missing:
            ...     print(f"Error: {missing}")
        """
        if provider == "openai" and not self.openai_api_key:
            return "OPENAI_API_KEY environment variable not set"
        if provider == "anthropic" and not self.anthropic_api_key:
            return "ANTHROPIC_API_KEY environment variable not set"
        return None


# Template for .env file
ENV_TEMPLATE = """# API Keys for extraction providers
# Get your API keys from:
#   OpenAI: https://platform.openai.com/api-keys
#   Anthropic: https://console.anthropic.com/settings/keys

OPENAI_API_KEY=your-openai-api-key-here
ANTHROPIC_API_KEY=your-anthropic-api-key-here

# Ollama settings (local, no API key needed)
# Start Ollama with: ollama serve
OLLAMA_BASE_URL=http://localhost:11434

# Default models (as of December 2025)
# OpenAI GPT-5: gpt-5.2 (latest), gpt-5.1, gpt-5
# Claude: claude-sonnet-4-5 (recommended), claude-opus-4-5, claude-haiku-3-5
# Ollama: llama3.1:8b, gpt-oss:20b, deepseek-r1:7b, qwq:32b
OPENAI_MODEL=gpt-5.2
ANTHROPIC_MODEL=claude-sonnet-4-5
OLLAMA_MODEL=llama3.1:8b

# Generation settings
MAX_TOKENS=16384
TEMPERATURE=0.1
"""


def create_env_template(path: Path = Path(".env.example")) -> None:
    """
    Create a template .env file.

    Args:
        path: Path for the template file (default: .env.example)

    Example:
        >>> create_env_template()
        Created .env.example
    """
    with open(path, 'w') as f:
        f.write(ENV_TEMPLATE)
    print(f"Created {path}")
