"""
Ollama local LLM provider for DSM-5 criteria extraction.

This module provides integration with Ollama for running local LLMs to extract
structured Prolog diagnostic criteria from DSM-5 text.

API Documentation:
    - Ollama API: https://github.com/ollama/ollama/blob/main/docs/api.md
    - Python SDK: https://github.com/ollama/ollama-python
    - Model Library: https://ollama.com/library

Ollama Server:
    Ollama runs locally on port 11434 by default. Start with:
    $ ollama serve

Thinking Models:
    Some models support a "think" parameter for enhanced reasoning:
    - gpt-oss: Uses think="low"|"medium"|"high" for reasoning levels
    - deepseek-r1: Uses think=True for reasoning mode
    - qwq: Uses think=True for reasoning mode
    - Other models: May ignore the think parameter

Example Usage:
    >>> from src.extraction.providers import OllamaProvider
    >>> provider = OllamaProvider(model="gpt-oss:20b", think="high")
    >>> result = provider.extract(dsm5_text, "ptsd", template_guide)
"""

import time
from typing import Optional, Union, Literal

from ..base import ExtractionProvider, ExtractionResult
from ..config import Config


# Type alias for thinking levels
ThinkLevel = Union[bool, Literal["low", "medium", "high"]]


class OllamaProvider(ExtractionProvider):
    """
    Ollama local LLM extraction provider.

    This provider uses locally-running Ollama models to extract DSM-5 diagnostic
    criteria into structured Prolog format. It supports the think parameter
    for models with reasoning capabilities.

    Attributes:
        model: The Ollama model to use (default: llama3.1:8b)
        base_url: Ollama server URL (default: http://localhost:11434)
        max_tokens: Maximum tokens in the response (num_predict)
        temperature: Sampling temperature (0.0-2.0)
        think: Thinking/reasoning level for supported models

    Example:
        >>> provider = OllamaProvider(
        ...     model="gpt-oss:20b",
        ...     think="high",
        ...     temperature=0.1
        ... )
        >>> result = provider.extract(
        ...     dsm5_text="...",
        ...     disorder_id="ptsd",
        ...     template_guide="..."
        ... )
        >>> print(result.success, result.duration_seconds)
    """

    def __init__(
        self,
        model: str = None,
        base_url: str = None,
        max_tokens: int = 16384,
        temperature: float = 0.1,
        think: ThinkLevel = None,
        **kwargs
    ):
        """
        Initialize the Ollama provider.

        Args:
            model: Model identifier (default: llama3.1:8b). Popular options:
                   - llama3.1:8b/70b: Meta's Llama 3.1 models
                   - gpt-oss:20b: GPT-OSS with thinking support
                   - deepseek-r1:7b/14b/32b: DeepSeek reasoning models
                   - qwq:32b: Alibaba's QwQ reasoning model
                   - mistral:7b: Mistral AI's base model
            base_url: Ollama server URL (default: http://localhost:11434).
                      Change if running Ollama on a different host/port.
            max_tokens: Maximum tokens in response (default: 16384).
                        Maps to Ollama's num_predict option.
            temperature: Sampling temperature (default: 0.1).
                         Lower values = more deterministic output.
            think: Thinking/reasoning mode for supported models:
                   - None: No thinking (default)
                   - True: Enable thinking (for deepseek-r1, qwq)
                   - "low"/"medium"/"high": Thinking levels (for gpt-oss)
            **kwargs: Additional arguments passed to base class.
        """
        config = Config.from_env()
        model = model or config.default_ollama_model
        super().__init__(model, **kwargs)

        self.base_url = base_url or config.ollama_base_url
        self.max_tokens = max_tokens
        self.temperature = temperature
        self.think = think
        self._client = None

    @property
    def provider_name(self) -> str:
        """Return the provider identifier."""
        return "ollama"

    @property
    def client(self):
        """
        Lazy-load the Ollama client.

        Returns:
            ollama.Client: Configured Ollama client instance.

        Raises:
            ImportError: If ollama package is not installed.
        """
        if self._client is None:
            try:
                import ollama
                self._client = ollama.Client(host=self.base_url)
            except ImportError:
                raise ImportError(
                    "Ollama SDK not installed. Install with: pip install ollama"
                )
        return self._client

    def extract(
        self,
        dsm5_text: str,
        disorder_id: str,
        template_guide: str,
        schema_reference: Optional[str] = None,
        custom_prompt: Optional[str] = None,
        custom_system_prompt: Optional[str] = None,
    ) -> ExtractionResult:
        """
        Extract Prolog diagnostic criteria from DSM-5 text.

        This method sends the DSM-5 text along with the template guide to
        the local Ollama model, requesting structured Prolog output. For
        models with thinking capabilities, the think parameter controls
        the depth of reasoning.

        Args:
            dsm5_text: Raw DSM-5 diagnostic criteria text to extract from.
            disorder_id: Short identifier for the disorder (e.g., 'ptsd', 'asd').
                         Used in the generated Prolog predicates.
            template_guide: Content of gold_standard/README.md explaining
                            the expected Prolog structure.
            schema_reference: Optional content of schema.pl for additional
                              context about predicate signatures.
            custom_prompt: Optional custom prompt (overrides build_prompt).
            custom_system_prompt: Optional custom system prompt.

        Returns:
            ExtractionResult containing:
                - success: Whether extraction completed without errors
                - content: The extracted Prolog code (if successful)
                - model: Model used for extraction
                - provider: "ollama"
                - disorder_id: The disorder identifier
                - duration_seconds: Time taken for extraction
                - input_tokens: Number of input tokens used
                - output_tokens: Number of output tokens generated
                - error: Error message (if failed)

        Example:
            >>> result = provider.extract(
            ...     dsm5_text=open("data/dsm5_text/PTSD.txt").read(),
            ...     disorder_id="ptsd",
            ...     template_guide=open("gold_standard/README.md").read()
            ... )
            >>> if result.success:
            ...     with open("ptsd.pl", "w") as f:
            ...         f.write(result.content)
        """
        start_time = time.time()

        try:
            # Use custom prompts if provided, otherwise build from base class
            prompt = custom_prompt or self.build_prompt(
                dsm5_text, disorder_id, template_guide, schema_reference
            )
            system_prompt = custom_system_prompt or self.build_system_prompt()

            # Build options dict
            # See: https://github.com/ollama/ollama/blob/main/docs/api.md
            options = {
                "num_predict": self.max_tokens,
                "temperature": self.temperature,
            }

            # Add thinking parameter if specified
            # Different models support different thinking formats:
            # - gpt-oss: "low", "medium", "high"
            # - deepseek-r1, qwq: True/False
            if self.think is not None:
                options["think"] = self.think

            # Make API request
            response = self.client.chat(
                model=self.model,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": prompt},
                ],
                options=options,
            )

            # Extract content from response
            content = response['message']['content']
            duration = time.time() - start_time

            # Clean up any markdown formatting the model might add
            content = self._clean_content(content)

            # Ollama provides token counts in eval_count and prompt_eval_count
            input_tokens = response.get('prompt_eval_count')
            output_tokens = response.get('eval_count')

            return ExtractionResult(
                success=True,
                content=content,
                model=self.model,
                provider=self.provider_name,
                disorder_id=disorder_id,
                duration_seconds=duration,
                input_tokens=input_tokens,
                output_tokens=output_tokens,
            )

        except Exception as e:
            duration = time.time() - start_time
            return ExtractionResult(
                success=False,
                content="",
                model=self.model,
                provider=self.provider_name,
                disorder_id=disorder_id,
                duration_seconds=duration,
                error=str(e),
            )

    def _clean_content(self, content: str) -> str:
        """
        Remove markdown code block formatting if present.

        LLMs sometimes wrap code output in markdown code blocks even when
        instructed not to. This method strips those wrappers.

        Args:
            content: Raw content from API response.

        Returns:
            Cleaned content with markdown wrappers removed.
        """
        content = content.strip()

        # Remove opening code fence
        if content.startswith("```prolog"):
            content = content[9:]
        elif content.startswith("```"):
            content = content[3:]

        # Remove closing code fence
        if content.endswith("```"):
            content = content[:-3]

        return content.strip()

    def list_models(self) -> list:
        """
        List available Ollama models.

        Returns:
            List of model names available on the Ollama server.

        Example:
            >>> provider = OllamaProvider()
            >>> models = provider.list_models()
            >>> print(models)
            ['llama3.1:8b', 'gpt-oss:20b', 'mistral:7b']
        """
        try:
            response = self.client.list()
            return [m['name'] for m in response.get('models', [])]
        except Exception as e:
            print(f"Error listing models: {e}")
            return []

    def check_available(self) -> bool:
        """
        Check if Ollama server is running and accessible.

        Returns:
            True if server is running, False otherwise.

        Example:
            >>> provider = OllamaProvider()
            >>> if provider.check_available():
            ...     result = provider.extract(...)
            ... else:
            ...     print("Start Ollama with: ollama serve")
        """
        try:
            self.client.list()
            return True
        except Exception:
            return False

    def pull_model(self, model_name: str = None) -> bool:
        """
        Pull/download a model to the Ollama server.

        Args:
            model_name: Model to pull (default: self.model)

        Returns:
            True if successful, False otherwise.

        Example:
            >>> provider = OllamaProvider(model="gpt-oss:20b")
            >>> provider.pull_model()  # Downloads gpt-oss:20b
        """
        model_name = model_name or self.model
        try:
            print(f"Pulling model {model_name}...")
            self.client.pull(model_name)
            print(f"Model {model_name} ready.")
            return True
        except Exception as e:
            print(f"Error pulling model: {e}")
            return False
