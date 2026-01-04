"""
OpenAI API provider for DSM-5 criteria extraction.

This module provides integration with OpenAI's GPT-5 series models for extracting
structured Prolog diagnostic criteria from DSM-5 text.

API Documentation:
    - OpenAI Platform: https://platform.openai.com/docs/api-reference/chat
    - GPT-5 Guide: https://platform.openai.com/docs/guides/latest-model
    - Python SDK: https://github.com/openai/openai-python

Models (as of December 2025):
    - gpt-5.2: Latest flagship model with enhanced reasoning
    - gpt-5.1: Previous generation, still highly capable
    - gpt-5: Original GPT-5 release

Reasoning Effort Levels (GPT-5.x):
    - none: No reasoning, fastest response (GPT-5.1+)
    - minimal: Minimal reasoning for simple tasks
    - low: Light reasoning, good balance of speed/quality
    - medium: Moderate reasoning (default)
    - high: Deep reasoning for complex tasks
    - xhigh: Maximum reasoning depth (GPT-5.2 only)

Example Usage:
    >>> from src.extraction.providers import OpenAIProvider
    >>> provider = OpenAIProvider(model="gpt-5.2", reasoning_effort="high")
    >>> result = provider.extract(dsm5_text, "ptsd", template_guide)
"""

import time
from typing import Optional, Literal

from ..base import ExtractionProvider, ExtractionResult
from ..config import Config


# Type alias for reasoning effort levels
ReasoningEffort = Literal["none", "minimal", "low", "medium", "high", "xhigh"]


class OpenAIProvider(ExtractionProvider):
    """
    OpenAI GPT-5 extraction provider.

    This provider uses OpenAI's GPT-5 series models to extract DSM-5 diagnostic
    criteria into structured Prolog format. It supports the reasoning_effort
    parameter to control how much "thinking" the model does before responding.

    Attributes:
        model: The OpenAI model to use (default: gpt-5.2)
        api_key: OpenAI API key (from env var OPENAI_API_KEY if not provided)
        max_tokens: Maximum tokens in the response
        temperature: Sampling temperature (0.0-2.0, lower = more deterministic)
        reasoning_effort: How much reasoning to apply (none to xhigh)

    Example:
        >>> provider = OpenAIProvider(
        ...     model="gpt-5.2",
        ...     reasoning_effort="high",
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
        api_key: str = None,
        max_tokens: int = 16384,
        temperature: float = 0.1,
        reasoning_effort: ReasoningEffort = "medium",
        **kwargs
    ):
        """
        Initialize the OpenAI provider.

        Args:
            model: Model identifier (default: gpt-5.2). Options include:
                   - gpt-5.2: Latest with xhigh reasoning support
                   - gpt-5.1: Stable with none reasoning support
                   - gpt-5: Original GPT-5
            api_key: OpenAI API key. If not provided, reads from
                     OPENAI_API_KEY environment variable.
            max_tokens: Maximum tokens in response (default: 16384).
                        GPT-5.2 supports up to 32768.
            temperature: Sampling temperature (default: 0.1).
                         Lower values = more deterministic output.
            reasoning_effort: Reasoning depth level (default: "medium").
                              Options: none, minimal, low, medium, high, xhigh.
                              Note: "xhigh" only available for GPT-5.2.
            **kwargs: Additional arguments passed to base class.

        Raises:
            ValueError: If API key not provided and not in environment.
        """
        # Load configuration from environment
        config = Config.from_env()
        model = model or config.default_openai_model

        super().__init__(model, **kwargs)

        self.api_key = api_key or config.openai_api_key
        self.max_tokens = max_tokens
        self.temperature = temperature
        self.reasoning_effort = reasoning_effort
        self._client = None

    @property
    def provider_name(self) -> str:
        """Return the provider identifier."""
        return "openai"

    @property
    def client(self):
        """
        Lazy-load the OpenAI client.

        Returns:
            openai.OpenAI: Configured OpenAI client instance.

        Raises:
            ImportError: If openai package is not installed.
            ValueError: If API key is not configured.
        """
        if self._client is None:
            try:
                from openai import OpenAI
            except ImportError:
                raise ImportError(
                    "OpenAI SDK not installed. Install with: pip install openai"
                )

            if not self.api_key:
                raise ValueError(
                    "OpenAI API key not provided. Set OPENAI_API_KEY environment "
                    "variable or pass api_key parameter."
                )

            self._client = OpenAI(api_key=self.api_key)
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
        GPT-5, requesting structured Prolog output. The reasoning_effort
        parameter controls how deeply the model analyses the criteria.

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
                - provider: "openai"
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

            # Make API request with reasoning effort parameter
            # See: https://platform.openai.com/docs/guides/reasoning
            # Note: GPT-5 uses max_completion_tokens instead of max_tokens
            # Note: GPT-5 with reasoning only supports temperature=1.0
            request_params = {
                "model": self.model,
                "messages": [
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": prompt},
                ],
                "max_completion_tokens": self.max_tokens,
                "reasoning_effort": self.reasoning_effort,
            }

            # Only add temperature if not using reasoning (reasoning requires temp=1.0)
            if self.reasoning_effort == "none":
                request_params["temperature"] = self.temperature

            response = self.client.chat.completions.create(**request_params)

            # Extract content from response
            content = response.choices[0].message.content
            duration = time.time() - start_time

            # Clean up any markdown formatting the model might add
            content = self._clean_content(content)

            return ExtractionResult(
                success=True,
                content=content,
                model=self.model,
                provider=self.provider_name,
                disorder_id=disorder_id,
                duration_seconds=duration,
                input_tokens=response.usage.prompt_tokens if response.usage else None,
                output_tokens=response.usage.completion_tokens if response.usage else None,
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
