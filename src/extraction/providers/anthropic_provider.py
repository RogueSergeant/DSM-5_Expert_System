"""
Anthropic Claude API provider for DSM-5 criteria extraction.

This module provides integration with Anthropic's Claude models for extracting
structured Prolog diagnostic criteria from DSM-5 text.

API Documentation:
    - Anthropic Docs: https://docs.anthropic.com/en/docs/
    - Python SDK: https://github.com/anthropics/anthropic-sdk-python
    - Extended Thinking: https://docs.anthropic.com/en/docs/build-with-claude/extended-thinking

Models (as of December 2025):
    - claude-sonnet-4-5: Latest Sonnet model, balanced speed/quality
    - claude-opus-4-5: Most capable model, best for complex tasks
    - claude-haiku-3-5: Fastest model, good for simple extractions

Extended Thinking:
    Claude models support extended thinking mode which allows the model to
    "think" before responding. This improves reasoning quality for complex tasks
    like structured code extraction.

    Budget tokens control how much thinking the model does:
    - 1024-5000: Light thinking for straightforward tasks
    - 5000-15000: Moderate thinking for most extractions
    - 15000-50000: Deep thinking for complex criteria
    - 50000+: Maximum thinking for difficult edge cases

Example Usage:
    >>> from src.extraction.providers import AnthropicProvider
    >>> provider = AnthropicProvider(model="claude-sonnet-4-5", thinking_budget=10000)
    >>> result = provider.extract(dsm5_text, "ptsd", template_guide)
"""

import time
from typing import Optional, Union, Literal

from ..base import ExtractionProvider, ExtractionResult
from ..config import Config


# Type for thinking mode
ThinkingMode = Union[bool, int]


class AnthropicProvider(ExtractionProvider):
    """
    Anthropic Claude extraction provider.

    This provider uses Anthropic's Claude models to extract DSM-5 diagnostic
    criteria into structured Prolog format. It supports extended thinking mode
    to improve extraction quality for complex diagnostic criteria.

    Attributes:
        model: The Claude model to use (default: claude-sonnet-4-5)
        api_key: Anthropic API key (from env var ANTHROPIC_API_KEY if not provided)
        max_tokens: Maximum tokens in the response
        temperature: Sampling temperature (0.0-1.0, lower = more deterministic)
        thinking_budget: Token budget for extended thinking (0 to disable, or 1024+)

    Example:
        >>> provider = AnthropicProvider(
        ...     model="claude-sonnet-4-5",
        ...     thinking_budget=10000,
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
        thinking_budget: int = 10000,
        **kwargs
    ):
        """
        Initialize the Anthropic provider.

        Args:
            model: Model identifier (default: claude-sonnet-4-5). Options include:
                   - claude-sonnet-4-5: Balanced speed and quality (recommended)
                   - claude-opus-4-5: Most capable, best for complex extractions
                   - claude-haiku-3-5: Fastest, for simple tasks
            api_key: Anthropic API key. If not provided, reads from
                     ANTHROPIC_API_KEY environment variable.
            max_tokens: Maximum tokens in response (default: 16384).
                        This is separate from thinking tokens.
            temperature: Sampling temperature (default: 0.1).
                         Lower values = more deterministic output.
                         Note: Must be 1.0 when using extended thinking.
            thinking_budget: Token budget for extended thinking (default: 10000).
                             Set to 0 to disable extended thinking.
                             Minimum value when enabled is 1024.
                             Higher values = more thorough reasoning.
            **kwargs: Additional arguments passed to base class.

        Raises:
            ValueError: If API key not provided and not in environment.
        """
        config = Config.from_env()
        model = model or config.default_anthropic_model
        super().__init__(model, **kwargs)

        self.api_key = api_key or config.anthropic_api_key
        self.max_tokens = max_tokens
        self.temperature = temperature
        self.thinking_budget = thinking_budget
        self._client = None

    @property
    def provider_name(self) -> str:
        """Return the provider identifier."""
        return "anthropic"

    @property
    def client(self):
        """
        Lazy-load the Anthropic client.

        Returns:
            anthropic.Anthropic: Configured Anthropic client instance.

        Raises:
            ImportError: If anthropic package is not installed.
            ValueError: If API key is not configured.
        """
        if self._client is None:
            try:
                from anthropic import Anthropic
            except ImportError:
                raise ImportError(
                    "Anthropic SDK not installed. Install with: pip install anthropic"
                )

            if not self.api_key:
                raise ValueError(
                    "Anthropic API key not provided. Set ANTHROPIC_API_KEY environment "
                    "variable or pass api_key parameter."
                )

            self._client = Anthropic(api_key=self.api_key)
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
        Claude, requesting structured Prolog output. When extended thinking
        is enabled, the model will reason through the extraction before
        generating the final output.

        Args:
            dsm5_text: Raw DSM-5 diagnostic criteria text to extract from.
            disorder_id: Short identifier for the disorder (e.g., 'ptsd', 'asd').
                         Used in the generated Prolog predicates.
            template_guide: Content of gold_standard/README.md explaining
                            the expected Prolog structure.
            schema_reference: Optional content of schema.pl for additional
                              context about predicate signatures.

        Returns:
            ExtractionResult containing:
                - success: Whether extraction completed without errors
                - content: The extracted Prolog code (if successful)
                - model: Model used for extraction
                - provider: "anthropic"
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

            # Build API request parameters
            # See: https://docs.anthropic.com/en/docs/build-with-claude/extended-thinking
            request_params = {
                "model": self.model,
                "max_tokens": self.max_tokens,
                "system": system_prompt,
                "messages": [
                    {"role": "user", "content": prompt},
                ],
            }

            # Add extended thinking if enabled
            # Note: temperature must be 1.0 when using extended thinking
            if self.thinking_budget > 0:
                request_params["thinking"] = {
                    "type": "enabled",
                    "budget_tokens": max(1024, self.thinking_budget),
                }
                # Extended thinking requires temperature=1.0
                request_params["temperature"] = 1.0
            else:
                request_params["temperature"] = self.temperature

            # Make API request
            message = self.client.messages.create(**request_params)

            # Extract text content from response
            # When thinking is enabled, response contains both thinking and text blocks
            content = ""
            thinking_content = ""
            for block in message.content:
                if hasattr(block, 'text'):
                    content += block.text
                elif hasattr(block, 'thinking'):
                    thinking_content = block.thinking

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
                input_tokens=message.usage.input_tokens if message.usage else None,
                output_tokens=message.usage.output_tokens if message.usage else None,
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
