"""
Base classes for LLM extraction providers.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional
import time


@dataclass
class ExtractionResult:
    """Result of an extraction attempt."""
    success: bool
    content: str  # The extracted .pl file content
    model: str
    provider: str
    disorder_id: str

    # Timing and usage
    duration_seconds: float = 0.0
    input_tokens: Optional[int] = None
    output_tokens: Optional[int] = None

    # Error info if failed
    error: Optional[str] = None

    # Validation results (populated by evaluate.py)
    syntax_valid: Optional[bool] = None
    validation_issues: list = field(default_factory=list)

    def __str__(self) -> str:
        status = "SUCCESS" if self.success else "FAILED"
        return f"[{status}] {self.provider}/{self.model} -> {self.disorder_id}.pl ({self.duration_seconds:.1f}s)"


class ExtractionProvider(ABC):
    """Abstract base class for LLM extraction providers."""

    def __init__(self, model: str, **kwargs):
        self.model = model
        self.kwargs = kwargs

    @property
    @abstractmethod
    def provider_name(self) -> str:
        """Return the provider name (e.g., 'openai', 'anthropic', 'ollama')."""
        pass

    @abstractmethod
    def extract(
        self,
        dsm5_text: str,
        disorder_id: str,
        template_guide: str,
        schema_reference: Optional[str] = None,
    ) -> ExtractionResult:
        """
        Extract Prolog criteria from DSM-5 text.

        Args:
            dsm5_text: Raw DSM-5 diagnostic criteria text
            disorder_id: Short identifier (e.g., 'ptsd', 'asd')
            template_guide: Content of gold_standard/README.md
            schema_reference: Optional content of schema.pl

        Returns:
            ExtractionResult with the extracted .pl content
        """
        pass

    def build_prompt(
        self,
        dsm5_text: str,
        disorder_id: str,
        template_guide: str,
        schema_reference: Optional[str] = None,
    ) -> str:
        """Build the extraction prompt."""
        prompt = f"""You are an expert in both clinical psychology (DSM-5 diagnostic criteria) and Prolog programming.

Your task is to extract the diagnostic criteria from the DSM-5 text provided and convert it into a structured Prolog file following the template guide.

## Template Guide (how to structure the .pl file):

{template_guide}

## DSM-5 Text to Extract From:

{dsm5_text}

## Instructions:

1. Create a complete Prolog file for the disorder with ID: `{disorder_id}`
2. Follow the template guide exactly for predicate structure
3. Include ALL multifile declarations at the top
4. Extract ALL symptoms, grouping them by category
5. Include ALL exclusion criteria
6. Include duration and onset requirements
7. Add subjective criteria for clinical significance
8. Add relevant specifiers and differential features

Output ONLY the Prolog code, no explanations. Start with the file header comment."""

        if schema_reference:
            prompt += f"""

## Schema Reference (predicate signatures):

{schema_reference}"""

        return prompt

    def build_system_prompt(self) -> str:
        """Build the system prompt."""
        return """You are a precise code generator that converts clinical diagnostic criteria into structured Prolog code.
You output only valid Prolog code with no markdown formatting, no explanations, and no additional text.
Your output should be a complete, syntactically correct Prolog file."""


def load_file(path: Path) -> str:
    """Load file content as string."""
    with open(path, 'r', encoding='utf-8') as f:
        return f.read()


def get_project_root() -> Path:
    """Get the project root directory."""
    # Navigate up from src/extraction to project root
    return Path(__file__).parent.parent.parent


def get_dsm5_text(disorder_id: str) -> str:
    """Load DSM-5 text for a disorder."""
    root = get_project_root()
    path = root / 'data' / 'dsm5_text' / f'{disorder_id.upper()}.txt'
    if not path.exists():
        raise FileNotFoundError(f"DSM-5 text not found: {path}")
    return load_file(path)


def get_template_guide() -> str:
    """Load the gold standard template guide."""
    root = get_project_root()
    path = root / 'src' / 'prolog' / 'gold_standard' / 'README.md'
    return load_file(path)


def get_schema_reference() -> str:
    """Load the schema.pl file for reference."""
    root = get_project_root()
    path = root / 'src' / 'prolog' / 'schema.pl'
    return load_file(path)
