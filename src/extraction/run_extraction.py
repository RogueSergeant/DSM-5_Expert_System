#!/usr/bin/env python3
"""
Main CLI script for running LLM extractions.

This script provides a command-line interface for extracting DSM-5 diagnostic
criteria from text into structured Prolog files using various LLM providers.

Usage Examples:
    # Using Ollama with thinking
    python -m src.extraction.run_extraction --disorder ptsd --provider ollama --model gpt-oss:20b --think high

    # Using OpenAI GPT-5 with reasoning
    python -m src.extraction.run_extraction --disorder ptsd --provider openai --reasoning-effort high

    # Using Anthropic Claude with extended thinking
    python -m src.extraction.run_extraction --disorder ptsd --provider anthropic --thinking-budget 10000

    # Test all providers and save to outputs/extractions/
    python -m src.extraction.run_extraction --disorder ptsd --all --save

    # Save to production location (src/prolog/extracted/{disorder}.pl)
    python -m src.extraction.run_extraction --disorder ptsd --provider anthropic --thinking-budget 15000 --production

    # List available Ollama models
    python -m src.extraction.run_extraction --list-models

Output Locations:
    --save:       outputs/extractions/{disorder}_{provider}_{model}_{timestamp}.pl
    --production: src/prolog/extracted/{disorder}.pl
"""

import argparse
import json
from datetime import datetime
from pathlib import Path

from .base import get_dsm5_text, get_template_guide, get_schema_reference, get_project_root
from .config import Config, create_env_template
from .providers import get_provider, OpenAIProvider, AnthropicProvider, OllamaProvider
from .evaluate import evaluate_extraction, print_validation_report


def parse_args():
    """
    Parse command-line arguments.

    Returns:
        argparse.Namespace: Parsed arguments.
    """
    parser = argparse.ArgumentParser(
        description="Extract DSM-5 criteria to Prolog using LLMs",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    # Ollama with thinking
    %(prog)s --disorder ptsd --provider ollama --model gpt-oss:20b --think high

    # OpenAI GPT-5 with reasoning
    %(prog)s --disorder ptsd --provider openai --reasoning-effort high

    # Anthropic Claude with extended thinking
    %(prog)s --disorder ptsd --provider anthropic --thinking-budget 10000
        """
    )

    # Required arguments
    parser.add_argument(
        "--disorder",
        required=True,
        help="Disorder ID to extract (e.g., ptsd, asd, mdd)",
    )

    # Provider selection
    provider_group = parser.add_argument_group("Provider Selection")
    provider_group.add_argument(
        "--provider",
        choices=["openai", "anthropic", "ollama"],
        help="LLM provider to use",
    )
    provider_group.add_argument(
        "--model",
        help="Specific model to use (overrides default)",
    )
    provider_group.add_argument(
        "--all",
        action="store_true",
        help="Test all available providers",
    )

    # Thinking parameters
    thinking_group = parser.add_argument_group("Thinking Parameters")
    thinking_group.add_argument(
        "--reasoning-effort",
        choices=["none", "minimal", "low", "medium", "high", "xhigh"],
        default="medium",
        help="OpenAI GPT-5 reasoning effort level (default: medium)",
    )
    thinking_group.add_argument(
        "--thinking-budget",
        type=int,
        default=10000,
        help="Anthropic extended thinking budget in tokens (default: 10000, 0 to disable)",
    )
    thinking_group.add_argument(
        "--think",
        choices=["low", "medium", "high"],
        help="Ollama thinking level for supported models (gpt-oss, etc.)",
    )

    # Output options
    output_group = parser.add_argument_group("Output Options")
    output_group.add_argument(
        "--save",
        action="store_true",
        help="Save extracted .pl file to outputs/extractions/ (with timestamp)",
    )
    output_group.add_argument(
        "--production",
        action="store_true",
        help="Save to src/prolog/extracted/{disorder}.pl for production use",
    )
    output_group.add_argument(
        "--no-schema",
        action="store_true",
        help="Don't include schema.pl as reference",
    )

    # Utility commands
    utility_group = parser.add_argument_group("Utilities")
    utility_group.add_argument(
        "--init-env",
        action="store_true",
        help="Create .env.example template file",
    )
    utility_group.add_argument(
        "--list-models",
        action="store_true",
        help="List available Ollama models",
    )

    return parser.parse_args()


def run_extraction(
    disorder_id: str,
    provider_name: str,
    model: str = None,
    include_schema: bool = True,
    save: bool = False,
    production: bool = False,
    reasoning_effort: str = "medium",
    thinking_budget: int = 10000,
    think: str = None,
):
    """
    Run extraction with a specific provider.

    Args:
        disorder_id: The disorder identifier (e.g., 'ptsd', 'asd')
        provider_name: Provider name ('openai', 'anthropic', 'ollama')
        model: Specific model to use (optional)
        include_schema: Whether to include schema.pl as reference
        save: Whether to save results to outputs/extractions/ (timestamped)
        production: Whether to save to src/prolog/extracted/ (clean name)
        reasoning_effort: OpenAI GPT-5 reasoning effort level
        thinking_budget: Anthropic extended thinking budget
        think: Ollama thinking level

    Returns:
        ExtractionResult or None if extraction failed.
    """
    print(f"\n{'='*60}")
    print(f"Extracting: {disorder_id.upper()}")
    print(f"Provider: {provider_name}")
    print(f"{'='*60}")

    # Load config
    config = Config.from_env()

    # Check provider config
    missing = config.get_missing_config(provider_name)
    if missing:
        print(f"ERROR: {missing}")
        return None

    # Load inputs
    try:
        dsm5_text = get_dsm5_text(disorder_id)
        template_guide = get_template_guide()
        schema_ref = get_schema_reference() if include_schema else None
    except FileNotFoundError as e:
        print(f"ERROR: {e}")
        return None

    # Create provider with appropriate thinking parameter
    provider_kwargs = {"model": model} if model else {}

    if provider_name == "openai":
        provider_kwargs["reasoning_effort"] = reasoning_effort
        print(f"Reasoning Effort: {reasoning_effort}")
    elif provider_name == "anthropic":
        provider_kwargs["thinking_budget"] = thinking_budget
        if thinking_budget > 0:
            print(f"Thinking Budget: {thinking_budget} tokens")
        else:
            print("Extended Thinking: Disabled")
    elif provider_name == "ollama":
        if think:
            provider_kwargs["think"] = think
            print(f"Think Level: {think}")

    provider = get_provider(provider_name, **provider_kwargs)
    print(f"Model: {provider.model}")
    print(f"Starting extraction...")

    # Run extraction
    result = provider.extract(
        dsm5_text=dsm5_text,
        disorder_id=disorder_id,
        template_guide=template_guide,
        schema_reference=schema_ref,
    )

    # Evaluate result
    result = evaluate_extraction(result)

    # Print report
    print_validation_report(result)

    # Save if requested
    if save and result.success:
        save_result(result)

    # Save to production location if requested
    if production and result.success and result.syntax_valid:
        save_production_result(result)
    elif production and result.success and not result.syntax_valid:
        print("WARNING: Skipping production save - syntax validation failed")

    return result


def save_result(result):
    """
    Save extraction result to outputs directory.

    Args:
        result: ExtractionResult to save
    """
    root = get_project_root()
    output_dir = root / 'outputs' / 'extractions'
    output_dir.mkdir(parents=True, exist_ok=True)

    # Create filename with timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"{result.disorder_id}_{result.provider}_{result.model.replace(':', '-')}_{timestamp}"

    # Save .pl file
    pl_path = output_dir / f"{filename}.pl"
    pl_path.write_text(result.content)
    print(f"Saved: {pl_path}")

    # Save metadata
    meta_path = output_dir / f"{filename}.json"
    metadata = {
        'disorder_id': result.disorder_id,
        'provider': result.provider,
        'model': result.model,
        'success': result.success,
        'duration_seconds': result.duration_seconds,
        'input_tokens': result.input_tokens,
        'output_tokens': result.output_tokens,
        'syntax_valid': result.syntax_valid,
        'validation_issues': result.validation_issues,
        'timestamp': timestamp,
    }
    meta_path.write_text(json.dumps(metadata, indent=2))
    print(f"Saved: {meta_path}")


def save_production_result(result):
    """
    Save extraction result to production location.

    Saves to src/prolog/extracted/{disorder_id}.pl with clean naming
    for use in the diagnostic system.

    Args:
        result: ExtractionResult to save (must have syntax_valid=True)
    """
    root = get_project_root()
    output_dir = root / 'src' / 'prolog' / 'extracted'
    output_dir.mkdir(parents=True, exist_ok=True)

    # Clean filename: just {disorder_id}.pl
    pl_path = output_dir / f"{result.disorder_id}.pl"

    # Save .pl file
    pl_path.write_text(result.content)
    print(f"Production saved: {pl_path}")

    # Save metadata alongside
    meta_path = output_dir / f"{result.disorder_id}.json"
    metadata = {
        'disorder_id': result.disorder_id,
        'provider': result.provider,
        'model': result.model,
        'extracted_at': datetime.now().isoformat(),
        'duration_seconds': result.duration_seconds,
        'input_tokens': result.input_tokens,
        'output_tokens': result.output_tokens,
        'syntax_valid': result.syntax_valid,
        'validation_issues': result.validation_issues,
    }
    meta_path.write_text(json.dumps(metadata, indent=2))
    print(f"Production metadata: {meta_path}")


def main():
    """Main entry point for the CLI."""
    args = parse_args()

    # Handle special commands
    if args.init_env:
        create_env_template()
        return

    if args.list_models:
        provider = OllamaProvider()
        if provider.check_available():
            models = provider.list_models()
            print("Available Ollama models:")
            for m in models:
                print(f"  - {m}")
        else:
            print("Ollama server not running. Start with: ollama serve")
        return

    # Run extraction(s)
    if args.all:
        # Test all providers
        providers = ["ollama", "openai", "anthropic"]
        results = []
        for p in providers:
            config = Config.from_env()
            if config.validate(p):
                result = run_extraction(
                    disorder_id=args.disorder,
                    provider_name=p,
                    model=args.model,
                    include_schema=not args.no_schema,
                    save=args.save,
                    production=args.production,
                    reasoning_effort=args.reasoning_effort,
                    thinking_budget=args.thinking_budget,
                    think=args.think,
                )
                if result:
                    results.append(result)
            else:
                print(f"\nSkipping {p}: {config.get_missing_config(p)}")

        # Summary
        print("\n" + "="*60)
        print("SUMMARY")
        print("="*60)
        for r in results:
            status = "PASS" if r.success and r.syntax_valid else "FAIL"
            print(f"  [{status}] {r.provider}/{r.model}: {r.duration_seconds:.1f}s")

    elif args.provider:
        run_extraction(
            disorder_id=args.disorder,
            provider_name=args.provider,
            model=args.model,
            include_schema=not args.no_schema,
            save=args.save,
            production=args.production,
            reasoning_effort=args.reasoning_effort,
            thinking_budget=args.thinking_budget,
            think=args.think,
        )
    else:
        print("ERROR: Specify --provider or --all")


if __name__ == "__main__":
    main()
