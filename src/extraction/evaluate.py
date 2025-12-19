"""
Evaluation module for validating extracted .pl files.

Uses SWI-Prolog to:
1. Check syntax validity
2. Run validate_disorder/2 predicate
3. Compare against gold standards
"""

import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple

from .base import ExtractionResult, get_project_root


@dataclass
class ValidationResult:
    """Result of validating a .pl file."""
    syntax_valid: bool
    loads_successfully: bool
    disorder_defined: bool
    symptom_count: int
    exclusion_count: int
    validation_issues: List[str]
    error_message: Optional[str] = None


def validate_syntax(pl_content: str) -> Tuple[bool, Optional[str]]:
    """
    Check if Prolog content has valid syntax.

    Returns:
        (is_valid, error_message)
    """
    with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as f:
        f.write(pl_content)
        temp_path = f.name

    try:
        # Try to load the file with SWI-Prolog
        result = subprocess.run(
            ['swipl', '-g', f"['{temp_path}'], halt(0)", '-t', 'halt(1)'],
            capture_output=True,
            text=True,
            timeout=30,
        )

        if result.returncode == 0:
            return True, None
        else:
            error = result.stderr.strip() or result.stdout.strip()
            return False, error

    except subprocess.TimeoutExpired:
        return False, "Timeout: Prolog took too long to load file"
    except FileNotFoundError:
        return False, "SWI-Prolog (swipl) not found. Please install it."
    except Exception as e:
        return False, str(e)
    finally:
        Path(temp_path).unlink(missing_ok=True)


def validate_with_schema(pl_content: str, disorder_id: str) -> ValidationResult:
    """
    Validate extracted .pl content against the schema.

    Loads the schema, then the extracted content, and runs validation predicates.
    """
    root = get_project_root()
    schema_path = root / 'src' / 'prolog' / 'schema.pl'

    with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as f:
        f.write(pl_content)
        temp_path = f.name

    try:
        # Build Prolog query to validate
        query = f"""
            ['{schema_path}'],
            ['{temp_path}'],

            % Check if disorder is defined
            (disorder({disorder_id}, Name, _)
             -> format('DISORDER_DEFINED:~w~n', [Name])
             ;  writeln('DISORDER_DEFINED:false')),

            % Count symptoms
            findall(S, symptom({disorder_id}, S, _, _), Symptoms),
            length(Symptoms, SympCount),
            format('SYMPTOM_COUNT:~w~n', [SympCount]),

            % Count exclusions
            findall(E, exclusion_criterion({disorder_id}, E, _, _), Exclusions),
            length(Exclusions, ExclCount),
            format('EXCLUSION_COUNT:~w~n', [ExclCount]),

            % Run validate_disorder if available
            (validate_disorder({disorder_id}, Issues)
             -> (Issues = []
                 -> writeln('VALIDATION:pass')
                 ;  format('VALIDATION:~w~n', [Issues]))
             ;  writeln('VALIDATION:skipped')),

            halt(0)
        """

        result = subprocess.run(
            ['swipl', '-g', query, '-t', 'halt(1)'],
            capture_output=True,
            text=True,
            timeout=30,
        )

        output = result.stdout + result.stderr

        # Parse output
        disorder_defined = 'DISORDER_DEFINED:false' not in output
        symptom_count = _parse_count(output, 'SYMPTOM_COUNT:')
        exclusion_count = _parse_count(output, 'EXCLUSION_COUNT:')

        validation_issues = []
        if 'VALIDATION:pass' not in output and 'VALIDATION:skipped' not in output:
            # Extract validation issues
            for line in output.split('\n'):
                if line.startswith('VALIDATION:'):
                    issues_str = line.replace('VALIDATION:', '')
                    if issues_str and issues_str != 'pass':
                        validation_issues.append(issues_str)

        return ValidationResult(
            syntax_valid=result.returncode == 0,
            loads_successfully=result.returncode == 0,
            disorder_defined=disorder_defined,
            symptom_count=symptom_count,
            exclusion_count=exclusion_count,
            validation_issues=validation_issues,
            error_message=result.stderr if result.returncode != 0 else None,
        )

    except subprocess.TimeoutExpired:
        return ValidationResult(
            syntax_valid=False,
            loads_successfully=False,
            disorder_defined=False,
            symptom_count=0,
            exclusion_count=0,
            validation_issues=[],
            error_message="Timeout during validation",
        )
    except Exception as e:
        return ValidationResult(
            syntax_valid=False,
            loads_successfully=False,
            disorder_defined=False,
            symptom_count=0,
            exclusion_count=0,
            validation_issues=[],
            error_message=str(e),
        )
    finally:
        Path(temp_path).unlink(missing_ok=True)


def _parse_count(output: str, prefix: str) -> int:
    """Parse a count value from output."""
    for line in output.split('\n'):
        if line.startswith(prefix):
            try:
                return int(line.replace(prefix, '').strip())
            except ValueError:
                return 0
    return 0


def compare_with_gold_standard(
    extracted_content: str,
    disorder_id: str,
) -> dict:
    """
    Compare extracted content with gold standard if available.

    Returns comparison metrics.
    """
    root = get_project_root()
    gold_path = root / 'src' / 'prolog' / 'gold_standard' / f'{disorder_id}.pl'

    if not gold_path.exists():
        return {'gold_standard_available': False}

    # Validate both files
    extracted_result = validate_with_schema(extracted_content, disorder_id)
    gold_content = gold_path.read_text()
    gold_result = validate_with_schema(gold_content, disorder_id)

    return {
        'gold_standard_available': True,
        'extracted_symptoms': extracted_result.symptom_count,
        'gold_symptoms': gold_result.symptom_count,
        'symptom_match': extracted_result.symptom_count == gold_result.symptom_count,
        'extracted_exclusions': extracted_result.exclusion_count,
        'gold_exclusions': gold_result.exclusion_count,
        'exclusion_match': extracted_result.exclusion_count == gold_result.exclusion_count,
    }


def evaluate_extraction(result: ExtractionResult) -> ExtractionResult:
    """
    Evaluate an extraction result and populate validation fields.

    Modifies the result in-place and returns it.
    """
    if not result.success or not result.content:
        result.syntax_valid = False
        result.validation_issues = ["Extraction failed"]
        return result

    # Check syntax
    syntax_valid, error = validate_syntax(result.content)
    result.syntax_valid = syntax_valid

    if not syntax_valid:
        result.validation_issues = [f"Syntax error: {error}"]
        return result

    # Validate with schema
    validation = validate_with_schema(result.content, result.disorder_id)
    result.validation_issues = validation.validation_issues

    if not validation.disorder_defined:
        result.validation_issues.append(f"Disorder '{result.disorder_id}' not defined")

    if validation.symptom_count == 0:
        result.validation_issues.append("No symptoms defined")

    return result


def print_validation_report(result: ExtractionResult) -> None:
    """Print a formatted validation report."""
    print(f"\n{'='*60}")
    print(f"Extraction Result: {result.disorder_id}")
    print(f"{'='*60}")
    print(f"Provider: {result.provider}")
    print(f"Model: {result.model}")
    print(f"Duration: {result.duration_seconds:.2f}s")

    if result.input_tokens:
        print(f"Tokens: {result.input_tokens} in / {result.output_tokens} out")

    print(f"\nStatus: {'SUCCESS' if result.success else 'FAILED'}")

    if result.error:
        print(f"Error: {result.error}")

    print(f"Syntax Valid: {result.syntax_valid}")

    if result.validation_issues:
        print(f"\nValidation Issues:")
        for issue in result.validation_issues:
            print(f"  - {issue}")
    else:
        print("\nNo validation issues found.")

    print(f"{'='*60}\n")
