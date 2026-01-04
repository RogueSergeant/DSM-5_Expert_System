"""
Batch Clinical Analyser - Extends ClinicalAnalyser for batch question answering.

This module implements batch processing of clinical vignette questions, allowing
multiple questions to be answered in a single LLM call for efficiency.

Key Features:
- Adaptive batching with A* search support (small/medium batches)
- Non-adaptive all-at-once processing
- Robust JSON parsing with fallback to line-based format
- Error handling with sequential fallback

Author: Alfie Roberts
Date: January 2026
"""

import json
import re
from typing import List, Dict, Optional, Tuple
from src.evaluation.benchmark import ClinicalAnalyser


class ParseError(Exception):
    """Raised when batch response parsing fails."""
    pass


class BatchClinicalAnalyser(ClinicalAnalyser):
    """
    Extends ClinicalAnalyser to support batch question answering.

    Allows answering multiple clinical assessment questions in a single
    LLM call, reducing total API requests while maintaining accuracy.
    """

    def __init__(self, vignette: Dict, provider_name: str = "openai"):
        """
        Initialise batch analyser.

        Args:
            vignette: Clinical vignette dictionary with 'clinical_text' key
            provider_name: LLM provider ("ollama", "openai", "anthropic")
        """
        super().__init__(vignette, provider_name)
        self.provider_name = provider_name

    def answer_batch(
        self,
        questions: List[str],
        batch_size: Optional[int] = None
    ) -> Dict[str, str]:
        """
        Answer multiple questions in batches.

        Args:
            questions: List of criterion questions to answer
            batch_size: If None, process all at once. Otherwise, chunk into batches.

        Returns:
            Dict mapping question_text -> answer ("YES"/"NO"/"UNKNOWN")

        Raises:
            ParseError: If response parsing fails after all attempts
        """
        if not questions:
            return {}

        if batch_size is None:
            # All-at-once mode
            return self._answer_single_batch(questions)
        else:
            # Chunked batch mode for adaptive search
            results = {}
            for i in range(0, len(questions), batch_size):
                batch = questions[i:i+batch_size]
                try:
                    batch_results = self._answer_single_batch(batch)
                    results.update(batch_results)
                except ParseError as e:
                    print(f"  [Warning] Batch parse failed: {e}")
                    print(f"  [Fallback] Processing batch sequentially...")
                    # Fallback to sequential for this batch only
                    fallback_results = self._fallback_to_sequential(batch)
                    results.update(fallback_results)

            return results

    def _answer_single_batch(self, questions: List[str]) -> Dict[str, str]:
        """
        Process a single batch of questions via LLM.

        Args:
            questions: List of questions for this batch

        Returns:
            Dict mapping question_text -> answer

        Raises:
            ParseError: If response cannot be parsed
        """
        # Build batch prompt
        prompt = self._build_batch_prompt(questions)
        system_prompt = self._build_batch_system_prompt()

        try:
            # Call LLM provider
            result = self.provider.extract(
                dsm5_text=prompt,
                disorder_id="batch_assessment",
                template_guide="",
                custom_prompt=prompt,
                custom_system_prompt=system_prompt
            )

            if not result.success:
                raise ParseError(f"LLM call failed: {result.error}")

            # Parse response
            return self._parse_batch_response(result.content, questions)

        except Exception as e:
            raise ParseError(f"Batch processing failed: {str(e)}")

    def _build_batch_prompt(self, questions: List[str]) -> str:
        """
        Build prompt for batch question answering.

        Args:
            questions: List of questions to include

        Returns:
            Formatted prompt string with numbered questions and JSON output requirement
        """
        questions_list = "\n".join([
            f"{i+1}. {q}"
            for i, q in enumerate(questions)
        ])

        prompt = f"""You are an expert clinical psychiatrist performing a structured assessment.
Analyze the following patient vignette:

"{self.clinical_text}"

TASK: Answer ALL {len(questions)} questions below based ONLY on the patient vignette above.

QUESTIONS:
{questions_list}

INSTRUCTIONS:
- For each question, answer YES, NO, or UNKNOWN
- YES: Clear evidence in vignette supports this criterion
- NO: Vignette explicitly contradicts or denies this criterion
- UNKNOWN: No information available to confirm or deny
- For "Is it true that..." questions:
  - These define required conditions that must be TRUE
  - Answer YES if condition is satisfied (or not mentioned if it's an exclusion)
  - Answer NO if condition is violated
  - Answer UNKNOWN only if explicitly ambiguous

OUTPUT FORMAT (JSON ONLY):
{{
  "1": "YES",
  "2": "NO",
  "3": "UNKNOWN",
  ...
  "{len(questions)}": "YES"
}}

Return ONLY the JSON object above with answers for ALL {len(questions)} questions. No explanations or additional text."""

        return prompt

    def _build_batch_system_prompt(self) -> str:
        """
        Build system prompt optimised for batch processing.

        Returns:
            Provider-specific system prompt
        """
        if self.provider_name == "ollama":
            # Ollama needs more explicit formatting instructions
            return """You are a clinical assessment system.
Output ONLY valid JSON with NO additional text, markdown, or explanations.
Format: {"1": "YES", "2": "NO", "3": "UNKNOWN", ...}
Each key is a question number (string), each value is YES/NO/UNKNOWN (string)."""

        elif self.provider_name == "openai":
            # OpenAI GPT-5 is more reliable with structured output
            return """You are a clinical expert providing structured assessments.
Output valid JSON mapping question numbers to YES/NO/UNKNOWN answers."""

        else:  # anthropic or other
            return """You are a clinical expert. Output ONLY valid JSON with question numbers as keys and YES/NO/UNKNOWN as values."""

    def _parse_batch_response(
        self,
        content: str,
        questions: List[str]
    ) -> Dict[str, str]:
        """
        Parse LLM response into question->answer mapping.

        Args:
            content: Raw LLM response
            questions: Original questions list

        Returns:
            Dict mapping question_text -> "YES"/"NO"/"UNKNOWN"

        Raises:
            ParseError: If response is malformed or incomplete
        """
        # Attempt JSON parsing
        try:
            return self._parse_json_format(content, questions)
        except (json.JSONDecodeError, KeyError, ValueError) as e:
            # Try line-by-line fallback
            try:
                return self._parse_line_format(content, questions)
            except Exception as e2:
                raise ParseError(
                    f"JSON parsing failed: {e}. Line parsing also failed: {e2}"
                )

    def _parse_json_format(self, content: str, questions: List[str]) -> Dict[str, str]:
        """
        Parse JSON-formatted response.

        Args:
            content: Response content
            questions: Questions list

        Returns:
            Parsed answers

        Raises:
            ValueError: If JSON is malformed or incomplete
        """
        # Clean markdown code blocks if present
        content_clean = content.strip()
        if content_clean.startswith("```json"):
            content_clean = content_clean[7:]
        elif content_clean.startswith("```"):
            content_clean = content_clean[3:]

        if content_clean.endswith("```"):
            content_clean = content_clean[:-3]

        content_clean = content_clean.strip()

        # Parse JSON
        data = json.loads(content_clean)

        # Validate: must have len(questions) entries
        if len(data) != len(questions):
            raise ValueError(
                f"Expected {len(questions)} answers, got {len(data)}"
            )

        # Build mapping: question_text -> answer
        result = {}
        for i, question in enumerate(questions):
            key = str(i + 1)
            if key not in data:
                raise ValueError(f"Missing answer for question {key}")

            answer = data[key].strip().upper()
            if answer not in ["YES", "NO", "UNKNOWN"]:
                raise ValueError(f"Invalid answer for Q{key}: {answer}")

            result[question] = answer

        return result

    def _parse_line_format(self, content: str, questions: List[str]) -> Dict[str, str]:
        """
        Fallback parser for line-based format.

        Parses responses like:
        1: YES
        2: NO
        3: UNKNOWN

        Args:
            content: Response content
            questions: Questions list

        Returns:
            Parsed answers

        Raises:
            ValueError: If parsing fails
        """
        result = {}
        pattern = r'^(\d+):\s*(YES|NO|UNKNOWN)\s*$'

        for line in content.split('\n'):
            line = line.strip()
            if not line:
                continue

            match = re.match(pattern, line, re.IGNORECASE)
            if match:
                q_num = int(match.group(1))
                answer = match.group(2).upper()

                if 1 <= q_num <= len(questions):
                    question = questions[q_num - 1]
                    result[question] = answer

        # Validate completeness
        if len(result) != len(questions):
            raise ValueError(
                f"Line parsing found {len(result)}/{len(questions)} answers"
            )

        return result

    def _fallback_to_sequential(self, questions: List[str]) -> Dict[str, str]:
        """
        Fallback to sequential processing if batch parsing fails.

        Args:
            questions: Questions to process

        Returns:
            Dict mapping question -> answer
        """
        results = {}
        for q in questions:
            # Use parent class's answer() method (1 question at a time)
            answer = super().answer(q)
            results[q] = answer

        return results

    def answer_batch_with_confidence(
        self,
        questions: List[str],
        batch_size: Optional[int] = None
    ) -> Dict[str, Tuple[str, float]]:
        """
        Answer multiple questions in batches with confidence scores.

        Args:
            questions: List of criterion questions to answer
            batch_size: If None, process all at once. Otherwise, chunk into batches.

        Returns:
            Dict mapping question_text -> (answer, confidence)
            where answer is "YES"/"NO"/"UNKNOWN" and confidence is 0.0-1.0
        """
        if not questions:
            return {}

        if batch_size is None:
            return self._answer_single_batch_with_confidence(questions)
        else:
            results = {}
            for i in range(0, len(questions), batch_size):
                batch = questions[i:i+batch_size]
                try:
                    batch_results = self._answer_single_batch_with_confidence(batch)
                    results.update(batch_results)
                except ParseError as e:
                    print(f"  [Warning] Batch parse failed: {e}")
                    print(f"  [Fallback] Processing batch sequentially...")
                    fallback_results = self._fallback_to_sequential_with_confidence(batch)
                    results.update(fallback_results)

            return results

    def _answer_single_batch_with_confidence(
        self,
        questions: List[str]
    ) -> Dict[str, Tuple[str, float]]:
        """
        Process a single batch of questions via LLM with confidence scores.

        Args:
            questions: List of questions for this batch

        Returns:
            Dict mapping question_text -> (answer, confidence)
        """
        prompt = self._build_batch_prompt_with_confidence(questions)
        system_prompt = self._build_batch_system_prompt_with_confidence()

        try:
            result = self.provider.extract(
                dsm5_text=prompt,
                disorder_id="batch_assessment",
                template_guide="",
                custom_prompt=prompt,
                custom_system_prompt=system_prompt
            )

            if not result.success:
                raise ParseError(f"LLM call failed: {result.error}")

            return self._parse_batch_response_with_confidence(result.content, questions)

        except Exception as e:
            raise ParseError(f"Batch processing failed: {str(e)}")

    def _build_batch_prompt_with_confidence(self, questions: List[str]) -> str:
        """Build prompt for batch question answering with confidence scores."""
        questions_list = "\n".join([
            f"{i+1}. {q}"
            for i, q in enumerate(questions)
        ])

        prompt = f"""You are an expert clinical psychiatrist performing a structured assessment.
Analyze the following patient vignette:

"{self.clinical_text}"

TASK: Answer ALL {len(questions)} questions below based ONLY on the patient vignette above.
For each answer, also provide your confidence level (0.0 to 1.0).

QUESTIONS:
{questions_list}

INSTRUCTIONS:
- For each question, answer YES, NO, or UNKNOWN
- YES: Clear evidence in vignette supports this criterion
- NO: Vignette explicitly contradicts or denies this criterion
- UNKNOWN: No information available to confirm or deny
- For "Is it true that..." questions:
  - These define required conditions that must be TRUE
  - Answer YES if condition is satisfied (or not mentioned if it's an exclusion)
  - Answer NO if condition is violated
  - Answer UNKNOWN only if explicitly ambiguous

CONFIDENCE SCALE:
- 1.0 = Absolutely certain, explicit clear evidence
- 0.8-0.9 = Very confident, strong evidence
- 0.6-0.7 = Moderately confident, reasonable inference
- 0.4-0.5 = Uncertain, weak evidence
- 0.1-0.3 = Very uncertain, mostly guessing

OUTPUT FORMAT (JSON ONLY):
{{
  "1": {{"answer": "YES", "confidence": 0.9}},
  "2": {{"answer": "NO", "confidence": 0.8}},
  "3": {{"answer": "UNKNOWN", "confidence": 0.5}},
  ...
  "{len(questions)}": {{"answer": "YES", "confidence": 0.85}}
}}

Return ONLY the JSON object above with answers and confidence for ALL {len(questions)} questions."""

        return prompt

    def _build_batch_system_prompt_with_confidence(self) -> str:
        """Build system prompt for batch processing with confidence."""
        return """You are a clinical assessment system.
Output ONLY valid JSON with NO additional text, markdown, or explanations.
Format: {"1": {"answer": "YES", "confidence": 0.9}, "2": {"answer": "NO", "confidence": 0.7}, ...}
Each key is a question number (string), each value has "answer" (YES/NO/UNKNOWN) and "confidence" (0.0-1.0)."""

    def _parse_batch_response_with_confidence(
        self,
        content: str,
        questions: List[str]
    ) -> Dict[str, Tuple[str, float]]:
        """Parse LLM response with confidence into question->(answer, confidence) mapping."""
        # Clean markdown code blocks
        content_clean = content.strip()
        if content_clean.startswith("```json"):
            content_clean = content_clean[7:]
        elif content_clean.startswith("```"):
            content_clean = content_clean[3:]
        if content_clean.endswith("```"):
            content_clean = content_clean[:-3]
        content_clean = content_clean.strip()

        try:
            data = json.loads(content_clean)
        except json.JSONDecodeError:
            # Try to extract JSON from mixed content
            json_match = re.search(r'\{[\s\S]*\}', content_clean)
            if json_match:
                data = json.loads(json_match.group())
            else:
                raise ParseError("Could not parse JSON from response")

        result = {}
        for i, question in enumerate(questions):
            key = str(i + 1)
            if key not in data:
                # Default if missing
                result[question] = ("UNKNOWN", 0.5)
                continue

            entry = data[key]
            if isinstance(entry, dict):
                answer = entry.get("answer", "UNKNOWN").strip().upper()
                confidence = float(entry.get("confidence", 0.7))
            elif isinstance(entry, str):
                # Fallback: just answer string, no confidence
                answer = entry.strip().upper()
                confidence = 0.7
            else:
                answer = "UNKNOWN"
                confidence = 0.5

            if answer not in ["YES", "NO", "UNKNOWN"]:
                answer = "UNKNOWN"

            # Ensure confidence in range
            confidence = max(0.0, min(1.0, confidence))

            result[question] = (answer, confidence)

        return result

    def _fallback_to_sequential_with_confidence(
        self,
        questions: List[str]
    ) -> Dict[str, Tuple[str, float]]:
        """Fallback to sequential processing with confidence if batch fails."""
        results = {}
        for q in questions:
            answer, confidence = super().answer_with_confidence(q)
            results[q] = (answer, confidence)
        return results
