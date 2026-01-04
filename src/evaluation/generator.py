
import json
import argparse
import random
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Any
from tqdm import tqdm

from src.extraction.config import Config
from src.extraction.providers import get_provider

# Disorders to generate cases for
DISORDERS = ['mdd', 'gad', 'adhd', 'ptsd', 'asd']
# Complexity levels
TYPES = ['CLEAR', 'MODERATE', 'AMBIGUOUS']
# Common comorbid pairs
COMORBID_PAIRS = [
    ('adhd', 'mdd'),
    ('gad', 'mdd'),
    ('ptsd', 'mdd'),
    ('adhd', 'gad'),
]

def build_generation_prompt(disorder: str, difficulty: str) -> str:
    """Build the prompt for generating a single vignette."""
    
    prompt = f"""You are an expert clinical psychiatrist engaging in a simulation exercise.
Your task is to generate a realistic clinical vignette for a patient with {disorder.upper()}.

Complexity Level: {difficulty}
{get_complexity_instruction(difficulty, disorder)}

## Output Structure (JSON only):
Output a single valid JSON object with these fields:
- "id": "generate unique string"
- "ground_truth": "{disorder}"
- "difficulty": "{difficulty}"
- "demographics": "Age, gender, occupation, etc."
- "clinical_text": "The full narrative description of the patient's presentation."
- "symptom_checklist": {{ "symptom_id": "present/absent" }} (Optional mapping if possible)

## CRITICAL RULES:
1. **BLINDING**: Do NOT mention the name of the disorder ("{disorder.upper()}") or any related codes (e.g., "F32.9") in the "clinical_text" or "demographics". The "clinical_text" must be a natural observation of symptoms and history.
2. **REALISM**: Use professional clinical language mixed with direct patient quotes.
3. **CRITERIA**: Ensure the symptoms described strictly align with DSM-5-TR criteria for {disorder.upper()}.
4. **FORMAT**: Return ONLY raw JSON. No markdown code blocks.
"""
    return prompt

def get_complexity_instruction(difficulty: str, disorder: str) -> str:
    if difficulty == 'CLEAR':
        return "The case should be a textbook presentation with obvious symptoms meeting all criteria and no significant conflicting features."
    elif difficulty == 'MODERATE':
        return "The case should have some non-standard presentation features or mild comorbidities, but the primary diagnosis remains clear."
    elif difficulty == 'AMBIGUOUS':
        differential = random.choice([d for d in DISORDERS if d != disorder])
        return f"The case should present a diagnostic challenge with overlapping features of {differential.upper()}. However, strictly strictly speaking, valid criteria for {disorder.upper()} are met, while {differential.upper()} should be excluded."
    return ""

def build_comorbid_generation_prompt(disorders: tuple, difficulty: str) -> str:
    """Build the prompt for generating a comorbid vignette with multiple diagnoses."""

    disorder_list = " and ".join([d.upper() for d in disorders])

    prompt = f"""You are an expert clinical psychiatrist engaging in a simulation exercise.
Your task is to generate a realistic clinical vignette for a patient with COMORBID diagnoses: {disorder_list}.

Complexity Level: {difficulty}
The patient must meet FULL DSM-5-TR criteria for BOTH {disorders[0].upper()} AND {disorders[1].upper()}.

## Output Structure (JSON only):
Output a single valid JSON object with these fields:
- "id": "generate unique string"
- "ground_truth": [{", ".join([f'"{d}"' for d in disorders])}] (array of disorders)
- "difficulty": "{difficulty}"
- "demographics": "Age, gender, occupation, etc."
- "clinical_text": "The full narrative description of the patient's presentation showing symptoms of both disorders."
- "symptom_checklist": {{ "symptom_id": "present/absent" }} (Optional mapping if possible)
- "comorbid": true

## CRITICAL RULES:
1. **BLINDING**: Do NOT mention the names of the disorders or any related codes in the "clinical_text" or "demographics". The "clinical_text" must be a natural observation of symptoms and history.
2. **REALISM**: Use professional clinical language mixed with direct patient quotes. Show how both conditions interact and manifest.
3. **CRITERIA**: Ensure the symptoms described strictly align with DSM-5-TR criteria for BOTH {disorders[0].upper()} AND {disorders[1].upper()}.
4. **COMORBIDITY**: The symptoms should naturally overlap and interact as they would in real comorbid presentations.
5. **FORMAT**: Return ONLY raw JSON. No markdown code blocks.
"""
    return prompt

def generate_vignettes(provider, count: int, output_dir: Path):
    """Generate independent vignettes with varied difficulty and some comorbid cases."""
    generated = []

    # Determine how many comorbid cases (~10% of total, at least 2)
    num_comorbid = max(2, int(count * 0.1))
    num_single = count - num_comorbid

    # Create a balanced difficulty distribution
    # Aim for roughly: 40% CLEAR, 35% MODERATE, 25% AMBIGUOUS
    difficulties = (
        ['CLEAR'] * int(count * 0.40) +
        ['MODERATE'] * int(count * 0.35) +
        ['AMBIGUOUS'] * (count - int(count * 0.40) - int(count * 0.35))
    )
    random.shuffle(difficulties)

    print(f"\nGenerating {count} vignettes:")
    print(f"  - {num_single} single-disorder cases")
    print(f"  - {num_comorbid} comorbid cases")
    print(f"  - Difficulty: {difficulties.count('CLEAR')} CLEAR, {difficulties.count('MODERATE')} MODERATE, {difficulties.count('AMBIGUOUS')} AMBIGUOUS\n")

    case_index = 0

    # Generate single-disorder cases
    pbar = tqdm(total=count, desc="Generating vignettes", unit="case")

    for i in range(num_single):
        disorder = DISORDERS[i % len(DISORDERS)]
        difficulty = difficulties[case_index]
        case_index += 1

        pbar.set_description(f"Gen {disorder.upper()} ({difficulty})")

        prompt = build_generation_prompt(disorder, difficulty)

        try:
            result = provider.extract(
                dsm5_text=prompt,
                disorder_id="TEST",
                template_guide="",
                custom_prompt=prompt,
                custom_system_prompt="You are a data generator. Output only valid JSON."
            )

            if result.success:
                content = result.content.strip()
                if content.startswith("```json"):
                    content = content[7:]
                if content.endswith("```"):
                    content = content[:-3]

                data = json.loads(content)
                data['id'] = f"vig_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{i:03d}_{disorder}"
                data['comorbid'] = False
                generated.append(data)
                pbar.set_postfix({"success": len(generated), "failed": i + 1 - len(generated)})
            else:
                tqdm.write(f"  ✗ Failed [{i+1}]: {result.error}")
                pbar.set_postfix({"success": len(generated), "failed": i + 1 - len(generated)})

        except Exception as e:
            tqdm.write(f"  ✗ Error [{i+1}]: {e}")
            pbar.set_postfix({"success": len(generated), "failed": i + 1 - len(generated)})

        pbar.update(1)

    # Generate comorbid cases
    for i in range(num_comorbid):
        disorders = random.choice(COMORBID_PAIRS)
        difficulty = difficulties[case_index]
        case_index += 1

        disorder_str = f"{disorders[0].upper()}+{disorders[1].upper()}"
        pbar.set_description(f"Gen COMORBID {disorder_str} ({difficulty})")

        prompt = build_comorbid_generation_prompt(disorders, difficulty)

        try:
            result = provider.extract(
                dsm5_text=prompt,
                disorder_id="TEST",
                template_guide="",
                custom_prompt=prompt,
                custom_system_prompt="You are a data generator. Output only valid JSON."
            )

            if result.success:
                content = result.content.strip()
                if content.startswith("```json"):
                    content = content[7:]
                if content.endswith("```"):
                    content = content[:-3]

                data = json.loads(content)
                data['id'] = f"vig_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{num_single + i:03d}_comorbid_{disorders[0]}_{disorders[1]}"
                generated.append(data)
                pbar.set_postfix({"success": len(generated), "failed": num_single + i + 1 - len(generated)})
            else:
                tqdm.write(f"  ✗ Failed [{num_single + i + 1}]: {result.error}")
                pbar.set_postfix({"success": len(generated), "failed": num_single + i + 1 - len(generated)})

        except Exception as e:
            tqdm.write(f"  ✗ Error [{num_single + i + 1}]: {e}")
            pbar.set_postfix({"success": len(generated), "failed": num_single + i + 1 - len(generated)})

        pbar.update(1)

    pbar.close()

    # Save
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    outfile = output_dir / f"vignettes_{timestamp}.json"
    with open(outfile, 'w') as f:
        json.dump(generated, f, indent=2)

    print(f"\n{'='*60}")
    print(f"✓ Saved {len(generated)}/{count} cases to {outfile}")

    # Print summary statistics
    if generated:
        comorbid_count = sum(1 for v in generated if v.get('comorbid', False))
        print(f"\nSummary:")
        print(f"  - Single-disorder: {len(generated) - comorbid_count}")
        print(f"  - Comorbid: {comorbid_count}")
        diff_counts = {}
        for v in generated:
            diff = v.get('difficulty', 'UNKNOWN')
            diff_counts[diff] = diff_counts.get(diff, 0) + 1
        for diff, count in sorted(diff_counts.items()):
            print(f"  - {diff}: {count}")
        print(f"{'='*60}")

def main():
    parser = argparse.ArgumentParser(description="Generate synthetic clinical vignettes")
    parser.add_argument("--count", type=int, default=5, help="Number of cases to generate")
    parser.add_argument("--provider", default="openai", choices=["openai", "anthropic", "ollama"])
    args = parser.parse_args()
    
    # Setup
    root = Path(__file__).parent.parent.parent
    output_dir = root / 'data' / 'vignettes'
    output_dir.mkdir(parents=True, exist_ok=True)
    
    config = Config.from_env()
    
    # Initialize provider
    try:
        provider = get_provider(args.provider)
        generate_vignettes(provider, args.count, output_dir)
    except Exception as e:
        print(f"Fatal error: {e}")

if __name__ == "__main__":
    main()
