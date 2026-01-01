
import json
import argparse
import random
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Any

from src.extraction.config import Config
from src.extraction.providers import get_provider

# Disorders to generate cases for
DISORDERS = ['mdd', 'gad', 'adhd', 'ptsd', 'asd']
# Complexity levels
TYPES = ['CLEAR', 'MODERATE', 'AMBIGUOUS']

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

def generate_vignettes(provider, count: int, output_dir: Path):
    """Generate independent vignettes."""
    generated = []
    
    print(f"Generating {count} vignettes...")
    
    print(f"Generating {count} vignettes...")
    
    for i in range(count):
        # Cycle through disorders if count covers multiples
        disorder = DISORDERS[i % len(DISORDERS)]
        
        # Difficulty: Weighted choice? Or verify diverse difficulties?
        # For now, random difficulty is fine, or we can cycle too.
        difficulty = random.choice(TYPES)
        
        print(f"[{i+1}/{count}] Generating {disorder.upper()} ({difficulty})...")
        
        prompt = build_generation_prompt(disorder, difficulty)
        
        try:
            # We use a simple system prompt
            result = provider.extract(
                dsm5_text=prompt, # Hack: passing prompt as dsm5_text since base.extract might expect it, 
                                  # but we'll override with custom_prompt if provider supports it logic.
                                  # Actually looking at base.py, extract takes custom_prompt.
                                  # Let's see how providers implement it.
                disorder_id="TEST",
                template_guide="",
                custom_prompt=prompt,
                custom_system_prompt="You are a data generator. Output only valid JSON."
            )
            
            if result.success:
                # Clean up json
                content = result.content.strip()
                if content.startswith("```json"):
                    content = content[7:]
                if content.endswith("```"):
                    content = content[:-3]
                
                data = json.loads(content)
                # Enforce ID
                data['id'] = f"vig_{datetime.now().strftime('%M%S')}_{i}_{disorder}"
                generated.append(data)
            else:
                print(f"Failed to generate: {result.error}")
                
        except Exception as e:
            print(f"Error in generation loop: {e}")

    # Save
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    outfile = output_dir / f"vignettes_{timestamp}.json"
    with open(outfile, 'w') as f:
        json.dump(generated, f, indent=2)
    
    print(f"\nSaved {len(generated)} cases to {outfile}")

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
