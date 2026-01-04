# Simplified Implementation Plan

## What We Have

```
src/prolog/
├── schema.pl              # ~950 lines - simplified inference engine
└── gold_standard/         # 5 disorders defined (MDD, GAD, ADHD, PTSD, ASD)

src/extraction/            # Layer 1 KB construction (works, demonstrated)

src/reasoning/engine.py    # Thin pyswip wrapper

data/vignettes/            # Test cases

tests/
└── test_prolog_schema.py  # 34 pytest tests
```

### Schema API (Implemented)
- `full_diagnosis(PatientID, DisorderID, Result)` - Comprehensive diagnosis
- `criterion_check(PatientID, DisorderID, Type, Status, Details)` - Check criterion
- `get_missing_items(PatientID, DisorderID, Items)` - Get unevaluated items (IDs only)
- `explain_diagnosis(PatientID, DisorderID, Explanation)` - Generate explanation

## What We Need to Build

### 1. Simple Diagnostic Driver (`src/diagnosis/driver.py`)

A minimal Python script that:

```python
# Pseudocode
def run_diagnosis(disorder_id):
    engine = PrologEngine()
    engine.load_schema()
    engine.load_gold_standards()

    # Get missing items from KB (IDs only - Python formats text)
    missing = engine.query(f"get_missing_items(patient, {disorder_id}, Items)")

    # Simple ordering: symptoms first, then exclusions, then duration/onset
    ordered = order_by_priority(missing)

    for item in ordered:
        answer = ask_user(item)  # or LLM for vignette testing
        engine.assert_fact(item_to_fact(item, answer))

        # Check current status
        result = engine.query(f"full_diagnosis(patient, {disorder_id}, Result)")
        if result.overall_status in ['met', 'not_met']:
            break

    return engine.query(f"full_diagnosis(patient, {disorder_id}, Result)")
```

**Key principle**: Prolog generates item IDs and evaluates answers. Python formats text and orchestrates.

### 2. Question Generation in Prolog

Add to `schema.pl`:

```prolog
% Generate all questions for a disorder
disorder_questions(DisorderID, Questions) :-
    findall(q(symptom, ID, Desc), symptom(DisorderID, ID, _, Desc), SymptomQs),
    findall(q(exclusion, ID, Desc), exclusion_criterion(DisorderID, ID, _, Desc), ExclQs),
    findall(q(subjective, ID, Desc), subjective_criterion(DisorderID, ID, Desc, _), SubjQs),
    append([SymptomQs, ExclQs, SubjQs], Questions).

% Generate questions for all active disorders
all_questions(Questions) :-
    findall(Qs, (disorder(D, _, _), disorder_questions(D, Qs)), AllQs),
    flatten(AllQs, Questions),
    sort(Questions, Questions).  % Remove duplicates
```

### 3. Simple Question Ordering

Instead of A* search, use clinical priority ordering:

```python
def order_questions(questions):
    """Order by clinical priority, not information-theoretic optimality."""
    priority = {
        'core_symptom': 1,      # Ask core symptoms first
        'symptom': 2,           # Then other symptoms
        'exclusion': 3,         # Then exclusions
        'duration': 4,          # Then duration
        'onset': 5,             # Then onset
        'subjective': 6,        # Subjective last (needs context)
    }
    return sorted(questions, key=lambda q: priority.get(q.type, 99))
```

This satisfies the spec's requirement for "prioritising broad screening criteria before disorder-specific probes" without the complexity of A*.

### 4. Pruning Logic in Prolog

The spec mentions pruning ("if trauma criterion fails, all PTSD-specific questions are skipped"). Handle this in Prolog:

```prolog
% Check if disorder should be pruned (definitely ruled out)
disorder_pruned(PatientID, DisorderID) :-
    % Core symptom absent
    symptom_category(DisorderID, core, CoreSymptoms, _, _),
    forall(member(S, CoreSymptoms),
           patient_symptom(PatientID, S, absent, _)).

disorder_pruned(PatientID, DisorderID) :-
    % Exclusion applies
    exclusion_criterion(DisorderID, ExcID, _, _),
    patient_exclusion_status(PatientID, ExcID, excluded).

% Get remaining candidate disorders
active_candidates(PatientID, Candidates) :-
    findall(D, (disorder(D, _, _), \+ disorder_pruned(PatientID, D)), Candidates).
```

### 5. Vignette Evaluation (`src/evaluation/evaluate.py`)

Simple evaluation without batch experiments:

```python
def evaluate_on_vignettes(vignettes_path, provider="openai"):
    """Run diagnosis on vignettes and measure accuracy."""
    vignettes = load_vignettes(vignettes_path)
    results = []

    for v in vignettes:
        predicted = run_diagnosis_on_vignette(v, provider)
        correct = predicted == v['ground_truth']
        results.append({
            'id': v['id'],
            'ground_truth': v['ground_truth'],
            'predicted': predicted,
            'correct': correct,
            'questions_asked': len(v['answers'])
        })

    accuracy = sum(r['correct'] for r in results) / len(results)
    avg_questions = sum(r['questions_asked'] for r in results) / len(results)

    print(f"Accuracy: {accuracy:.1%}")
    print(f"Avg questions: {avg_questions:.1f}")

    return results
```

### 6. Tier B: Subjective Assessment

For subjective criteria ("clinically significant distress", "excessive worry"), the LLM provides a suggestion with confidence that can be overridden:

```python
def assess_subjective(criterion_desc, clinical_notes, provider, interactive=False):
    """
    LLM assesses subjective criterion, user can override.

    Args:
        criterion_desc: The subjective criterion text
        clinical_notes: Patient clinical notes/vignette
        provider: LLM provider instance
        interactive: If True, prompt user for override

    Returns:
        (assessment, confidence, reasoning)
    """
    prompt = f"""
    <criterion>{criterion_desc}</criterion>
    <clinical_notes>{clinical_notes}</clinical_notes>

    Assess whether this subjective criterion is met based on the clinical notes.

    Respond with:
    ASSESSMENT: [met/not_met/unclear]
    CONFIDENCE: [0.0-1.0]
    REASONING: [brief clinical justification]
    """

    result = provider.complete(prompt)
    assessment, confidence, reasoning = parse_assessment(result)

    if interactive:
        print(f"\nSubjective Criterion: {criterion_desc}")
        print(f"LLM Assessment: {assessment} (confidence: {confidence:.0%})")
        print(f"Reasoning: {reasoning}")
        override = input("Accept? [y/n/change to met/not_met/unclear]: ").strip().lower()

        if override in ['met', 'not_met', 'unclear']:
            assessment = override
            confidence = 1.0  # User override has full confidence
        elif override != 'y':
            assessment = 'unclear'
            confidence = 0.5

    return assessment, confidence, reasoning
```

This implements the spec's Tier B: "LLM-generated suggestions with confidence scores, which clinicians can accept, modify, or override."

For vignette evaluation (non-interactive), the LLM assessment is used directly. For real diagnostic sessions, `interactive=True` allows clinician override.

## Implementation Order

### Phase 1: Core Diagnostic Loop
1. Add question generation predicates to `schema.pl`
2. Add pruning predicates to `schema.pl`
3. Write `src/diagnosis/driver.py` with simple ordering
4. Test manually with one disorder (MDD - simplest)

### Phase 2: Evaluation
1. Write `src/evaluation/evaluate.py`
2. Test on existing vignettes
3. Measure accuracy and question count

### Phase 3: Tier B Integration
1. Add LLM subjective assessment
2. Implement confidence propagation
3. Test on GAD (has "excessive worry" criterion)

### Phase 4: Polish
1. Clean up output formatting
2. Add explanation generation (Prolog proof trees)
3. Write simple tests

## Success Criteria (from Spec)

| Metric | Target | How to Measure |
|--------|--------|----------------|
| Diagnostic accuracy (clear cases) | >90% | Run on CLEAR vignettes |
| Correct disorder in top-2 | >90% | Check if ground truth in top 2 predictions |
| Questions vs exhaustive | >40% reduction | Compare to asking all questions |
| Inference time | <1s per query | Time Prolog queries |

## What We're NOT Building

- Complex A* search with information gain calculation
- Batch experiment framework
- Multi-provider comparison infrastructure
- Visualisation system
- Symptom alias mapping across disorders

## Files to Create

```
src/
├── diagnosis/
│   ├── __init__.py
│   └── driver.py          # Main diagnostic loop (~150 lines)
├── evaluation/
│   ├── __init__.py
│   └── evaluate.py        # Simple vignette evaluation (~100 lines)
└── prolog/
    └── schema.pl          # Add ~50 lines for question generation
```

**Total new code: ~300 lines** (vs ~2,000 lines archived)

## Key Design Principles

1. **Prolog does the reasoning** — Python is just glue
2. **Questions come from the KB** — Not hardcoded in Python
3. **Pruning is declarative** — Prolog rules, not Python if/else
4. **Simple ordering beats complex search** — Clinical priority is good enough
5. **Test on real vignettes** — Not synthetic batch experiments
