# System Architecture

This document describes the design and architecture of the Hybrid Diagnostic Decision Support System.

## System Overview

The system implements a **Category 3 Hybrid Approach** combining:
- **Symbolic reasoning** (Prolog expert system) for objective diagnostic criteria
- **Stochastic methods** (LLM) for knowledge extraction and subjective assessment

### Reference Standard

The knowledge base implements DSM-5-TR (2022) diagnostic criteria, not DSM-5 (2013). See [DSM Version Notes](DSM_VERSION_NOTES.md) for version differences and their impact on each disorder.

### Target Disorders

| Disorder | Abbreviation | Key Challenges | Complexity |
|----------|--------------|----------------|------------|
| Major Depressive Disorder | MDD | Count-based (5/9 symptoms), 2-week duration | Medium |
| Generalized Anxiety Disorder | GAD | Subjective "excessive worry" assessment | Medium |
| Attention-Deficit/Hyperactivity Disorder | ADHD | Onset before age 12, dual symptom domains | High |
| Posttraumatic Stress Disorder | PTSD | Trauma criterion, multiple symptom clusters | High |
| Autism Spectrum Disorder | ASD | Severity levels, developmental history | High |

## Data Flow

The system follows a three-layer pipeline from DSM-5 text to diagnostic inference:

```
Layer 1: Knowledge Base Construction
═══════════════════════════════════

data/dsm5_text/{DISORDER}.txt
           ↓
    LLM Extraction (with thinking)
    - Provider: Anthropic/OpenAI/Ollama
    - Thinking budget: 1024-15000 tokens
           ↓
    Prolog Output (.pl file)
           ↓
    Syntax Validation (SWI-Prolog)
    Schema Validation (validate_disorder/2)
           ↓
src/prolog/extracted/{disorder}.pl  OR  src/prolog/gold_standard/{disorder}.pl
```

```
Layer 2: Three-Tier Diagnostic Reasoning
═══════════════════════════════════════

Patient Data → Tier A: Objective (Prolog)
               - Symptom counts
               - Duration requirements
               - Onset requirements
               - Exclusion screening
                    ↓
               Tier B: Subjective (LLM)
               - Clinical significance
               - Severity assessment
               - Functional impairment
                    ↓
               Tier C: Integration (Prolog)
               - Combine Tier A + Tier B
               - Ranked diagnoses
               - Confidence scores
               - Explanations
```

```
Layer 3: Diagnostic Pathway Search (A*)
═══════════════════════════════════════

Initial State (all disorders pending)
           ↓
    Select next question
    - Priority: exclusions > core symptoms > duration/onset
    - Heuristic: discriminating power between candidates
           ↓
    Prune candidates
    - Core symptoms absent
    - Exclusions met
    - Duration/onset failed
           ↓
Terminal State (diagnosis confirmed/ruled out)
```

## Module Architecture

```
src/
├── prolog/                    # Knowledge Base
│   ├── schema.pl             # Core inference engine (1043 lines)
│   │   ├── Predicate definitions (disorder/3, symptom/4, ...)
│   │   ├── Criterion evaluation rules
│   │   ├── Diagnostic inference (diagnosis_candidate/3)
│   │   └── Missing data tracking
│   ├── gold_standard/        # Hand-curated disorder definitions
│   │   ├── loader.pl        # Loads all gold standard files
│   │   ├── mdd.pl           # Major Depressive Disorder
│   │   ├── gad.pl           # Generalized Anxiety Disorder
│   │   ├── adhd.pl          # ADHD
│   │   ├── ptsd.pl          # PTSD
│   │   └── README.md        # Template guide for .pl structure
│   └── extracted/            # LLM-extracted disorder definitions
│       └── asd.pl           # Autism Spectrum Disorder
│
├── reasoning/                 # Prolog Interface & Utilities
│   ├── engine.py            # PrologEngine class (pyswip wrapper)
│   ├── utils.py             # KB exploration (explore_disorder, list_disorders)
│   ├── viz.py               # Diagnostic flowchart visualization
│   └── __init__.py          # Module exports
│
├── extraction/               # LLM Extraction Pipeline
│   ├── base.py              # ExtractionProvider ABC, ExtractionResult
│   ├── config.py            # Environment config, API key management
│   ├── evaluate.py          # Syntax/schema validation
│   ├── run_extraction.py    # CLI entry point
│   └── providers/           # LLM provider implementations
│       ├── openai_provider.py    # GPT-5 with reasoning_effort
│       ├── anthropic_provider.py # Claude Opus 4.5 with thinking
│       └── ollama_provider.py    # Local models (gpt-oss, deepseek-r1)
│
├── search/                   # Diagnostic Pathway Optimization
│   ├── manager.py           # SessionManager with pruning logic
│   └── search.py            # DiagnosticSearch (A* with verification mode)
│
└── evaluation/               # Benchmarking & Testing
    └── benchmark.py         # ClinicalAnalyzer for vignette evaluation

data/
├── dsm5_text/               # DSM-5-TR source text files
│   ├── MDD.txt
│   ├── GAD.txt
│   ├── ADHD.txt
│   ├── PTSD.txt
│   └── ASD.txt
└── vignettes/               # Clinical test cases (JSON)
    ├── mdd_clear_*.json
    ├── mdd_moderate_*.json
    └── ...

outputs/
└── extractions/             # Timestamped extraction results
    └── FINDINGS_2025-12-19.md  # Provider comparison report
```

## Three-Tier Diagnostic Reasoning

The diagnostic engine implements a hybrid architecture separating objective from subjective criteria:

### Tier A: Objective (Prolog)

**Criteria Type:** Objectively verifiable facts

**Handler:** Prolog rule-based inference

**Examples:**
- Symptom counts: "≥5 of 9 symptoms present"
- Duration: "Symptoms persist for ≥2 weeks"
- Onset: "Several symptoms present before age 12"
- Exclusions: "Not attributable to substance use"

**Implementation:**
```prolog
meets_symptom_criteria(PatientID, DisorderID) :-
    disorder(DisorderID, _, _),
    findall(CatID, symptom_category(DisorderID, CatID, _, _, _), Categories),
    check_all_symptom_categories(PatientID, DisorderID, Categories).

meets_duration_criteria(PatientID, DisorderID) :-
    duration_requirement(DisorderID, MinDuration, Unit),
    patient_duration(PatientID, DisorderID, Days),
    duration_in_days(MinDuration, Unit, RequiredDays),
    Days >= RequiredDays.
```

**Advantages:**
- Transparent reasoning chains
- Deterministic evaluation
- Complete audit trail

### Tier B: Subjective (LLM)

**Criteria Type:** Requires clinical judgment

**Handler:** LLM-based assessment

**Examples:**
- Clinical significance: "Symptoms cause clinically significant distress"
- Severity: "Excessive worry beyond what is appropriate"
- Functional impairment: "Impairs social or occupational functioning"

**Implementation:**
```python
class ClinicalAnalyzer:
    def assess_subjective_criterion(self, criterion: str, vignette: str) -> Assessment:
        prompt = f"""
        <criterion>{criterion}</criterion>
        <vignette>{vignette}</vignette>

        Assess whether the criterion is met with confidence 0-1.
        """
        response = self.llm.complete(prompt)
        return Assessment(met=response.met, confidence=response.confidence)
```

**Advantages:**
- Handles nuanced clinical judgment
- Quantifies uncertainty (confidence scores)
- Allows clinician override

### Tier C: Integration (Prolog)

**Purpose:** Combine Tier A and Tier B results into final diagnosis

**Handler:** Prolog inference with confidence propagation

**Implementation:**
```prolog
diagnosis_candidate(PatientID, DisorderID, Confidence) :-
    % Tier A: Objective criteria
    meets_symptom_criteria(PatientID, DisorderID),
    meets_duration_criteria(PatientID, DisorderID),
    meets_onset_criteria(PatientID, DisorderID),
    meets_exclusion_criteria(PatientID, DisorderID),

    % Tier B: Subjective criteria
    meets_subjective_criteria(PatientID, DisorderID),

    % Tier C: Integrate and calculate confidence
    calculate_diagnosis_confidence(PatientID, DisorderID, Confidence).
```

**Output:**
```
diagnosis_candidate(pt001, mdd, 0.92).
```

**Advantages:**
- Maintains explainability from Tier A
- Incorporates uncertainty from Tier B
- Single unified diagnostic result

## Design Decisions

### Why Prolog for Tier A?

**Rationale:** Objective diagnostic criteria are rule-based and deterministic. Prolog provides:
- Natural expression of DSM-5 logical rules
- Built-in backtracking for criterion checking
- Transparent inference chains for explainability
- Easy validation against gold standard

**Alternative Considered:** Python decision trees - rejected due to:
- More verbose rule encoding
- Harder to validate correctness
- Less natural fit for logical constraints

### Why LLM for Tier B?

**Rationale:** Subjective criteria require clinical judgment and nuanced interpretation. LLMs provide:
- Natural language understanding of vignettes
- Ability to assess "excessive", "clinically significant", etc.
- Confidence quantification for uncertain judgments

**Alternative Considered:** Rule-based heuristics - rejected due to:
- Inability to capture clinical nuance
- Brittle keyword matching
- No uncertainty quantification

### Why Three Tiers Instead of End-to-End LLM?

**Rationale:** Separation of concerns improves:
- **Explainability:** Tier A provides transparent reasoning
- **Validation:** Tier A can be validated against gold standard
- **Reliability:** Tier A is deterministic, reducing hallucination risk
- **Efficiency:** Tier B only called for subjective criteria (1-2 per disorder)

**Evidence:** See [Provider Evaluation](PROVIDER_EVALUATION.md) - even best LLMs (Claude Opus 4.5) miss objective criteria 5-10% of the time when doing end-to-end extraction.

### Why A* Search for Pathway Optimization?

**Rationale:** Diagnostic interviews can be optimized to minimize questions:
- **Prioritization:** Ask high-value questions first (exclusions, core symptoms)
- **Pruning:** Eliminate candidates early based on responses
- **Verification:** Boost onset/duration questions when symptoms met

**Evidence:** A* reduces average questions from 40-50 (exhaustive) to 15-25 (optimized), a 40-60% reduction.

### Why pyswip Over Other Prolog Interfaces?

**Rationale:** pyswip provides:
- Mature Python-SWI-Prolog binding
- Active maintenance and community support
- Compatible with SWI-Prolog 9.x
- Simple API for consulting files and queries

**Alternative Considered:** janus (official SWI-Prolog Python interface) - rejected due to:
- Less mature at time of development
- Less documentation and examples
- pyswip sufficient for our needs

## Performance Characteristics

| Component | Operation | Performance |
|-----------|-----------|-------------|
| LLM Extraction | Full disorder | 30-120s (depends on provider/thinking budget) |
| Prolog Query | Symptom count check | <1ms |
| Prolog Query | Full diagnosis | 1-5ms |
| LLM Assessment | Subjective criterion | 2-5s |
| A* Search | Complete pathway (15-25 questions) | 30-60s (including LLM calls) |

## Scalability

The current implementation supports:
- **Disorders:** 5 implemented (MDD, GAD, ADHD, PTSD, ASD), extensible to all DSM-5-TR
- **Concurrent sessions:** Limited by pyswip (single Prolog instance per process)
- **KB size:** Schema.pl (1043 lines), each disorder (200-400 lines) - scales linearly

**Future Optimization:**
- Multi-process Prolog instances for concurrent patients
- Caching of LLM subjective assessments
- Pre-computed symptom alias mappings

## Related Documentation

- [Getting Started](GETTING_STARTED.md) - Setup and first steps
- [API Reference](API_REFERENCE.md) - Prolog predicate reference
- [Provider Evaluation](PROVIDER_EVALUATION.md) - LLM comparison
- [Extraction Benchmarks](EXTRACTION_BENCHMARKS.md) - Detailed test results
