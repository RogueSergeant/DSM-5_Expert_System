# Hybrid AI System for DSM-5-TR Mental Health Diagnostic Assessment
## Combining Symbolic Reasoning with Large Language Models

**Module**: 7COSC013W.1 Foundations of AI
**Student ID**: [Your Student ID]
**Word Count**: [4,000-5,000 words]
**Date**: January 2026

---

## Abstract
<!-- ~150 words -->
<!-- Brief overview of the problem, approach (Category 3 hybrid: Prolog + LLM), key results, and conclusions -->

[Write your abstract here]

---

# Part A: Project Description
<!-- 15 marks, ~800-1000 words total -->

## 1. Problem Statement and Real-World Context
<!-- ~250 words -->

### 1.1 The Diagnostic Challenge

[Describe the complexity of mental health diagnosis - requires both objective criteria (symptom counts, durations) and subjective clinical judgment]

### 1.2 DSM-5-TR Complexity

[Explain DSM-5-TR structure: 5+ disorders with 20-50 diagnostic criteria each, requiring structured reasoning]

### 1.3 Gap in Existing Approaches

[Discuss why pure LLM systems lack explainability; pure rule systems can't handle clinical nuance]

### 1.4 Real-World Need

[Articulate the need for clinical decision support that is transparent, auditable, and handles uncertainty]

---

## 2. Proposed AI Techniques
<!-- ~300 words -->

This project implements a **Category 3: Hybrid Approach** combining three core AI technique categories:

| Technique | Category | Application in System |
|-----------|----------|----------------------|
| Prolog-based symbolic reasoning | Knowledge Representation | Tier A: Objective criteria (symptom counts, duration, exclusions) |
| LLM-assisted judgment | Machine Learning | Tier B: Subjective criteria (clinical significance, severity) |
| A* search optimisation | Search & Optimisation | Question sequencing for efficient diagnosis |

### 2.1 Why a Hybrid Approach?

[Justify why neither symbolic nor neural approaches alone suffice for high-stakes medical diagnosis requiring both transparency and nuance]

### 2.2 Three-Tier Architecture Overview

[Brief description of Tier A (Prolog), Tier B (LLM), Tier C (Integration)]

---

## 3. Dataset and Problem Instance Description
<!-- ~200 words -->

### 3.1 Knowledge Base

- **Source**: DSM-5-TR (2022) diagnostic criteria
- **Disorders**: MDD, GAD, ADHD, PTSD, ASD
- **Location**: `data/dsm5_text/`

### 3.2 Test Data

- Clinical vignettes with ground truth diagnoses
- Location: `data/vignettes/`

### 3.3 Gold Standards

- Hand-curated Prolog representations
- Location: `src/prolog/gold_standard/`

[Describe the data sources and their characteristics]

---

## 4. Success Criteria and Evaluation Metrics
<!-- ~200 words -->

| Metric | Target | Rationale |
|--------|--------|-----------|
| Diagnostic accuracy | >85% | Match expected diagnoses on clinical vignettes |
| Question efficiency | 40-60% reduction | Fewer questions than exhaustive baseline approach |
| Extraction validity | >95% syntax correct | LLM-generated Prolog must be syntactically valid |
| Explainability | Full audit trail | Every diagnostic decision traceable to DSM-5-TR criteria |

[Expand on each metric and justify your targets]

---

# Part B: Technical Implementation and Evaluation
<!-- 85 marks total -->

## 5. System Architecture
<!-- ~600 words -->

### 5.1 Three-Tier Design

```
┌─────────────────────────────────────────────────────┐
│                    Tier C: Integration               │
│     Prolog-based confidence propagation + ranking   │
└─────────────────────────────────────────────────────┘
         ▲                           ▲
         │                           │
┌────────┴────────┐       ┌─────────┴─────────┐
│ Tier A: Symbolic │       │ Tier B: Stochastic │
│ (Prolog Engine)  │       │ (LLM Assessment)   │
│                  │       │                    │
│ • Symptom counts │       │ • Clinical         │
│ • Duration rules │       │   significance     │
│ • Onset checks   │       │ • Severity ratings │
│ • Exclusions     │       │ • Confidence 0-1   │
└──────────────────┘       └────────────────────┘
```

### 5.2 Data Flow

[Describe how data flows through the three tiers]

### 5.3 Component Interactions

[Explain how Python orchestrates Prolog reasoning and LLM calls]

**Key Files:**
- `src/prolog/schema.pl` - Inference engine
- `src/reasoning/engine.py` - Python-Prolog interface
- `src/extraction/` - LLM extraction pipeline

---

## 6. Knowledge Representation Implementation
<!-- ~800 words -->

### 6.1 Prolog Knowledge Base Design

#### 6.1.1 Predicate Structure

[Explain the core predicates:]

```prolog
% Disorder definition
disorder(DisorderID, FullName, Category).

% Symptom definition
symptom(DisorderID, SymptomID, Category, Description).

% Symptom grouping with count requirements
symptom_category(DisorderID, CategoryName, SymptomList, RequirementType, Count).
% RequirementType: at_least, exactly, all, at_least_one_of
```

#### 6.1.2 DSM-5-TR Criteria Encoding

[Describe how DSM-5-TR criteria are encoded in Prolog]

### 6.2 Diagnostic Inference Rules

#### 6.2.1 Criterion Checking Predicates

```prolog
meets_symptom_criteria(DisorderID, PatientID) :-
    % Logic for checking symptom count requirements
    ...

meets_duration_criteria(DisorderID, PatientID) :-
    % Logic for checking duration requirements
    ...
```

#### 6.2.2 Main Diagnosis Predicate

[Explain `diagnosis_candidate/3` and `full_diagnosis/3`]

### 6.3 Knowledge Acquisition Pipeline

#### 6.3.1 LLM Extraction Process

[Describe the DSM text → Prolog conversion process]

#### 6.3.2 Multi-Provider Architecture

[Explain support for Anthropic, OpenAI, Ollama]

#### 6.3.3 Validation Pipeline

1. **Syntax Validation**: SWI-Prolog subprocess checks
2. **Schema Validation**: Prolog-based completeness checks
3. **Gold Standard Comparison**: Accuracy verification

---

## 7. Search and Optimisation Implementation
<!-- ~600 words -->

### 7.1 Problem Formulation

#### 7.1.1 State Space Definition

- **State**: Set of answered questions × active diagnosis candidates
- **Initial State**: No questions answered, all disorders as candidates
- **Goal State**: All candidates confirmed or excluded with minimum questions

#### 7.1.2 Search Space Complexity

[Discuss the combinatorial complexity of exhaustive questioning]

### 7.2 A* Search with Heuristic Design

#### 7.2.1 Heuristic Function

```python
def heuristic_score(question, state):
    score = 0
    if is_core_symptom(question):
        score += 50  # Prioritize core symptoms
    if near_diagnosis_completion(state):
        score += 1000  # Finish verification
    return score
```

#### 7.2.2 Greedy Simplification

[Explain the real-time optimisation trade-offs]

### 7.3 Pruning Strategy

#### 7.3.1 Early Elimination Rules

- Exclusion criteria met → immediate candidate removal
- All core symptoms absent → definite failure

#### 7.3.2 Symptom Aliasing

[Explain how equivalent symptoms across disorders are mapped]

```python
SYMPTOM_ALIASES = {
    'sleep_disturbance': ['mdd_a4', 'gad_c4', 'ptsd_e6'],
    'concentration': ['mdd_a8', 'gad_c3', 'ptsd_e5', 'adhd_a1f'],
}
```

---

## 8. Machine Learning Integration
<!-- ~500 words -->

### 8.1 LLM for Subjective Assessment

#### 8.1.1 Clinical Significance Determination

[Explain how LLMs assess subjective criteria]

#### 8.1.2 Confidence Score Generation

[Describe the 0.0-1.0 confidence quantification]

### 8.2 LLM for Knowledge Extraction

#### 8.2.1 DSM Text to Prolog Conversion

[Explain the extraction process]

#### 8.2.2 Extended Thinking for Complex Criteria

[Discuss thinking budget and reasoning effort parameters]

### 8.3 LLM for Clinical Simulation (Benchmarking)

[Explain how LLMs simulate patient responses for evaluation]

---

## 9. Evaluation Results
<!-- 35 marks, ~1000 words -->

### 9.1 Quantitative Metrics

#### 9.1.1 Extraction Validity

| Metric | Anthropic | OpenAI | Ollama |
|--------|-----------|--------|--------|
| Syntax correctness | [%] | [%] | [%] |
| Schema compliance | [%] | [%] | [%] |
| Criteria completeness | [%] | [%] | [%] |

#### 9.1.2 Diagnostic Performance

| Metric | Result | Target | Status |
|--------|--------|--------|--------|
| Diagnostic accuracy | [%] | >85% | [Met/Not Met] |
| Question efficiency | [%] reduction | 40-60% | [Met/Not Met] |
| Average response time | [s] | <60s | [Met/Not Met] |

#### 9.1.3 Search Optimisation Results

| Disorder | Baseline Questions | Optimized Questions | Reduction |
|----------|-------------------|---------------------|-----------|
| MDD | [n] | [n] | [%] |
| GAD | [n] | [n] | [%] |
| ADHD | [n] | [n] | [%] |
| PTSD | [n] | [n] | [%] |
| ASD | [n] | [n] | [%] |

### 9.2 Provider Comparison (AI Approach Comparison)

| Provider | Speed | Completeness | Cost | Best Use Case |
|----------|-------|--------------|------|---------------|
| Anthropic (Claude) | 81-105s | Best (100%) | $0.16 | Gold standard creation |
| OpenAI (GPT-5) | 67-83s | Excellent (95%+) | $0.35 | Batch processing |
| Ollama (Local) | 506-1345s | Poor (missing criteria) | $0.00 | Not recommended for production |

[Discuss the implications of these findings]

### 9.3 Comparative Analysis: Hybrid vs Pure Approaches

| Approach | Explainability | Nuance Handling | Accuracy | Scalability |
|----------|----------------|-----------------|----------|-------------|
| Pure Prolog (symbolic only) | Excellent | Poor | Limited | Excellent |
| Pure LLM (neural only) | Poor | Excellent | Variable | Good |
| **Hybrid (this work)** | Excellent | Good | Best | Good |

#### 9.3.1 Why Hybrid Outperforms Pure Approaches

[Provide detailed analysis with evidence]

### 9.4 Trade-off Analysis

#### 9.4.1 Performance vs Explainability

[Discuss: LLMs faster but Prolog provides audit trails]

#### 9.4.2 Accuracy vs Completeness

[Discuss: Anthropic most complete but slower than OpenAI]

#### 9.4.3 Cost vs Quality

[Discuss: Ollama free but systematically incomplete]

#### 9.4.4 Complexity vs Maintainability

[Discuss: Three-tier requires more code but enables independent validation]

---

## 10. Critical Reflection
<!-- ~500 words -->

### 10.1 Design Decisions and Rationale

#### 10.1.1 Why Prolog Over Alternatives?

| Alternative | Rejected Because |
|-------------|------------------|
| Python decision trees | More verbose, harder to validate against DSM text |
| OWL/RDF ontologies | Overkill for rule-based criteria, steeper learning curve |
| Custom rule engine | Reinventing the wheel, less mature |

#### 10.1.2 Why Three Tiers Instead of Two?

[Justify the separation of integration logic]

#### 10.1.3 Why A* Over Exhaustive Search?

[Explain the efficiency gains]

### 10.2 Iterative Refinement Evidence

#### 10.2.1 Architecture Evolution

[Reference git history showing development progression]

#### 10.2.2 Provider Benchmarking Informing Recommendations

[Explain how evaluation results shaped the final recommendations]

#### 10.2.3 Gold Standard Validation Driving Improvements

[Discuss how validation failures led to extraction refinements]

### 10.3 Limitations

1. **Single Prolog instance**: pyswip limitation affects concurrent patient processing
2. **Ollama model limitations**: Systematic gaps for complex criteria like PTSD Criterion A
3. **LLM latency**: Subjective assessment adds 2-5s per criterion
4. **Limited disorder coverage**: Only 5 disorders implemented

### 10.4 Future Work

1. Multi-process Prolog instances for concurrent patients
2. LLM assessment caching for repeated queries
3. Integration with clinical ontologies (SNOMED CT, ICD-11)
4. Expansion to additional DSM-5-TR disorders
5. Clinical validation study with practising clinicians

---

## 11. Conclusion
<!-- ~200 words -->

[Summarize:]
- The problem addressed
- The hybrid approach taken
- Key achievements against success criteria
- Main contributions
- Recommendations for future development/deployment

---

## References

<!-- Use Harvard or APA style consistently -->

1. American Psychiatric Association. (2022). *Diagnostic and Statistical Manual of Mental Disorders* (5th ed., text rev.). American Psychiatric Publishing.

2. [SWI-Prolog documentation]

3. [pyswip library reference]

4. [OpenAI API documentation]

5. [Anthropic API documentation]

6. [Relevant academic papers on hybrid AI systems]

7. [Expert systems in medicine literature]

8. [A* search algorithm references]

---

## Appendices
<!-- Not counted in word limit -->

### Appendix A: Prolog Predicate Reference

[Full list of predicates with descriptions]

### Appendix B: Sample Clinical Vignette

```json
{
  "id": "vignette_001",
  "clinical_text": "...",
  "demographics": {...},
  "expected_diagnoses": [...]
}
```

### Appendix C: Extraction Prompt Templates

[System and user prompts used for LLM extraction]

### Appendix D: Sample Diagnostic Output

[Example full_diagnosis/3 output with explanation]

---

## Marking Criteria Checklist

Use this to verify coverage before submission:

| Criterion | Marks | Report Section | Covered? |
|-----------|-------|----------------|----------|
| Problem appropriateness and complexity | 10 | Sections 1-4 | [ ] |
| Report clarity and professional presentation | 5 | Throughout | [ ] |
| Code implementation and functionality | 30 | Sections 5-8 | [ ] |
| AI technique selection and application | 20 | Sections 6-8 | [ ] |
| Quantitative evaluation with appropriate metrics | 20 | Section 9.1-9.2 | [ ] |
| Comparative analysis and trade-off discussion | 15 | Sections 9.3-9.4, 10 | [ ] |
| **Total** | **100** | | |

---

## Learning Outcomes Checklist

| Learning Outcome | Evidence | Section |
|------------------|----------|---------|
| LO2: Search techniques and knowledge representation | A* search + Prolog KB | 6, 7 |
| LO3: Learning techniques (supervised, unsupervised, RL) | LLM integration | 8 |
| LO4: Appropriate AI strategies for use cases | Comparative analysis | 9.3-9.4, 10 |

---

## Word Count Targets

| Section | Target Words | Actual Words |
|---------|--------------|--------------|
| Abstract | 150 | |
| Part A (Sections 1-4) | 800-1000 | |
| Section 5 (Architecture) | 600 | |
| Section 6 (Knowledge Rep) | 800 | |
| Section 7 (Search) | 600 | |
| Section 8 (ML Integration) | 500 | |
| Section 9 (Evaluation) | 1000 | |
| Section 10 (Reflection) | 500 | |
| Section 11 (Conclusion) | 200 | |
| **Total** | **4,000-5,000** | |
