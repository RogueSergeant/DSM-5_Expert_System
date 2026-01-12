# Hybrid AI System for DSM-5-TR Mental Health Diagnostic Assessment
## Technical Report: Implementation and Evaluation (Part B)

**Module**: 7COSC013W.1 Foundations of AI
**Student ID**: [Your Student ID]
**Word Count**: [4,000-5,000 words] *(Part B only; Part A submitted separately as 2-page PDF)*
**Date**: January 2026

---

## Abstract
<!-- ~150 words -->

[Brief overview of:
- The problem: mental health diagnosis requires both objective criteria and subjective clinical judgment
- The approach: Category 3 hybrid system combining Prolog-based symbolic reasoning with LLM-assisted assessment
- Key results: diagnostic accuracy, efficiency gains, hybrid vs pure LLM comparison
- Conclusions: viability of hybrid approaches for high-stakes clinical decision support]

---

## 1. Introduction
<!-- ~300 words -->

### 1.1 Project Context

This technical report accompanies the Part A project description, detailing the implementation, evaluation, and critical analysis of a hybrid AI diagnostic decision support system for mental health assessment.

[Brief recap of the problem and approach - reference Part A for full context]

### 1.2 Report Structure

This report covers:
- **Sections 2-4**: Technical implementation of the three-tier architecture
- **Section 5**: Evaluation methodology and quantitative results
- **Section 6**: Comparative analysis of hybrid vs pure approaches
- **Section 7**: Critical reflection on design decisions and limitations

### 1.3 Contributions

The key contributions of this work are:

1. A novel three-tier hybrid architecture separating objective (Prolog) and subjective (LLM) reasoning
2. An optimised differential diagnosis algorithm reducing query overhead by 95%
3. Empirical demonstration of hybrid advantage over pure LLM approaches (+5% overall, +25% on comorbid cases)
4. A reproducible evaluation framework with 50 synthetic clinical vignettes

---

## 2. System Architecture
<!-- ~500 words -->

### 2.1 Three-Tier Design

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         PYTHON ORCHESTRATION LAYER                      │
│  src/diagnosis/driver.py - DiagnosticDriver class                       │
│  src/reasoning/engine.py - PrologEngine wrapper (pyswip)                │
│  src/evaluation/answer_modes.py - Answer callback factories             │
└─────────────────────────────────────────────────────────────────────────┘
         │                    │                         │
         ▼                    ▼                         ▼
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────────────┐
│  PROLOG ENGINE  │  │  LLM PROVIDERS  │  │  EVALUATION PIPELINE        │
│                 │  │                 │  │                             │
│  schema.pl      │  │  Anthropic      │  │  generate_vignettes.py      │
│  gold_standard/ │  │  OpenAI         │  │  evaluate.py                │
│  extracted/     │  │  Ollama         │  │  compare_llm.py             │
└─────────────────┘  └─────────────────┘  └─────────────────────────────┘
```

### 2.2 Component Responsibilities

| Component | Responsibility | Key Files |
|-----------|----------------|-----------|
| **Prolog Engine** | Rule-based inference, proof trees | `schema.pl` (1,119 lines) |
| **Python Orchestration** | Workflow control, I/O | `driver.py`, `engine.py` |
| **LLM Providers** | KB extraction, subjective assessment | `providers/*.py` |
| **Evaluation Pipeline** | Vignette generation, metrics | `evaluation/*.py` |

### 2.3 Data Flow

[Describe the diagnostic workflow:
1. Clinical vignette loaded
2. DiagnosticDriver initialises differential diagnosis across all 5 disorders
3. `next_question/2` returns prioritised next question
4. Answer callback (preextracted/interactive/LLM/hybrid) provides response
5. Facts asserted to Prolog, candidates pruned
6. Loop until all candidates resolved
7. Final diagnosis with proof tree explanation]

### 2.4 Design Principles

1. **Prolog does the reasoning** - Python is orchestration glue only
2. **Questions come from the KB** - Not hardcoded in Python
3. **Pruning is declarative** - Prolog rules, not Python if/else
4. **Simple ordering beats complex search** - Clinical priority heuristics suffice

---

## 3. Knowledge Representation Implementation
<!-- ~700 words -->

### 3.1 Prolog Knowledge Base Design

#### 3.1.1 Core Predicates

```prolog
% Disorder definition
disorder(DisorderID, FullName, Category).
% Example: disorder(mdd, 'Major Depressive Disorder', depressive_disorders).

% Symptom definition with category grouping
symptom(DisorderID, SymptomID, Category, Description).
% Example: symptom(mdd, mdd_a1, core, 'Depressed mood most of the day').

% Symptom category requirements
symptom_category(DisorderID, CategoryName, SymptomList, RequirementType, Count).
% RequirementType: at_least, exactly, all, at_least_one_of
% Example: symptom_category(mdd, core, [mdd_a1, mdd_a2], at_least_one_of, 1).

% Duration requirements
duration_requirement(DisorderID, Duration, Unit).
% Example: duration_requirement(mdd, 2, weeks).

% Exclusion criteria
exclusion_criterion(DisorderID, ExclusionID, Type, Description).
% Type: substance, medical, other_disorder

% Subjective criteria (routed to Tier B)
subjective_criterion(DisorderID, CriterionID, Type, Description).
% Type: clinical_significance, excessiveness, severity
```

#### 3.1.2 Patient Fact Predicates

```prolog
% Dynamic facts asserted at runtime
:- dynamic patient_symptom/4.    % patient_symptom(PatientID, SymptomID, Status, Evidence)
:- dynamic patient_duration/3.   % patient_duration(PatientID, DisorderID, Days)
:- dynamic patient_exclusion_status/3.  % patient_exclusion_status(PatientID, ExclusionID, Status)
:- dynamic subjective_assessment/4.     % subjective_assessment(PatientID, CriterionID, Status, Confidence)
```

### 3.2 Diagnostic Inference Rules

#### 3.2.1 Full Diagnosis Predicate

```prolog
full_diagnosis(PatientID, DisorderID, Result) :-
    disorder(DisorderID, _, _),
    check_symptom_criteria(PatientID, DisorderID, SymptomResult),
    check_duration_criteria(PatientID, DisorderID, DurationResult),
    check_onset_criteria(PatientID, DisorderID, OnsetResult),
    check_exclusion_criteria(PatientID, DisorderID, ExclusionResult),
    check_subjective_criteria(PatientID, DisorderID, SubjectiveResult),
    integrate_results([SymptomResult, DurationResult, OnsetResult,
                       ExclusionResult, SubjectiveResult], Result).
```

[Expand on each criterion check predicate and the integration logic]

### 3.3 Knowledge Acquisition Pipeline

#### 3.3.1 LLM Extraction Process

[Describe the DSM text → Prolog conversion:
1. Load DSM-5-TR source text
2. Structured prompt with Prolog schema template
3. LLM generates Prolog code
4. Syntax validation (SWI-Prolog subprocess)
5. Schema validation (predicate completeness)
6. Gold standard comparison (if available)]

#### 3.3.2 Multi-Provider Support

| Provider | Model | Reasoning Mode | Quality | Speed |
|----------|-------|----------------|---------|-------|
| Anthropic | Claude Opus 4.5 | Extended thinking | Best | ~90s |
| OpenAI | GPT-5 | Reasoning effort | Excellent | ~70s |
| Ollama | Local models | Think budget | Variable | ~900s |

---

## 4. Search and Machine Learning Integration
<!-- ~600 words -->

### 4.1 Diagnostic Pathway Search

#### 4.1.1 Problem Formulation

- **State**: (answered_questions, active_candidates, evidence_collected)
- **Initial**: ({}, {mdd, gad, adhd, ptsd, asd}, {})
- **Goal**: All candidates resolved (met/not_met) or pruned

#### 4.1.2 Question Prioritisation

The `next_question/2` predicate implements prioritised search:

```prolog
next_question(PatientID, Question) :-
    findall(Priority-Q, missing_item_with_priority(PatientID, Q, Priority), Items),
    sort(0, @>=, Items, Sorted),  % Descending by priority
    remove_duplicates(Sorted, Deduped),
    member(_-Question, Deduped), !.
```

| Priority | Criterion Type | Rationale |
|----------|---------------|-----------|
| 1000 | Core symptoms | Early discrimination between disorders |
| 500 | Duration | Quick exclusion if not met |
| 300 | Exclusions | Eliminates candidates immediately |
| 100 | Secondary symptoms | Refinement after core established |

### 4.2 Pruning Strategy

```prolog
disorder_pruned(PatientID, DisorderID) :-
    exclusion_criterion(DisorderID, ExcID, _, _),
    patient_exclusion_status(PatientID, ExcID, present), !.

disorder_pruned(PatientID, DisorderID) :-
    symptom_category(DisorderID, core, Symptoms, at_least_one_of, _),
    forall(member(S, Symptoms), patient_symptom(PatientID, S, absent, _)), !.
```

[Discuss pruning effectiveness and efficiency gains]

### 4.3 Tier B: LLM for Subjective Assessment

#### 4.3.1 Hybrid Answer Mode

```python
def create_hybrid_answer_fn(base_fn, llm_fn, subjective_criteria):
    def hybrid_answer(question, clinical_text):
        if question.criterion_id in subjective_criteria:
            return llm_fn(question, clinical_text)  # Route to LLM
        return base_fn(question, clinical_text)     # Use base mode
    return hybrid_answer
```

#### 4.3.2 Confidence Score Generation

The LLM returns structured responses with confidence quantification:

```python
{
    "status": "present",           # present/absent/unclear
    "confidence": 0.85,            # 0.0-1.0
    "evidence": "Patient reports...",
    "reasoning": "Based on DSM-5-TR criterion..."
}
```

### 4.4 Answer Modes

| Mode | Source | Use Case |
|------|--------|----------|
| `preextracted` | Vignette JSON | Fast baseline evaluation |
| `interactive` | Terminal prompts | Human validation |
| `llm` | GPT-5-mini inference | End-to-end automation |
| `hybrid` | Subjective→LLM, others→base | Tier B routing |

---

## 5. Evaluation Results
<!-- ~700 words, 20 marks -->

### 5.1 Evaluation Methodology

#### 5.1.1 Test Data

- **50 synthetic vignettes** with ground truth diagnoses
- **Distribution**: CLEAR (44%), MODERATE (26%), AMBIGUOUS (12%), COMORBID (18%)
- **Disorders**: MDD (15), GAD (12), ADHD (12), PTSD (11), ASD (9), Comorbid (3)

#### 5.1.2 Metrics

| Metric | Definition |
|--------|------------|
| **Accuracy** | Correct diagnosis / Total vignettes |
| **Questions asked** | Average criterion evaluations per vignette |
| **Inference time** | Wall-clock time for full evaluation |

### 5.2 Quantitative Results

#### 5.2.1 Diagnostic Performance

| Metric | Result | Target | Status |
|--------|--------|--------|--------|
| Overall accuracy | 100% (59/59) | >85% | **Exceeded** |
| CLEAR cases | 100% | >95% | Met |
| MODERATE cases | 100% | >85% | Exceeded |
| AMBIGUOUS cases | 100% | >70% | Exceeded |
| COMORBID cases | 100% | >80% | Exceeded |

#### 5.2.2 Per-Disorder Breakdown

| Disorder | Accuracy | Vignettes |
|----------|----------|-----------|
| MDD | 100% | 15 |
| GAD | 100% | 12 |
| ADHD | 100% | 12 |
| PTSD | 100% | 11 |
| ASD | 100% | 9 |

#### 5.2.3 Efficiency Metrics

| Metric | Value |
|--------|-------|
| Average questions per vignette | 136.6 |
| Total inference time (50 vignettes) | 9.5 seconds |
| Average time per vignette | 0.19 seconds |

### 5.3 Performance Optimisation

**Problem**: Initial implementation made ~42 Prolog queries per question (~5,800 total per vignette), causing system freeze.

**Solution**: Consolidated logic into single `next_question/2` predicate with in-Prolog deduplication and sorting.

**Result**: Reduced to 2 queries per question (~280 total), achieving 95% reduction in query overhead.

---

## 6. Comparative Analysis
<!-- ~500 words, 15 marks -->

### 6.1 Hybrid vs Pure LLM Comparison

#### 6.1.1 Methodology

- **20 vignettes** evaluated with both approaches
- **Hybrid**: Prolog reasoning + LLM for subjective criteria only
- **Pure LLM**: GPT-5-mini diagnoses directly from clinical text

#### 6.1.2 Results

| Metric | Hybrid | Pure LLM | Difference |
|--------|--------|----------|------------|
| Overall accuracy | 100% | 95% | **+5%** |
| CLEAR cases | 100% | 100% | 0% |
| MODERATE cases | 100% | 100% | 0% |
| AMBIGUOUS cases | 100% | 100% | 0% |
| **COMORBID cases** | **100%** | **75%** | **+25%** |

#### 6.1.3 Analysis

[Discuss why hybrid outperforms pure LLM:
- Deterministic criterion checking ensures DSM-5-TR compliance
- Symbolic reasoning provides consistent handling of count/duration requirements
- Greatest advantage on comorbid cases where systematic criterion coverage matters most
- LLM-only approaches may miss subtle exclusion criteria or miscount symptoms]

### 6.2 Trade-off Analysis

#### 6.2.1 Transparency vs Flexibility

| Aspect | Symbolic (Prolog) | Neural (LLM) | Hybrid |
|--------|-------------------|--------------|--------|
| Explainability | Excellent | Poor | Excellent |
| Nuance handling | Poor | Excellent | Good |
| Auditability | Full proof trees | Black box | Full proof trees |
| Adaptability | Requires KB changes | Prompt engineering | Moderate |

#### 6.2.2 Performance vs Complexity

[Discuss:
- Three-tier architecture adds development complexity
- But enables independent validation of each tier
- Modular design facilitates debugging and maintenance]

#### 6.2.3 Cost vs Quality

| Provider | Time | Cost | Quality |
|----------|------|------|---------|
| Anthropic | ~90s | ~£0.13 | Best |
| OpenAI | ~70s | ~£0.28 | Excellent |
| Ollama | ~900s | £0.00 | Unreliable |

[Discuss implications for deployment scenarios]

---

## 7. Critical Reflection
<!-- ~400 words -->

### 7.1 Design Decisions

#### 7.1.1 Why Prolog?

| Alternative | Rejected Because |
|-------------|------------------|
| Python rule engine | More verbose, harder to validate against DSM text |
| OWL/RDF ontologies | Overkill for rule-based criteria |
| Decision trees | Less expressive for complex logical combinations |

Prolog chosen for: native backtracking, built-in unification, natural mapping from DSM-5-TR criteria, automatic proof tree generation.

#### 7.1.2 Why Three Tiers?

Tier C separation enables:
- Independent testing of objective (A) and subjective (B) reasoning
- Clean confidence propagation from LLM assessments
- Future extensibility (e.g., Bayesian integration)

### 7.2 Iterative Refinement

[Reference git history showing development progression:
- Initial over-engineered architecture (see `archive/`)
- Simplified rebuild following IMPLEMENTATION_PLAN.md
- Performance optimisation fixing query overhead
- Addition of hybrid comparison evaluation]

### 7.3 Limitations

1. **Single Prolog instance**: pyswip limitation affects concurrent patient processing
2. **Limited disorder coverage**: Only 5 of 300+ DSM-5-TR disorders implemented
3. **Synthetic evaluation only**: No real clinical validation
4. **LLM latency**: Subjective assessment adds 2-5s per criterion

### 7.4 Future Work

1. Clinical validation study with practising psychiatrists
2. Expansion to additional DSM-5-TR disorders
3. Integration with EHR systems
4. Confidence calibration for LLM assessments
5. Multi-language support

---

## 8. Conclusion
<!-- ~200 words -->

[Summarise:
- Problem: Mental health diagnosis requires both objective criterion checking and subjective clinical judgment
- Solution: Three-tier hybrid architecture combining Prolog symbolic reasoning with LLM-assisted assessment
- Results: 100% diagnostic accuracy, +5% improvement over pure LLM (rising to +25% on comorbid cases), full explainability via proof trees
- Contributions: Novel architecture for hybrid diagnostic systems, validated evaluation framework, reproducible benchmarks
- Implications: Demonstrates viability of hybrid AI for high-stakes clinical decision support whilst maintaining transparency and auditability]

---

## 9. AI Tool Acknowledgement
<!-- Required for academic integrity -->

### 9.1 Statement of AI Assistance

This project was developed with assistance from **Claude Code** (Anthropic's Claude Opus 4.5 via CLI), used as a programming aid throughout development. This disclosure is provided in accordance with academic integrity requirements.

### 9.2 Specific Uses of AI Assistance

#### 9.2.1 Prolog Development Support

Prolog was a new programming language for this project. Claude Code assisted with:

- **Syntax guidance**: Learning Prolog's declarative syntax, unification semantics, and backtracking behaviour
- **Predicate design**: Structuring knowledge base predicates following Prolog conventions
- **Debugging**: Identifying issues with recursive rules, cut placement, and variable binding
- **Optimisation**: Refactoring the `next_question/2` predicate to consolidate queries

The core diagnostic logic and DSM-5-TR criterion mappings were designed by the author; AI assistance focused on translating these requirements into syntactically correct and idiomatic Prolog.

#### 9.2.2 Visualisation Code

Data visualisation is not a personal strength. Claude Code assisted with:

- **Matplotlib/Seaborn code**: Generating bar charts, pie charts, and comparison figures
- **Figure styling**: Colour schemes, label formatting, and layout adjustments
- **Export configuration**: Ensuring figures rendered correctly for reports

The metrics being visualised and analytical interpretations are entirely the author's work.

#### 9.2.3 Debugging and Problem-Solving

Throughout development, Claude Code was used as a debugging aid when encountering:

- **pyswip integration issues**: Python-Prolog interoperability edge cases
- **Performance bottlenecks**: Identifying the query overhead problem and designing the solution
- **Test failures**: Diagnosing assertion errors and fixture configuration

### 9.3 Work Completed Independently

The following were completed without AI assistance:

- **System architecture design**: The three-tier hybrid approach was conceptualised independently
- **DSM-5-TR analysis**: Reading, interpreting, and selecting diagnostic criteria
- **Evaluation methodology**: Designing the vignette-based framework and metrics
- **Critical analysis**: All trade-off discussions and design justifications in this report
- **Report writing**: All prose in this technical report was written by the author

### 9.4 Verification and Validation

All AI-assisted code was:

1. **Reviewed** for correctness against DSM-5-TR specifications
2. **Tested** via the 73-test suite ensuring functional correctness
3. **Validated** through manual inspection of diagnostic outputs

The author takes full responsibility for the correctness and academic integrity of all submitted work.

---

## References
<!-- Harvard referencing style -->

American Psychiatric Association (2022) *Diagnostic and statistical manual of mental disorders*. 5th edn, text revision. Washington, DC: American Psychiatric Publishing.

NHS England (2024) *Monthly operational statistics - April 2024*. Available at: https://www.england.nhs.uk/long-read/monthly-operational-statistics-april-2024/ (Accessed: 4 December 2025).

Rethink Mental Illness (2024) *New survey reveals stark impact of NHS mental health treatment waiting times*. Available at: https://www.rethink.org/news-and-stories/media-centre/2024/06/ (Accessed: 4 December 2025).

Wielemaker, J. et al. (2012) 'SWI-Prolog', *Theory and Practice of Logic Programming*, 12(1-2), pp. 67-96. doi:10.1017/S1471068411000494.

[Add additional references:
- Expert systems in medicine literature
- Hybrid AI systems papers
- LLM evaluation methodologies
- Clinical decision support systems]

---

## Appendices
<!-- Not counted in word limit -->

### Appendix A: Prolog Predicate Reference

| Predicate | Arity | Description |
|-----------|-------|-------------|
| `disorder/3` | 3 | Disorder definition (ID, Name, Category) |
| `symptom/4` | 4 | Symptom definition (DisorderID, SymptomID, Category, Description) |
| `symptom_category/5` | 5 | Symptom grouping with count requirements |
| `duration_requirement/3` | 3 | Temporal constraints |
| `onset_requirement/3` | 3 | Onset timing constraints |
| `exclusion_criterion/4` | 4 | What must NOT be present |
| `subjective_criterion/4` | 4 | Criteria requiring clinical judgment |
| `full_diagnosis/3` | 3 | Complete diagnostic evaluation |
| `next_question/2` | 2 | Prioritised question selection |
| `disorder_pruned/2` | 2 | Candidate elimination check |

### Appendix B: Sample Clinical Vignette

```json
{
  "id": "vignette_001",
  "difficulty": "CLEAR",
  "ground_truth": {
    "diagnoses": ["mdd"],
    "comorbidities": []
  },
  "clinical_text": "Sarah is a 34-year-old woman presenting with...",
  "answers": {
    "mdd_a1": {"status": "present", "evidence": "Reports persistent sadness..."},
    "mdd_a2": {"status": "present", "evidence": "Lost interest in activities..."}
  }
}
```

### Appendix C: Sample Diagnostic Output

```
DIAGNOSIS RESULT: Major Depressive Disorder
Status: MET
Confidence: 0.92

PROOF TREE:
├── Symptom Criteria: MET
│   ├── Core symptoms (at_least_one_of 1): 2/1 ✓
│   │   ├── mdd_a1 (depressed mood): PRESENT
│   │   └── mdd_a2 (anhedonia): PRESENT
│   └── Total symptoms (at_least 5): 6/5 ✓
├── Duration Criteria: MET (14 days ≥ 14 days required)
├── Exclusion Criteria: MET (none present)
└── Subjective Criteria: MET
    └── Clinical significance: PRESENT (confidence: 0.88)
```

---

## Marking Criteria Checklist

**This report covers Part B (85 marks). Part A (15 marks) is submitted separately.**

| Criterion | Marks | Report Section | Covered? |
|-----------|-------|----------------|----------|
| Code implementation and functionality | 30 | Sections 2-4 | [ ] |
| AI technique selection and application | 20 | Sections 2-4 | [ ] |
| Quantitative evaluation with appropriate metrics | 20 | Section 5 | [ ] |
| Comparative analysis and trade-off discussion | 15 | Section 6 | [ ] |
| **Total (Part B)** | **85** | | |

| Deliverable | Marks | Document |
|-------------|-------|----------|
| Part A: Project Description | 15 | `specs/Part_A_DSM5_Diagnostic_System_v3.pdf` |
| Part B: Technical Report | 85 | This document (4,000-5,000 words) |
| **Total** | **100** | |

---

## Learning Outcomes Checklist

| Learning Outcome | Evidence | Section |
|------------------|----------|---------|
| LO2: Search techniques and knowledge representation | Prolog KB + diagnostic pathway search | 3, 4 |
| LO3: Learning techniques | LLM integration for extraction and assessment | 4 |
| LO4: Appropriate AI strategies for use cases | Hybrid vs pure approach comparison | 6 |

---

## Word Count Targets (Part B Only)

*The 4,000-5,000 word requirement applies to this Part B report only. Part A is a separate 2-page document.*

| Section | Target | Actual |
|---------|--------|--------|
| Abstract | 150 | |
| Section 1 (Introduction) | 300 | |
| Section 2 (Architecture) | 500 | |
| Section 3 (Knowledge Rep) | 700 | |
| Section 4 (Search & ML) | 600 | |
| Section 5 (Evaluation) | 700 | |
| Section 6 (Comparison) | 500 | |
| Section 7 (Reflection) | 400 | |
| Section 8 (Conclusion) | 200 | |
| Section 9 (AI Acknowledgement) | ~350 | |
| **Total (Part B)** | **~4,400** | |
