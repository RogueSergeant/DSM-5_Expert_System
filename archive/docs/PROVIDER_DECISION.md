# LLM Provider Decision: OpenAI vs Ollama

> **Status**: Final Decision
> **Date**: January 2026
> **Author**: Alfie Roberts

## Executive Summary

**All tests, deployments, and analyses in this project use OpenAI exclusively.**

This decision is driven by practical time constraints for academic delivery. However, **for production deployment in a real clinical setting, I strongly recommend self-hosted Ollama** for data privacy and regulatory compliance.

---

## Decision

### For This Academic Project: OpenAI Only

**Rationale**:

1. **Time Constraints**: Sequential Ollama baseline takes **7.5 hours** for 10 vignettes
   - Projected: 83+ hours for full 111-vignette benchmark
   - Academic deadline does not permit this runtime

2. **Development Velocity**: OpenAI enables rapid iteration
   - ~30 minutes per full benchmark run
   - Multiple experiments per day vs. days per experiment

3. **Cost**: Acceptable for academic scope
   - Estimated £5-10 per full 111-vignette benchmark
   - Total project cost <£100 for all experiments

4. **Reliability**: Proven API performance
   - Consistent JSON parsing
   - Minimal prompt engineering required
   - Well-documented error handling

**Scope**: This applies to:
- All benchmark runs ([benchmark.py](../src/evaluation/benchmark.py))
- Batch vs sequential experiments ([run_batch_experiment.py](../run_batch_experiment.py))
- Pure LLM baseline comparisons ([run_pure_llm_baseline.py](../run_pure_llm_baseline.py))
- Final evaluation for submission
- All analyses in [submission_notebook.ipynb](../submission_notebook.ipynb)

---

## Production Deployment Recommendation

### For Real Clinical Settings: Self-Hosted Ollama

If this system were deployed in a real healthcare environment, I **strongly recommend** using Ollama on infrastructure controlled by the health centre:

**Why Ollama for Production?**

1. **Data Privacy**: Patient data never leaves the organisation
   - No external API calls
   - Complete control over data flow
   - No third-party data processing agreements required

2. **Regulatory Compliance**:
   - GDPR: Data processing within EU/UK jurisdiction
   - NHS Data Security and Protection Toolkit compliance
   - HIPAA compliance (US) - on-premises processing
   - No reliance on external vendor's data policies

3. **Cost Model**: One-time hardware investment vs per-query fees
   - No ongoing API costs
   - Predictable infrastructure budget
   - Cost scales with hardware, not usage

4. **Autonomy**: No dependency on external service availability
   - No rate limits
   - No API deprecation risks
   - Complete version control

**Implementation Strategy**:

- **Hardware**: GPU server (e.g., NVIDIA A100, RTX 4090)
- **Model**: `qwen2.5:32b-instruct` or similar medical-capable model
- **Deployment**: Docker container on secure hospital network
- **Workflow**: Batch processing for non-urgent assessments
- **Fallback**: Human clinician review for uncertain cases

**Mitigating Speed Constraints**:

Whilst Ollama is slower than OpenAI API, this is manageable in production:
- Use batch processing overnight for routine screenings
- Prioritise immediate cases for human clinician assessment
- Pre-compute common diagnostic pathways
- Optimise with quantised models (8-bit inference) on appropriate hardware

---

## Comparative Performance

### Benchmark Runtime Comparison

| Provider | 10 Vignettes (Sequential) | 111 Vignettes (Projected) |
|----------|--------------------------|---------------------------|
| **OpenAI** (gpt-4o-mini, high reasoning) | ~30 minutes | ~5.5 hours |
| **Ollama** (qwen2.5:32b-instruct) | ~7.5 hours | ~83 hours |

**Speedup**: OpenAI is **15x faster** than Ollama for this workload

### Cost Comparison (111 Vignettes)

| Provider | Cost per Benchmark | Annual Cost (52 runs) |
|----------|-------------------|----------------------|
| **OpenAI** | ~£8 | ~£416 |
| **Ollama** (hardware) | £0 (after setup) | £0 (electricity only) |

**Hardware Investment**: ~£3000-5000 one-time (GPU server)
**Break-even**: ~7-12 months of weekly benchmarking

---

## Implications for This Project

### What This Means for the Submission

1. **All results use OpenAI** as the LLM provider
2. **Ollama results are not included** in final analyses
3. **Provider evaluation** ([PROVIDER_EVALUATION.md](./PROVIDER_EVALUATION.md)) documents Ollama capabilities but is not used for main experiments
4. **Reproducibility**: Readers can reproduce results with OpenAI API key (costs apply)

### Limitations Acknowledged

- **Generalisability**: Results may not transfer perfectly to Ollama models
- **Data Privacy**: OpenAI usage not suitable for real patient data
- **External Dependency**: Relies on OpenAI API availability

### Mitigation

- Document system architecture to be **provider-agnostic**
- Use abstracted `ExtractionProvider` interface
- Provide clear guidance for swapping providers in production
- Validate Prolog extraction quality across providers in [PROVIDER_EVALUATION.md](./PROVIDER_EVALUATION.md)

---

## References

- Provider evaluation: [PROVIDER_EVALUATION.md](./PROVIDER_EVALUATION.md)
- Extraction pipeline: [run_extraction.py](../src/extraction/run_extraction.py)
- Benchmark implementation: [benchmark.py](../src/evaluation/benchmark.py)
- Configuration: [config.py](../src/extraction/config.py)
