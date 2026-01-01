# DSM Version Notes

## Issue Discovered

**Date**: 2025-12-19

The diagnostic criteria text files in `data/dsm5_text/` were originally sourced from **DSM-5 (2013)** rather than the current **DSM-5-TR (2022)**. This document outlines the differences and their impact on the Prolog knowledge base.

## Background

- **DSM-5**: Published May 2013
- **DSM-5-TR**: Published March 2022 (Text Revision)

The DSM-5-TR was primarily a **text revision**, updating descriptive content, prevalence data, and cultural considerations. Most diagnostic criteria remained unchanged, with only minor clarifications.

## Impact Analysis by Disorder

### ASD (Autism Spectrum Disorder)

**Status**: Minor criteria clarification needed

| Component | DSM-5 | DSM-5-TR | Impact |
|-----------|-------|----------|--------|
| Criterion A wording | "as manifested by the following" | "as manifested by **all of** the following" | Clarification only - original intent was always "all of" |
| Severity specifier | "Associated intellectual disability disorder" | "Associated intellectual developmental problem" | Terminology update |

**Action Required**: Update ASD text file and Prolog to reflect "all of" clarification.

### ADHD (Attention-Deficit/Hyperactivity Disorder)

**Status**: No criteria changes

The DSM-5-TR contains only text updates:
- Updated heritability estimates
- New neuroimaging findings
- Refined prevalence data

**Action Required**: None for Prolog criteria. Consider adding version comment.

### MDD (Major Depressive Disorder)

**Status**: No core criteria changes

Changes in DSM-5-TR:
- "Mixed features" specifier clarified (now explicitly excludes if full manic/hypomanic criteria met)
- Prolonged Grief Disorder added as separate diagnosis (not MDD)
- "Unspecified Mood Disorder" category restored

**Action Required**: None for core MDD Prolog. Mixed features specifier could be updated if implemented.

### GAD (Generalised Anxiety Disorder)

**Status**: No criteria changes

The DSM-5-TR contains only text updates to prevalence, risk factors, and cultural considerations.

**Action Required**: None for Prolog criteria. Consider adding version comment.

### PTSD (Post-Traumatic Stress Disorder)

**Status**: No criteria changes for adults

| Population | Change |
|------------|--------|
| Adults | None |
| Children ≤6 years | Redundant note about electronic media exposure removed from Criterion A.2 |

**Action Required**: None for adult PTSD Prolog. If implementing child-specific criteria, update A.2.

## Summary Table

| Disorder | Criteria Changed? | Action Needed |
|----------|------------------|---------------|
| ASD | Minor clarification | Update "all of" wording |
| ADHD | No | Version comment only |
| MDD | No (specifiers clarified) | Version comment only |
| GAD | No | Version comment only |
| PTSD | No (adults) | Version comment only |

## Recommendations

1. **Update source text files** to reflect DSM-5-TR where applicable
2. **Add version headers** to Prolog files indicating DSM-5-TR (2022) reference
3. **ASD specifically** needs the Criterion A clarification applied
4. **Document in CLAUDE.md** that DSM-5-TR is the reference standard

## References

- American Psychiatric Association. (2022). *Diagnostic and Statistical Manual of Mental Disorders* (5th ed., text rev.). https://doi.org/10.1176/appi.books.9780890425787
- First, M. B., et al. (2022). DSM-5-TR: Overview of what's new and what's changed. *World Psychiatry*, 21(2), 218-219.
