# Plan: Issue #333 — Correct HVarPart Relative Importance

## Summary

Implement #333 on a dedicated
`issue/333-hvarpart-relative-importance` branch. Preserve fitted HVarPart
objects, rebuild extraction and aggregation around unmodified results, produce
signed and sensitivity summaries, regenerate affected artifacts, and prepare
insertion-ready manuscript text.

Figure 2 will first be faithfully recreated from corrected data. Its complete
redesign is deferred until human review of that recreation.

## Implementation Phases

### 1. Establish the reproducible baseline

- Record existing target/output checksums and current H1/H2 audit counts before
  changing calculations.
- Resolve or document the current `renv` mismatch without changing the lockfile
  or adding dependencies.
- Confirm that both H1/H2 manifests load in a fresh R session.

### 2. Replace overlapping importance contracts

- Make
  `compute_hvarpart_importance(data_source, id_cols, expected_predictors)` the
  canonical extractor.
- Read `Unique`, `Average.share`, `Individual`, `I.perc(%)`, and total adjusted
  R-squared directly from `varhp_output`.
- Return unmodified raw fields plus explicit eligibility and exclusion
  diagnostics.
- Add `summarise_hvarpart_importance()` with `signed`, `zero_truncated`, and
  `exclude_negative` profiles.
- Add a model-level audit summarizer that avoids double-counting predictor
  rows.
- Remove the overlapping `extract_hvar_importance()` and
  `get_summary_tables()` functions after migrating their consumers.
- Do not change the fitting functions or invalidate expensive fitted targets.

### 3. Add downstream targets and traceable outputs

- Add downstream extraction, audit, and profile-comparison targets to the
  existing H1 and H2 pipelines.
- Tag results by `spatial_spd`, `spatial_events`, `temporal_spd`,
  `temporal_events`, and `h2_spd`.
- Export predictor-level raw components, model-level audit counts, and
  profile-comparison summaries.
- Keep issue #322's broader variance-decomposition work outside this change.

### 4. Recreate affected figures

- Move HVarPart plotting logic from analytical scripts into documented, tested
  functions under `R/functions/`.
- Recreate Figure 2 with its current composition and corrected signed values.
- Use a shared central linear range, mark truncated tails, and add a full-range
  supplementary figure.
- Make temporal H1 and H2 displays safe for signed allocations.
- Regenerate the established core examples and PAP-collinearity outputs.
- Stop before a complete Figure 2 redesign and present the recreation for
  review.

### 5. Reconcile scientific prose

- Add a dedicated reviewer-response Quarto document driven by exported tables.
- Update captions and internal notes to describe signed allocations correctly.
- Create an insertion-ready manuscript patch with traceable Methods, Results,
  captions, limitations, and reviewer-response text.
- Document whether either sensitivity profile changes the main conclusion.

## Final Acceptance

- No arbitrary positive replacement of negative HVarPart contributions remains.
- Focused tests and the full project test suite pass.
- H1/H2 manifests and downstream targets succeed in fresh R sessions.
- Corrected Figure 2 is reviewed before any complete redesign begins.
- A draft PR records outputs, checks, provenance, and conclusion comparisons.

## Assumptions

- One dedicated issue branch and draft PR contains the work.
- No new R dependencies are required.
- Existing external H1/H2 target stores remain available.
- The main manuscript source is external, so repository delivery is an
  insertion-ready patch.
- The complete Figure 2 redesign is a separate post-recreation stage.
