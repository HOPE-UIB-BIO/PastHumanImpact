# Decision record: human-impact reframing (gate G1)

Date: 2026-11-16.
Decided by Ondřej after co-author feedback on the working direction; this record closes gate G1 of `Data/Plans/plan_human_impact_reframing_2026-10-23.md` and brings the venue decision (G3) forward.
Where this record and the plan, the conceptual assessment (`human_proxy_conceptual_assessment_2026-10-23.md`), or the stress test (`single_proxy_strategy_review_2026-10-23.md`) disagree, this record takes precedence.

## Disclosure

These decisions were made after the exploratory single-proxy previews had been seen (the "Current evidence" table in the plan).
The hypotheses and the headline rule are therefore fixed before the confirmatory runs, not before any results, and the manuscript Methods must say so.

## Decisions

| # | Question | Decision | Reason |
| --- | --- | --- | --- |
| D1 | Design | One human proxy per model; no joint human block | Degrees of freedom with about 13 ages per record, block-size asymmetry, mixed constructs |
| D2 | Proxy set | Main comparison: SPD, KK10, HYDE 3.5 land use. Supplementary: HYDE 3.5 population, expert events | KK10 and HYDE count as one evidence family and act as a high versus low early land-use bracket |
| D3 | Dropped from scope | ArchaeoGLOBE and the optional single-PAP analysis (H3, analysis D) | Scope |
| D4 | Hypotheses | H1a/H2a: demography explains more PAP variation than climate. H1b/H2b: land use explains more PAP variation than climate. Each H1 hypothesis is tested at both scales | Keeps the direction of the submitted hypotheses and makes each one a well-posed question |
| D5 | Headline rule | A conclusion is stated generally only if it holds for every main-comparison proxy in the main results and is not overturned in the extension runs; otherwise it is stated per construct | Prevents choosing the convenient proxy |
| D6 | Decision metric | See "Decision metric" below, including a "no clear dominance" outcome | The rule cannot be applied without a definition of "holds" |
| D7 | Venue | Stay R1 of COMMSENV-25-2408, subject to the editor agreeing the timeline | The design answers Reviewer 2 (what SPD represents) and Reviewer 3 (early land use) directly |
| D8 | Window | Main results: common cohort in the common 8.0–2.0 ka window. Extension: each proxy's full data over its native window | The common cohort is the only comparison in which the proxy is the only thing that varies. This reverses the plan's original order |
| D9 | Whole-Holocene claim | Promoted to a main result only if HYDE 3.5 land use carries information before about 6 ka rather than near-zero constants | HYDE 3.2 was near zero before about 3 ka |
| D10 | Climate representation | Main: shared, response-free filtered selection (`max_cor = 0.8`, `max_vif = 5`). Sensitivities: all four variables where residual df allow, and climate principal components | Needed for degrees of freedom; its effect on the balance must be measured (test A) |
| D11 | SPD scale | Square-root SPD in every run | All existing sensitivity runs already use it |
| D12 | Residual-df minimum | Five, provisional | Matches canonical R1. Confirm after counting the records lost under the filtered climate selection; if the loss is large, document a move to four |
| D13 | Co-author sign-off | Proceed on the agreement received; remaining co-authors get a dated window in the gate issue, after which silence counts as consent | The work cannot wait indefinitely |

## Decision metric

- **Spatial H1:** the overall dbMEM-adjusted human-minus-climate balance and its 95 % bootstrap interval. A hypothesis is supported if the interval lies above zero, rejected in favour of climate if it lies below zero, and reported as "no clear dominance" if it includes zero.
- **Temporal H1:** the pooled human-minus-climate allocation across estimable continental-region-age units and its 95 % interval, read the same way. The count of units with human greater than climate is reported alongside it.
- **H2:** the analysis-level human-minus-climate allocation across estimable groups, read the same way.
- "Holds for every proxy" means the same outcome class (human, climate, or no clear dominance) for SPD, KK10, and HYDE 3.5 land use. Because KK10 and HYDE are one evidence family, agreement among all three is agreement between two families, and is described as such.
- The zero-truncated profile is the headline profile; signed and exclude-negative profiles are reported next to it, and a conclusion that changes outcome class between profiles is flagged.

## Gating tests, run before the Phase 3 build

Both use existing outputs and extractions and are methods diagnostics, not confirmatory runs.

- **Test A, climate reduction:** fit the SPD model with all four climate variables and with the filtered selection on identical sequences and ages. Report how far the reduction alone moves the human-minus-climate balance.
- **Test B, trend sharing:** in spatial H1, quantify the human ∩ time fraction for KK10 and HYDE and refit with a detrended version of each proxy. Within a record KK10 is a smooth, trend-like curve, so its share may be variation shared with time.

How test B sets the scope:

- If the gridded-proxy share collapses after detrending, climate leads for every representation, and the manuscript stays close to the current R1 with the single-proxy runs as a robustness section.
- If it survives, the scale-divergence framing applies (change through time follows climate whatever the proxy; among-site attribution depends on the construct) and the larger rewrite goes ahead.

The single-proxy H1 engine is built in either case.

## Scope reductions

- SPD bootstrap resampling: not run; stated as a limitation.
- Circularity-reduced event sensitivity (PAPs recomputed without indicator taxa): not run.
- The 300-grain refit (#341): once, for H1 only, not per proxy and not for H2.
- The full validation suite runs for the three main-comparison proxies only; supplementary proxies get the core H1 run without the suite.
- Kept: the H2 refit for the three main-comparison proxies, the climate-representation, scale-matched, native-step, and transformation sensitivities, the human ∩ space and human ∩ time diagnostics, and the supplementary overlap analysis (S3).

## Order of work

1. Record the hypotheses, headline rule, and decision metric in the gate issue (#345).
2. Ask the editor about the revision timeline.
3. Restore pipeline registry and provenance consistency (#344), then run tests A and B (#348).
4. Add HYDE 3.5 to the proxy data layer (#346) and check its early-Holocene values (D9).
5. Set the scope from tests A and B, then build the single-proxy H1 runs, followed by H2 and the reduced validation set.

## Open items

- D12: record loss under a residual-df minimum of five.
- D7: the editor's answer on the timeline; no revision deadline is recorded in the repository.
- Phase 1 "verify" items in the conceptual assessment (GLUES share in KK10 before 3 ka, the Bradshaw 2004 series, HYDE 3.5 prehistoric population and time steps).
