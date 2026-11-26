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
| D5 | Headline rule (amended 2026-11-18) | The primary result is the outcome table: one outcome class per main-comparison proxy and per window (main and extension), for each analysis. Differences between proxies and between windows are expected and are reported as findings. A conclusion is stated generally only if every cell of the table has the same outcome class; otherwise it is stated per construct and, where it applies, per window. See "Reporting the outcome table" below | Prevents choosing the convenient proxy or window; the exploratory previews already differ by proxy at the spatial scale |
| D6 | Decision metric | See "Decision metric" below, including a "no clear dominance" outcome | The rule cannot be applied without a definition of "holds" |
| D7 | Venue | Stay R1 of COMMSENV-25-2408, subject to the editor agreeing the timeline | The design answers Reviewer 2 (what SPD represents) and Reviewer 3 (early land use) directly |
| D8 | Window | Main results: common cohort in the common 8.0–2.0 ka window. Extension: each proxy's full data over its native window | The common cohort is the only comparison in which the proxy is the only thing that varies. This reverses the plan's original order |
| D9 | Whole-Holocene claim (amended 2026-11-26) | Promoted to a main result: HYDE 3.5 land use over its native window, 0.5–11.5 ka. Its zero values before the onset of agriculture are reported as estimates, not missing data. See "Early-Holocene HYDE 3.5 land use" below | HYDE 3.5 models the whole period: it places population in every layer from 10,000 BCE and dates the onset of agriculture from radiocarbon data (Eurasia) and ArchaeoGLOBE (elsewhere), so zero land use is an estimate of absence. No human land use is consistent with unchanged vegetation |
| D10 | Climate representation (amended 2026-11-19) | Main: shared, response-free filtered selection (`max_cor = 0.8`, `max_vif = 5`), in every run. Sensitivity: climate principal components only. The all-four-variables sensitivity is dropped | Needed for degrees of freedom. Test A measured the effect of the reduction: it moves the balance slightly towards human but climate leads in both arms at both scales (`results_human_proxy_design_diagnostics_2026-11-19.md`) |
| D11 | SPD scale | Square-root SPD in every run | All existing sensitivity runs already use it |
| D12 | Residual-df minimum (confirmed 2026-11-19) | Five | Matches canonical R1. Under the filtered climate selection, five loses 18 of 1175 records of the 2.0–8.0 ka common cohort compared with four, and every region keeps at least 33 records (`results_human_proxy_design_diagnostics_2026-11-19.md`) |
| D13 | Co-author sign-off | Proceed on the agreement received; remaining co-authors get a dated window in the gate issue, after which silence counts as consent | The work cannot wait indefinitely |
| D14 | Spatial support of the gridded proxies (added 2026-11-26) | Main: KK10 and HYDE 3.5 averaged over each record's SPD buffer, the same area as SPD. Sensitivity: a 50 km radius around the record. This replaces the plan's design, in which the record's cell and small radii were the main extraction and the SPD buffer was the scale-matched sensitivity | Every proxy then shares one spatial scale, so the proxy is the only thing that varies between models. The 50 km radius is closer to the pollen-source area, and few records lack a value there (4 for KK10, 6 for HYDE 3.5, against 78 and 34 at the record's cell) |

## Decision metric

- **Spatial H1:** the overall dbMEM-adjusted human-minus-climate balance and its 95 % bootstrap interval. A hypothesis is supported if the interval lies above zero, rejected in favour of climate if it lies below zero, and reported as "no clear dominance" if it includes zero.
- **Temporal H1:** the pooled human-minus-climate allocation across estimable continental-region-age units and its 95 % interval, read the same way. The count of units with human greater than climate is reported alongside it.
- **H2:** the analysis-level human-minus-climate allocation across estimable groups, read the same way.
- "Holds for every proxy" means the same outcome class (human, climate, or no clear dominance) for SPD, KK10, and HYDE 3.5 land use. Because KK10 and HYDE are one evidence family, agreement among all three is agreement between two families, and is described as such.
- The zero-truncated profile is the headline profile; signed and exclude-negative profiles are reported next to it, and a conclusion that changes outcome class between profiles is flagged.

## Reporting the outcome table

Added 2026-11-18, before any confirmatory run, with the amendment of D5.

- For each analysis (spatial H1, temporal H1, H2) the results are reported as a table of outcome classes: rows are SPD, KK10, and HYDE 3.5 land use; columns are the main window (common cohort, 8.0–2.0 ka) and the extension (each proxy's native window).
- A general statement requires every cell to share one outcome class. This is not the expected outcome; the per-construct and per-window statements are the default.
- When proxies differ, the comparison is reported by construct: demography (SPD) against land use (KK10 and HYDE). If KK10 and HYDE differ from each other, land use is reported as dependent on the early land-use scenario, not as one class.
- When the main window and the extension differ for a proxy, the difference is reported as a difference between the two windows. The extension differs from the main window in both time span and record set, so it is not interpreted as a change in the balance through time.
- A claim that the balance changes through time is made only from comparisons on the same records: the age units of temporal H1, or the common cohort split into periods if that is needed.

## Gating tests, run before the Phase 3 build

Both use existing outputs and extractions and are methods diagnostics, not confirmatory runs.

- **Test A, climate reduction:** fit the SPD model with all four climate variables and with the filtered selection on identical sequences and ages. Report how far the reduction alone moves the human-minus-climate balance.
- **Test B, trend sharing:** in spatial H1, quantify the human ∩ time fraction for KK10 and HYDE and refit with a detrended version of each proxy. Within a record KK10 is a smooth, trend-like curve, so its share may be variation shared with time.

How test B sets the scope:

- If the gridded-proxy share collapses after detrending, climate leads for every representation, and the manuscript stays close to the current R1 with the single-proxy runs as a robustness section.
- If it survives, the scale-divergence framing applies (change through time follows climate whatever the proxy; among-site attribution depends on the construct) and the larger rewrite goes ahead.

The single-proxy H1 engine is built in either case.

Outcome, decided by Ondřej on 2026-11-19 (results: `results_human_proxy_design_diagnostics_2026-11-19.md`; reading rule posted in #348 before the run):

- Under the posted rule, the KK10 human share collapses after detrending, HYDE 3.2 population is partial, and SPD loses a larger share than KK10, so the trend sharing is common to all proxies.
- Scope: the manuscript stays close to the current R1, with the single-proxy runs as a robustness section.
- The main analysis uses the original, not detrended, proxies. The detrended refits are reported as a supplementary analysis. Detrending on linear age leaves the fitted model unchanged, because time is already a linear term in age; it only moves the variation that the proxy shares with time from human to time. It therefore shows how much of the human share overlaps with the time trend, not that this part is non-human.

## Early-Holocene HYDE 3.5 land use

Added 2026-11-26, with the amendment of D9, from the human-proxy data layer (#346; `Outputs/Tables/Data_preparation/Human_proxies/human_proxies__early_holocene_check.csv`).

- Globally, HYDE 3.5 has no land use at 11,950 BP (10,000 BCE) although it places 4.6 million people there. Land use first appears at 10,950 BP in 1,238 grid cells and grows in every later layer.
- At the SPD buffer, the fraction of the area under land use is above zero for 4 of 1,268 records at 11.0–11.5 ka, 26–30 records at 9.0–10.5 ka, 389 records at 8.0–8.5 ka, and 568 records at 6.0–6.5 ka. It exceeds 1 % for at most 3 records before 8.0 ka and for 64 records at 6.0 ka; the median is zero at every age from 11.5 to 6.0 ka.
- HYDE 3.5 has 1000-year steps before 1 CE, so all values on the 500-year grid are interpolated and their effective resolution before 2 ka is 1000 years.
- Consequence for spatial H1: at 9.0 ka and earlier, the HYDE 3.5 predictor is zero for at least 1,238 of 1,268 records at each age, so it can explain little variation among records at those ages by construction.

## Scope reductions

- SPD bootstrap resampling: not run; stated as a limitation.
- Circularity-reduced event sensitivity (PAPs recomputed without indicator taxa): not run.
- The 300-grain refit (#341): once, for H1 only, not per proxy and not for H2. Decided later on 2026-11-16: rarefaction target 300 grains with a sample minimum of 150 (following Reviewer 2; not one of the Supplementary Fig. S23 scenarios, so retention must be computed before the rebuild), SPD only, 2.0–8.0 ka, baseline and 300-grain runs compared on the records retained under both. The 62 percentage-only sequences, which the count filters cannot apply to, stay in the main 300-grain run; its result is also reported without them.
- The full validation suite runs for the three main-comparison proxies only; supplementary proxies get the core H1 run without the suite.
- Kept: the H2 refit for the three main-comparison proxies, the climate-representation, scale-matched, native-step, and transformation sensitivities, the human ∩ space and human ∩ time diagnostics, and the supplementary overlap analysis (S3).

## Order of work

1. Record the hypotheses, headline rule, and decision metric in the gate issue (#345).
2. Ask the editor about the revision timeline.
3. Restore pipeline registry and provenance consistency (#344), then run tests A and B (#348).
4. Add HYDE 3.5 to the proxy data layer (#346) and check its early-Holocene values (D9).
5. Set the scope from tests A and B, then build the single-proxy H1 runs, followed by H2 and the reduced validation set.

## Open items

- D7: the editor's answer on the timeline; no revision deadline is recorded in the repository.
- Phase 1 "verify" items in the conceptual assessment (GLUES share in KK10 before 3 ka, the Bradshaw 2004 series, HYDE 3.5 prehistoric population and time steps).
