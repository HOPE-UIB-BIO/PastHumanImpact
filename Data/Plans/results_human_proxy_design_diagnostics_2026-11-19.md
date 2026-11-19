# Results: human-proxy design diagnostics (tests A and B, record loss)

Date: 2026-11-19.
Issue: #348. Decision record: `Data/Plans/decision_record_human_impact_reframing_2026-11-16.md` (D10, D12, "Gating tests").

These are methods diagnostics on the existing matched SPD, KK10 and HYDE extractions. They are not confirmatory runs. All values are point estimates; there is no 95 % interval yet, because the bootstrap interval comes with the single-proxy engine (#347).

## Decisions taken on these results

Decided by Ondřej on 2026-11-19, after seeing the results below.

| # | Question | Decision |
| --- | --- | --- |
| D12 | Residual-df minimum | Five. The provisional value is confirmed; moving from four to five loses 18 of 1175 records under filtered climate |
| D10 | Climate representation | Filtered climate (`max_cor = 0.8`, `max_vif = 5`) in every run. The only climate sensitivity is principal components; the all-four-variables sensitivity is dropped. Test A shows that the filtering alone does not change which side leads |
| Test B | Scope | The manuscript stays close to the current R1, with the single-proxy runs as a robustness section |
| Test B | Reporting of age detrending | The collapse of the KK10 human share after detrending is noted. The main analysis uses the original (not detrended) proxies. The detrended refits are reported as a supplementary analysis, with the caveat below |

These decisions are also recorded as amendments in the decision record (D10, D12, and the test B outcome).

## How the results were produced

- Pipeline: `R/analyses/91_sensitivity_analyses/human_proxy_hvarpart_design_diagnostics/pipeline.R`, run with its `00_run.R`. Store: `sensitivity_analyses/human_proxy_hvarpart_design_diagnostics`.
- Tables: `Outputs/Tables/H1/Sensitivity/Human_proxy_hvarpart_design_diagnostics/`. The file names below are relative to that folder. Run settings and upstream store fingerprints are in `human_proxies__design_diagnostics__provenance.csv`.
- Cohort: the matched 2.0–8.0 ka records shared by SPD, KK10 and HYDE (1175 records, 500-year steps).
- Spatial H1 means the per-record models with a time control. Temporal H1 means the continental-region × age models with dbMEM spatial control.
- All models were fitted at a residual-df minimum of four; the "five" view keeps the records with residual df of at least five. The threshold only decides which records are fitted, so this gives the same fits as a separate run at five.
- HYDE here is HYDE 3.2 population (`popc`), not HYDE 3.5 land use. KK10 is land use.
- In-pipeline checks passed: no model errors, the detrended proxies are uncorrelated with age within every record (`human_proxies__age_detrending__orthogonality_check.csv`), and the commonality fractions of every fit sum to its total adjusted R² (`human_proxies__commonality_total_check__time_control.csv`).
- The SPD filtered-climate model reproduces the earlier `spd_matched_bridge` model of the collinearity-managed pipeline exactly (status, residual df, sample counts and components).

## Record loss (D12)

Source: `human_proxies__residual_df_record_loss__time_control.csv`.

| Model | Estimable at minimum 4 | Estimable at minimum 5 | Lost by moving to 5 |
| --- | --- | --- | --- |
| SPD, filtered climate | 1147 | 1129 | 18 |
| SPD, all four climate variables | 1138 | 1099 | 39 |
| KK10, filtered climate | 1148 | 1130 | 18 |
| HYDE 3.2 population, filtered climate | 1123 | 1107 | 16 |

- At a minimum of five with filtered climate, SPD loses 46 of 1175 records in total: 22 for too few residual df, 21 for too few unique ages, and 3 for a missing predictor.
- HYDE loses 28 records to a missing predictor at either minimum, because HYDE 3.2 population is constant (zero) within those records.
- By region, the 18 SPD records lost by moving to five under filtered climate are 8 in North America, 4 in Europe, 3 in Latin America, 2 in Oceania and 1 in Asia. With all four climate variables the same comparison loses 17, 14, 3, 1 and 4. Every region keeps at least 33 estimable records (`human_proxies__residual_df_record_loss_by_region__time_control.csv`).

## Test A: climate reduction (SPD)

Same records and ages; the two arms differ only in the climate block (all four variables, or the shared filtered selection).

| Scale, minimum 5 | All four climate | Filtered climate | Source |
| --- | --- | --- | --- |
| Spatial H1, overall zero-truncated balance (headline), 1056 paired records | −0.351 | −0.306 | `spd__climate_reduction__spatial_balance_estimates__time_and_space_control.csv` |
| Spatial H1, overall signed balance | −0.109 | −0.088 | same |
| Temporal H1, mean zero-truncated human − climate allocation, 65 region × age units | −0.606 | −0.590 | `spd__climate_reduction__composition_summary__space_control.csv` |
| Temporal H1, mean signed difference | −0.103 | −0.091 | same |

- Filtering moves the balance slightly towards human at both scales, but climate leads in both arms.
- In spatial H1, the number of records ranked human above climate goes from 283 to 301, and 98 of 1056 records change side (`spd__climate_reduction__ranking_summary__time_control.csv`).
- In temporal H1, all 65 units are on the climate side in both arms, and no unit changes side.
- At a minimum of four the overall spatial balances are −0.349 and −0.305 (zero-truncated), on 1093 paired records.
- 296 of the 1056 paired records keep all four climate variables under the filter, so their two arms are identical. The median per-record change in the balance is zero (`spd__climate_reduction__balance_summary__time_control.csv`).

## Test B: age detrending (spatial H1)

Each proxy was replaced, within each record, by the residuals of a linear regression on age, and the models were refitted with the same filtered climate. The comparison uses the common cohort (records estimable in all six test-B models): 1016 records at a minimum of five.

The reading rule was fixed and posted in #348 on 2026-11-18, before any test-B result was computed.

| Proxy | Overall zero-truncated balance, original → detrended | Overall signed balance, original → detrended | Median human contribution, original → detrended (share kept) | Reading |
| --- | --- | --- | --- | --- |
| KK10 (decisive) | +0.076 → −0.338 | +0.023 → −0.101 | 0.151 → 0.058 (38 %) | Collapses |
| HYDE 3.2 population | +0.129 → −0.181 | +0.041 → −0.059 | 0.166 → 0.092 (55 %) | Partial |
| SPD (reference) | −0.329 → −0.597 | −0.094 → −0.160 | 0.065 → 0.016 (25 %) | Larger loss than KK10 |

Sources: `human_proxies__age_detrending__spatial_balance_estimates__time_and_space_control.csv` and `human_proxies__age_detrending__balance_summary__time_control.csv`.

- KK10 meets both collapse conditions: the overall balance moves to the climate side, and less than half of the human contribution remains.
- HYDE moves to the climate side but keeps more than half of its contribution, so it reads as partial. Under the rule this is reported as a difference between constructs and data versions.
- SPD loses a larger fraction than KK10, so under the rule the result is reported as trend sharing common to all proxies, not specific to the gridded ones.
- The readings are the same at a minimum of four and for the signed profile, so no outcome class changes.
- Records ranked human above climate: KK10 606 → 245 (387 change side), HYDE 658 → 346 (354 change side), SPD 261 → 140 (135 change side) (`human_proxies__age_detrending__ranking_summary__time_control.csv`).
- Before detrending, the median share of the human marginal contribution that is shared with time (the human ∩ time fraction plus the human ∩ climate ∩ time fraction) is 0.70 for KK10, 0.55 for HYDE and 0.72 for SPD (`human_proxies__human_time_commonality__summary__time_control.csv`).

### Caveat for reporting the detrended analysis

The models already contain time as a linear term in age. Removing the linear age trend from the proxy therefore leaves the fitted model the same: the total adjusted R² does not change (median and quartile changes are 0 for all three proxies, and no record changes by more than 0.001). Detrending changes only how the variation that the proxy and time explain jointly is divided. Before detrending, that variation is split between human and time; after detrending, all of it goes to time.

Test B therefore measures how much of each proxy's human share is variation it shares with the time trend. It does not show that this variation is not caused by people: a steady rise in population or land use over time produces the same pattern. This is why the main analysis uses the original proxies, and the detrended refits are reported as supplementary.

## Limits

- Point estimates only, with no 95 % interval.
- Detrending is linear in age only, and only the proxy is detrended; climate keeps its own trend.
- HYDE is HYDE 3.2 population. Test B for land use rests on KK10 until HYDE 3.5 land use is added (#346).
- The `rdacca.hp` commonality fractions are rounded to four decimals, so very small fractions are coarse.
- Record loss is counted on the 2.0–8.0 ka common cohort only, not on each proxy's native window.
