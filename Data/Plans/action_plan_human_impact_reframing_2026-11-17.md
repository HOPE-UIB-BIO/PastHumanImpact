# Action plan: human-impact reframing

Date: 2026-11-17.
Follows `Data/Plans/decision_record_human_impact_reframing_2026-11-16.md`, which holds the decisions; this file holds only the order of work. The GitHub issues (#317 and its sub-issues) carry the detailed scope in their "Update (2026-11-16)" sections.

## Step 0: this week

1. Commit and push the decision record and plan amendments, so the links in the #345 and #317 comments resolve.
2. Editor (#356): check the current revision deadline in the journal's submission system, agree a short holding email with the co-authors (draft in `Manuscript/_internal/`), and send it. The holding email announces the broadened human-proxy analysis and promises a firm date after the gating tests.
3. Start #344 (pipeline registry and provenance consistency). It is the first step on the critical path; it includes square-root SPD and the provisional residual-df minimum of five.
4. 22 November 2026: the co-author window closes. If nobody objects, close #345.

## Step 1: after #344, in parallel

5. #341, 300-grain refit: rebuild the pollen, PAP, and predictor stores under the 300-grain target with a 150-grain sample minimum, in a worktree. `filter_all_data()` is currently called with fixed defaults in `R/analyses/01_data_preparation/04_pollen/pipeline.R`, so the filter must first be parameterised and given separate stores; this is cleanest once #344 has fixed the contract registry. This step has the longest run time.
6. #348, gating diagnostics on existing outputs: test A (climate reduction), test B (trend sharing of KK10 and HYDE with time in spatial H1), and the record-loss count under a residual-df minimum of five.
7. #346 (HYDE 3.5 data layer and the early-Holocene check, D9) and the open facts in #345 (GLUES share in KK10 before 3 ka, the Bradshaw 2004 series, HYDE 3.5 time steps before 1 CE).

## Step 2: decision point after test B

8. Record the scope outcome in #348 and #317: close to the current R1 (the gridded-proxy share collapses after detrending) or the scale-divergence rewrite (it survives).
9. Confirm the residual-df minimum (five or four) in #344.
10. Send the editor a firm resubmission date (#356).

## Step 3: build

11. #347 single-proxy H1 engine, then in parallel: the #341 H1 fit, #349, #352, and the reduced validation set (#353, #354).
12. #350 figures, then the #351 memo and design confirmation with the co-authors.
13. #323 manuscript and reviewer response, including the pollen-count replies and Supplementary Methods, which now state that H1 was not refitted at stricter thresholds.

## Practical notes

- Run R with R 4.5.1 (`C:/Program Files/R/R-4.5.1/bin/Rscript.exe`); the default `Rscript` is R 4.6.2, which has no project library.
- The main risk is time, not method: steps 2 and 10 protect the timeline, and steps 3 and 5 decide when the build can start.
