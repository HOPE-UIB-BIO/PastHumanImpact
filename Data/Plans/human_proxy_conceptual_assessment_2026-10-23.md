# Human-impact proxies: what they mean and how we can use them

Companion to `Data/Plans/plan_human_impact_reframing_2026-10-23.md`.
The working direction (section 8) is stress-tested in `Data/Plans/single_proxy_strategy_review_2026-10-23.md`; its findings are folded into sections 4, 5, 8 and 9 here.
Status: draft for discussion with the co-authors; it informs decision gate G2 and does not decide it.
Statements marked "verify" are checked in Phase 1 of the plan.

## 1. Why the construct matters more than the model

Our hypotheses ask whether *human impact* is the predominant predictor of pollen assemblage change.
In R1 we measured human impact with an SPD, but an SPD measures how much radiocarbon-dated human activity there was in a region, which is a proxy for relative population size.
It does not measure what people did to the vegetation.
"Climate outweighs SPD" is therefore a claim about human presence, not about human impact.

The KK10 result showed that the answer changes when the human variable changes.
Before choosing a proxy, we need to agree which link of the human–vegetation chain each proxy represents, what claim it can support, and which part of our design it is informative for.

## 2. Four axes for judging a proxy

### 2.1 Position in the causal chain

Human influence on vegetation runs through a chain:

1. **Presence and pressure**: how many people were in the region (population, demography).
2. **Activity and mechanism**: what they did with the land (clearance, cultivation, grazing, burning).
3. **Realised impact**: the vegetation change that resulted.
4. **Pollen signal**: what the pollen record shows, which is our response.

The closer a proxy sits to realised impact, the closer it gets to the response itself.
At the end of the chain the proxy *is* derived from the pollen record, which makes it circular.
This is the central trade-off of the whole problem: **proximity to impact versus independence from the response**.
No available proxy is both close to impact and independent of pollen.

### 2.2 Evidence versus model

- **Empirical proxies** (SPD, expert events, ArchaeoGLOBE) summarise observations, with observational biases.
- **Modelled proxies** (KK10, HYDE) combine regional population estimates with assumptions about land use per person and about where within a region people lived or farmed.
  Their spatial patterns and part of their temporal patterns are imposed by those assumptions, not observed.

### 2.3 Spatial scale

Pollen from lakes and mires mainly records vegetation within tens of kilometres.
SPD is summed over 250 km (500 km fallback) because radiocarbon dates are sparse; a smaller window would contain too few dates.
That constraint belongs to the SPD data and does not apply to the gridded proxies.
KK10 and HYDE are 5' grids (about 9 km at the equator), so they can be extracted at a scale matched to the pollen-source area: the record's cell or a small radius, with a sensitivity across radii.
The PR #343 extraction averaged them over the SPD buffer only so that the convergence analysis compared like with like; the predictor extraction should be decoupled from it.
The expert events are assessed at the scale of the pollen-source area, which matches the response.
A finer extraction scale improves the spatial match, but it does not make the gridded values locally observed: within a region they are still allocated by model weights (section 3).

### 2.4 Which variation the proxy carries

Our two H1 scales use different variation, and a proxy must be informative in the dimension being tested:

- **Spatial H1** fits a model *within each record through time* (with time controlled), then aggregates the results spatially. The proxy needs meaningful temporal dynamics at each site.
- **Temporal H1** fits a model *among records within a region × 500-year slice* (with space controlled). The proxy needs meaningful differences among sites at the same time.
- **H2** works with regional trajectories per region × climate zone, so it needs a meaningful regional time series.

| Proxy | Within-record change through time (spatial H1) | Among-site differences within a slice (temporal H1) | Regional trajectory (H2) |
| --- | --- | --- | --- |
| SPD | Good: relative change through time within a region is what SPDs are built to show (booms, busts) | Weak: neighbouring buffers overlap, and among-site differences reflect where archaeologists dated sites | Good |
| KK10 | Weak to moderate: essentially the regional population curve, smooth and trend-like | Weak: within a region and time, differences come from a present-day land-suitability map | Moderate: smooth regional curve |
| HYDE `popc` | Weak: regional totals interpolated between millennial steps before 1 CE | Weak: allocation weights, partly the present-day population pattern | Moderate, coarse |
| Expert events | Good: local, staged step changes | Good: local impact stage differs among neighbouring sites | Moderate if aggregated (share of records past each stage) |
| ArchaeoGLOBE | Very weak: few time slices | None within a region (one value per region) | Moderate: regional land-use stage |

This table is partly a hypothesis, but the data support it: SPD and KK10 agree on level (Kendall tau 0.87) but not on change (first-difference tau 0.07).

## 3. The proxies one by one

### 3.1 SPD of archaeological radiocarbon dates

- **What it measures:** relative intensity of dated human activity in a region through time, used as a proxy for relative population change. It measures density or pressure, not impact.
- **How it is built:** summed calibrated date probabilities within 250 km of each record (500 km fallback).
- **What it is not:** an absolute population estimate, or comparable in magnitude among regions with different research intensity. It says nothing about land use (foragers and farmers can produce similar SPDs).
- **Independence:** independent of pollen and of climate data. Population itself responds to climate, so a real climate → people → vegetation pathway produces shared variance. That shared variance is meaningful, not an artefact.
- **Coverage:** reliable only to about 2 ka, because radiocarbon dating declines once written sources exist. Usable back to about 11.7 ka, with increasing taphonomic loss and fewer dates.
- **Known problems:** calibration-curve artefacts, research and dating bias, the debate over SPDs as palaeodemography (Reviewer 2), and data gaps where fewer than 50 dates fall within 500 km (87 records).
- **Claims it can support:** "climate versus human pressure", and how vegetation responded to demographic booms and busts.
- **Role:** a strong candidate for the *demography* component, especially in spatial H1 and H2. It is weak in temporal H1, because among-site SPD differences partly map archaeological effort.

### 3.2 KK10 anthropogenic land cover

- **What it measures:** modelled fraction of land used by people (cleared for crops and pasture).
- **How it is built:**
  - Regional population, from historical estimates back to about 3 ka and from the climate-driven GLUES model before that (verify).
  - Per-capita land use falls as population density rises (intensification), so early low-density populations clear a lot of land per person. KK10 is therefore the "high early land use" scenario (verify).
  - Allocation within regions follows present-day land suitability for crops and pasture.
- **What it is not:** observed land cover. It can be extracted at the scale of the pollen-source area, but its temporal shape at a site is roughly a scaled regional population curve, and its among-site pattern within a region is a static suitability map.
- **Independence:**
  - Pollen: the calibration may include the Bradshaw (2004) Scandinavian forest-cover series (verify).
  - Climate: two possible leaks. GLUES population before 3 ka depends on climate-driven productivity, and the suitability map depends on climate and soil.
  - Other proxies: KK10 is largely population transformed by a land-use model, so it partly duplicates the population signal it shares with HYDE (r = 0.96 within records).
- **Coverage:** 8 ka to AD 1850, annual layers.
- **Claims it can support:** "climate versus modelled land-use pressure", with the assumptions stated.
- **Role:** a candidate for the *land-use* component. Its very large effect in spatial H1 needs diagnosing before we trust it. A smooth, trend-like predictor can take over variance that is shared with time.

### 3.3 HYDE 3.2 population (`popc`)

- **What it measures:** modelled population count per grid cell. It is the same construct as SPD (demography), from a different source: historical and modelled population totals rather than archaeological evidence.
- **How it is built:** regional totals hindcast from McEvedy and Jones and similar sources, allocated with weighting maps (including the present-day population pattern and distance to water). Steps are millennial before 1 CE.
- **What it is not:** land use (we use `popc`, not the HYDE cropland and pasture layers). It is not locally informative, and it has little information before about 3 ka ("nearly pristine world").
- **Independence:** HYDE 3.2 does not use pollen (checked 2026-10-22). The land-use layers use BIOME and NPP climate weighting, but `popc` does not. For HYDE 3.5, see section 3.4.
- **Coverage:** 12 ka to present, the only gridded proxy covering the whole Holocene, but coarse.
- **Claims it can support:** an independent check of the demography construct ("does the SPD-based result hold with a non-archaeological population estimate?").
- **Role:**
  - Sensitivity for the demography component, not an additional predictor next to KK10, because the two are near-duplicates.
  - The HYDE land-use layers could serve as a "low early land use" scenario against KK10's "high" scenario. Together they would bracket land-use uncertainty, which also speaks to the early-anthropogenic question raised by Reviewer 3.

### 3.4 Could HYDE 3.5 replace our SPD?

What HYDE 3.5 changes, from its release description (Utrecht University and PBL, 2025, doi:10.24416/UU01-F45D44):

- It replaces HYDE 3.4, covers 10,000 BCE to 2025 CE at 5' resolution, and separates rain-fed and irrigated cropland, including rice.
- Radiocarbon data from the IMSET project set the **onset of agriculture in Eurasia**, and ArchaeoGLOBE expertise sets it outside Eurasia.
- The release description does not say that prehistoric population totals or their fluctuations change (verify in the documentation).

So the radiocarbon overlap with our SPD is narrow.
Radiocarbon dates tell HYDE 3.5 *when farming started* in a region, not *how many people* were there through time.
HYDE 3.5 is therefore not an alternative SPD.
It is a land-use reconstruction whose timing of agricultural onset is anchored in archaeology.

**When would we use HYDE 3.5 instead of our SPD?**
When the claim we want to make is about land use across the whole Holocene in one consistent global product, rather than about demographic change.

**Pros of HYDE 3.5 as the primary human proxy:**

- It covers the whole Holocene to the present, with no 2 ka cut-off and no gaps (our SPD has 87 records with too few dates).
- It is a gridded product, extractable at the pollen-source scale, with no research-intensity bias among sites. That removes SPD's main weakness in temporal H1.
- Its land-use layers (cropland, grazing, rice) sit closer to impact than demography does, and agricultural onset now rests on archaeological evidence rather than on hindcasts alone.
- Units are absolute and comparable among regions.
- One internally consistent product gives both population and land use, instead of a mixture of SPD and KK10 with different assumptions.
- It is the land-use history behind the CMIP and LUH2 forcing datasets, so reviewers know it and it sidesteps Reviewer 2's objections to SPDs as palaeodemography.

**Cons:**

- It is a model, not an observation. Prehistoric population totals are hindcasts, and within regions they are allocated with weights that include present-day patterns, so the hazards in section 5 (human ∩ space, human ∩ time, climate leakage through BIOME and NPP weights in the land-use layers) apply in full.
- Temporal resolution before 1 CE is coarse (millennial in 3.2; verify for 3.5), so it carries no booms and busts. This is exactly the dynamic information SPD contributes, and the reason SPD and KK10 disagree on change.
- The radiocarbon input fixes onset dates only. Our SPD uses the same kind of evidence much more intensively, as a continuous local time series.
- In HYDE 3.2, early-Holocene land use is close to zero before about 3 ka. If that remains true in 3.5, a whole-Holocene model mainly contrasts "no humans" with "some humans" in the early part, by construction rather than by evidence. Check whether the archaeological onsets in 3.5 change this.
- Like KK10, it rests on McEvedy-type population totals, so using HYDE 3.5 with KK10 would be largely redundant.
- The 3.5 method is documented in a release description rather than a peer-reviewed paper (verify), and it changes between versions, so the version and checksum need locking as we did for 3.2.

**Where it could fit:**

1. **As an upgrade of our HYDE candidate.** Replace HYDE 3.2 `popc` with HYDE 3.5 (population plus cropland and grazing) in the evidence matrix. The archaeological onset dates make it strictly more informative than 3.2 for our question. This is worthwhile regardless of the other choices.
2. **As the land-use component in S3,** as an alternative to KK10: SPD (demography, empirical) plus HYDE 3.5 land use (modelled, archaeologically anchored onset).
3. **As the single whole-Holocene product (roles reversed).** HYDE 3.5 becomes primary for 11.7–0.5 ka, and SPD becomes the empirical check on demography over 11.7–2 ka. This is the strongest option for the whole-Holocene argument, if section 5's diagnostics show that its share is not just smooth structure.

A quick test before deciding: repeat the SPD convergence analysis against HYDE 3.5, including the first-difference comparison. If HYDE 3.5 agrees with SPD in level and remains flat in change, it complements SPD; it cannot replace it.

### 3.5 Expert pollen-based events

- **What they measure:** realised local human impact, in stages:
  - first indication (e.g. a continuous *Plantago lanceolata* curve);
  - first cultivation (cereal pollen);
  - extensive clearance (10 % Poaceae or *Calluna*);
  - complete clearance (20 %);
  - region-specific types elsewhere (European settlement, extensive impact, weak/medium/strong).
- **What they are not:** independent of the response.
  - They are defined from taxa (*Plantago*, cereals, Poaceae, *Calluna*) that also enter richness, evenness, turnover and rate-of-change calculations. Clearance thresholds are directly tied to evenness and composition.
  - They are binary or ordinal step functions, and their typologies differ among regions.
- **Coverage:** every record was evaluated by experts. A record without events is an expert-confirmed absence, not missing data. They cover the whole Holocene.
- **Strength:** they are the only proxy that is local, impact-level, and informative among neighbouring sites.
- **Claims they can support:** not "humans predict PAPs" in the main analysis, because they are circular. As a sensitivity they show whether a local, impact-level representation changes the attribution.
- **Role:** a supplementary, visually flagged sensitivity, as in R1, not a predictor in the main comparison. Using them as a benchmark (S6) was considered and dropped.
- **Note:** the R1 event sensitivity used matched subsets with observed events. Because zeros are true absences, check whether records without events were excluded, and whether that biased the comparison.

### 3.6 ArchaeoGLOBE (to evaluate)

- **What it measures:** expert archaeological assessment of land-use type and prevalence (foraging, pastoralism, extensive and intensive agriculture, urban) for about 146 world regions at about 9 time slices from 10 ka to AD 1850 (verify).
- **Strength:** it captures the *mode* of human impact (foragers versus farmers), which neither SPD nor KK10 does. It is independent of our pollen data, although some experts may draw on palaeoecology (verify).
- **Weakness:** very coarse in space and time, and categorical.
- **Role:** a stratifier, or a regional predictor in H2. It is not usable in H1 among-site models.

### 3.7 Agricultural onset dates (not used)

- **What they measure:** when farming arrived in a region, from archaeological dates (e.g. Neolithic arrival in Europe, maize arrival in the Americas) or from ArchaeoGLOBE.
- **Role considered:** pre-farming and post-farming phases per region for a natural-experiment design (S7). S7 was dropped as out of scope, so onset dates are not used.

### 3.8 Excluded, with reasons

- **LandCover6k, REVEALS and pollen-based land-cover maps:** built from pollen, so circular.
- **Charcoal and fire records:** fire is driven by both people and climate, and charcoal often comes from the same cores.

## 4. How the proxies relate to each other

- **SPD and HYDE `popc`** measure the same construct (demography) from different evidence. They are alternatives, not complements.
- **KK10** is a function of population and modelling assumptions. It shares its population basis with HYDE (r = 0.96), and part of its information with SPD. "SPD + KK10" is therefore not two fully independent constructs. It is demography from archaeology plus demography passed through a land-use model.
- **KK10 and HYDE land use** are two scenarios of the same construct, with high versus low early land use.
- **Two families of evidence, not three.** SPD is archaeological; KK10 and HYDE both come from historical-demographic modelling built on McEvedy-type population totals. Agreement between KK10 and HYDE is therefore not replication, and only agreement between SPD and the modelled family counts as triangulation (review, issue 4).
- **The events** measure the outcome, but from the same pollen records, so they stay a supplementary sensitivity.
- **ArchaeoGLOBE** measures the type of land use, which is a different and complementary dimension.

## 5. Structural hazards for HVarPart

1. **Block-size asymmetry.** Adding predictors to the human block gives it more capacity to take shared variance. The unfiltered three-proxy block was the most human-dominant. Comparisons must control the number of predictors per block, or use a composite (S4).
2. **Smooth fields absorb shared structure.**
   - In temporal H1, the human share mostly replaces the spatial (dbMEM) share once KK10 enters: median space share 0.255 → about 0.03.
   - In spatial H1, a trend-like KK10 series may take variance shared with time.
   - Both need explicit human ∩ space and human ∩ time diagnostics.
3. **Climate leakage into the human block.** KK10 (GLUES, suitability) and HYDE land use (BIOME, NPP) contain climate-derived information. Variance they share with climate is then split between the blocks, which inflates the human share for reasons unrelated to people.
4. **Genuine climate → people pathways.** Some shared variance is real ecology (people followed favourable climates). Report the shared fraction rather than only zero-truncated unique shares.
5. **Unequal reliability by dimension** (section 2.4). Each proxy should be judged in the analyses where it carries meaningful variation.
6. **The climate block's size matters too.** Block-size asymmetry cuts both ways: reducing climate to fewer variables lowers its capacity and can shift every human–climate balance towards humans, whatever the proxy. The balance always depends on how many variables represent each block (review, issue 3).
7. **Spatial scale confounds proxy comparisons.** SPD is summed over 250–500 km and the gridded proxies are extracted at the pollen-source scale, so a difference between proxies mixes construct and scale (review, issue 6).
8. **Interpolation adds smoothness.** Proxies with coarse native steps (HYDE before 1 CE) are interpolated to 500-year ages, which makes them smoother than the evidence behind them and favours spurious fits within records (review, issue 7).
9. **Linearity.** HVarPart uses linear (db)RDA, so a proxy that acts through thresholds is undervalued and the comparison partly ranks proxies by linearity (review, issue 8).
10. **Predictor uncertainty is not propagated.** SPD calibration and sampling uncertainty and within-product uncertainty in KK10 and HYDE are ignored; only the KK10–HYDE bracket covers land-use scenario uncertainty (review, issue 9).

## 6. Ways to use the proxies

Each strategy is described with the claim it supports and its cost.

**S1 — One construct as primary, the others as sensitivities.**
SPD (demography) as primary, with HYDE as a check on the demography construct and KK10 as a land-use sensitivity.
Claim: "climate versus human pressure", stated explicitly.
Simple and keeps R1 largely intact, but it cannot claim to measure impact, and the KK10 result becomes a prominent caveat.

**S2 — A pooled human block (the current joint models).**
SPD + KK10 (+ HYDE), filtered locally.
Claim: "climate versus human influence broadly".
It is broad, but it mixes constructs and is exposed to hazards 1–3. The meaning of "human" changes among units after local filtering.

**S3 — Construct-separated partition (overlap of human representations).**
Three focal groups in HVarPart: climate, demography (SPD or HYDE 3.5 population), and land use (KK10), with the time and space controls protected.
The partition shows how much of what each representation explains in the PAPs is unique and how much it shares with the other: demography only, land use only, demography ∩ land use, and each one's overlap with climate.
This is a different question from the convergence analysis, which compares the proxies with each other rather than with the response.
Claim: "demography and land use explain the same (or different) parts of assemblage change".
It costs one extra predictor, so under the single-proxy direction it is a supplementary overlap analysis, not the source of the headline (section 8).

**S4 — A composite human index.**
One standardised human variable (e.g. the first principal component of the human proxies within a record, or a standardised mean), so that the human block has one degree of freedom, as SPD alone does.
Claim: "climate versus a general human-intensity gradient".
It removes the block-size asymmetry, but loses interpretability. Mainly useful as a fairness check on S2 or S3.

**S5 — Triangulation envelope.**
Run each proxy alone and report the range of outcomes. A conclusion counts as robust only if it holds across all defensible representations.
Claim: "the conclusion is (or is not) robust to how human impact is represented".
Honest, and fits framing scenario 3, but it may leave the headline open.

**S6 — Events as a benchmark for the independent proxies. Dropped (2026-10-23).**
Do not use events as predictors. Ask whether the human share attributed by SPD, KK10 or S3 rises in records and periods after first indication, first cultivation, or clearance.
If it does, the independent proxies track real impact. If it does not, their human share is suspect.
This validates the construct rather than explaining PAPs with pollen-derived predictors, so circularity matters much less.
A second, stricter sensitivity recomputes the PAPs without the indicator taxa before using events as predictors. This reduces, but does not remove, the circularity.

**S7 — Natural experiment around agricultural onset (whole-Holocene design). Dropped (2026-10-23): out of scope for this paper.**
Split each region into pre-farming and post-farming phases using archaeological onset dates or ArchaeoGLOBE. Test whether the climate share falls, or the human share rises, after farming arrives.
This uses the whole Holocene without needing a gridded human proxy before 8 ka. Before farming, low human influence is a real expectation, not an artefact of near-zero HYDE values.
Claim: "assemblage dynamics decoupled from climate once farming arrived" (or did not).
It is the strongest use of the whole-Holocene argument. It needs onset data and new analysis code.

## 7. Proposed role matrix (for discussion)

| Proxy | Spatial H1 | Temporal H1 | H2 | Whole Holocene | Proposed role |
| --- | --- | --- | --- | --- | --- |
| SPD | Primary candidate (demography) | Sensitivity only (among-site effort bias) | Primary candidate | 11.7–2 ka window | Demography component |
| KK10 | Candidate (land use), after the human ∩ time diagnostic | Candidate, after the human ∩ space diagnostic | Candidate (needs temporal models) | 8–0.5 ka window | Land-use component |
| HYDE `popc` | Sensitivity (demography) | Sensitivity | Sensitivity | Coarse 11.7–0.5 ka | Check on the demography construct |
| HYDE land use (3.5) | Candidate land-use component, or scenario sensitivity | Candidate land-use component, or scenario sensitivity | Optional | 11.7–0.5 ka, archaeologically anchored onset | Upgrade of the HYDE candidate; possible whole-Holocene primary (section 3.4) |
| Expert events | Supplementary sensitivity | Supplementary sensitivity | Not used | Full Holocene | Circular; supplementary sensitivity only |
| ArchaeoGLOBE | No | No | Regional stratifier or predictor | 10 ka–AD 1850 | Mode of land use |

## 8. Working direction: one human proxy per model (to confirm at G2)

Proposed by Ondřej on 2026-10-23: no joint human block.
Each model contains exactly one human proxy and a reduced climate block, and the whole spatial and temporal H1 (and H2) analysis is repeated for each proxy.
This is S5 (triangulation envelope) made fair and well-powered.

### Why not a joint human model

- **Degrees of freedom.** Within-record models over 8.0–2.0 ka have 13 ages. Every extra human predictor costs a residual degree of freedom; the unfiltered three-proxy model lost 272 records to insufficient residual df, and the minimum already had to be relaxed from five to four.
- **Block-size asymmetry** (hazard 1). A multi-proxy human block gains capacity to take shared variance, so part of its larger share is a property of the block, not of people.
- **Mixed constructs.** SPD (demography) and KK10 (land use) answer different questions; pooling them makes "human" mean something different in every unit after local filtering.
- **Redundancy.** KK10 and HYDE are near-duplicates (r = 0.96), and KK10 partly carries the population signal SPD already represents.

What we give up: a joint model could, in principle, split demography from land use within one decomposition. The single-proxy design cannot; it can only compare the proxies side by side. Each single-proxy share therefore includes whatever is correlated with that proxy (omitted-variable bias; review, issue 2), so shares are variation *associated with* a representation, not its effect. Given the df constraint, that is an acceptable cost; the S3 overlap analysis measures how much the demography and land-use shares are the same variation, and the convergence analysis describes how the proxies relate.

### Hypotheses

H1 and H2 are restated per construct, with predictions, the decision metric (for example, the sign and interval of the dbMEM-adjusted balance), and the headline rule written down before the runs (review, issue 1):

- **H1a / H2a:** demography explains more of PAP change than climate.
- **H1b / H2b:** land use explains more of PAP change than climate.

Each is tested at both H1 scales. Without this, any combination of results could be presented as a finding.

### Design rules

- **One human predictor per model,** never filtered.
- **Climate selected once per analytical unit,** without the response (`collinear`, `max_cor = 0.8`, `max_vif = 5`, preference order annual temperature, coldest-month temperature, warm-season precipitation, cold-season precipitation), and **shared by every proxy's model in that unit**. Differences between proxy runs are then due to the proxy alone. Each dropped climate predictor returns one residual degree of freedom.
- **Time and dbMEM controls protected** and added after climate selection; cross-block (human–climate) correlation diagnosed, not used for removal.
- **Full data per proxy for temporal H1:** each proxy runs on all its records over its native window, as SPD and events do in the current temporal figure. A **common cohort** (units where every proxy is available) in the 8.0–2.0 ka window is a supplementary check that differences between proxies are not caused by sample composition (review, issue 11). Record-by-record comparisons in spatial H1 (the agreement map) use the common cohort by construction.
- **A headline rule fixed before the results:** a conclusion is stated in general terms only if it holds for every proxy in the main results and is not overturned in the common-cohort check; otherwise it is stated per construct ("with demography...", "with land use...").
- **Association language** throughout: "variation associated with demography", not "the effect of demography".
- **Required sensitivities**, because each answers a specific objection in the review:
  - climate representation: all four variables (where df allow), the filtered selection, and climate principal components (issue 3);
  - matched spatial scale: KK10 and HYDE averaged over the SPD buffer, reusing the PR #343 extraction (issue 6);
  - native steps: only ages with exact proxy layers (issue 7);
  - transformations: rank or log-type transforms for every proxy (issue 8);
  - SPD resampling with bootstrapped date sets for one scale, if feasible (issue 9);
  - human ∩ space and human ∩ time diagnostics with detrended variants, and proxy–climate correlations per unit (issues 5 and 7).

### Proposed proxy set

| Run | Proxy | Construct | Role |
| --- | --- | --- | --- |
| 1 | SPD (square-root or raw, per Phase 0) | Demography, empirical | Main comparison |
| 2 | KK10 land-use fraction | Land use, modelled, high early-use scenario | Main comparison |
| 3 | HYDE 3.5 land use (cropland plus grazing fraction) | Land use, modelled, low early-use scenario with archaeological onset | Main comparison |
| 4 | HYDE 3.5 population | Demography, modelled | Supplementary check on SPD |
| 5 | Expert events | Realised impact, circular | Supplementary sensitivity, visually flagged as circular |

Runs 2 and 3 bracket the land-use uncertainty (high versus low early land use), which also answers the early-anthropogenic question from Reviewer 3.
They belong to one evidence family (section 4), so they are a scenario bracket, not two independent replicates.

### What the existing runs already suggest

These runs used all four climate predictors (except the SPD bridge), so they only preview the design:

- Temporal H1 is climate-dominant for every single proxy: 0 of 65 slices human-dominant for the SPD bridge and for KK10, and 2 of 65 for HYDE 3.2.
- Spatial H1 depends on the proxy: 28.5 % of records human-dominant for the SPD bridge, 55.1 % for KK10 (overall balance +0.036), and 61.6 % for HYDE 3.2 (+0.101).

If this holds under the new design, the headline rule gives a clear structure: change through time within regions follows climate whatever the human proxy, while the spatial attribution depends on whether people are represented by demography or by land use.

### Other strategies under this direction

- **Kept:** S5 (the core of the design) and the proxy-native windows for the whole-Holocene argument.
- **Kept as a supplementary overlap analysis:** S3, with demography and land use as separate focal groups. It answers what the single-proxy runs cannot: whether KK10's larger spatial share over SPD is information SPD lacks, or the same variation represented differently.
  - Pairings: KK10 + SPD (modelled land use against empirical demography) as the main pairing; KK10 + HYDE 3.5 population as a check. KK10 and HYDE share their population basis (r = 0.96), so their overlap should be close to total; if it is not, the land-use model adds something of its own.
  - Read the commonality fractions, not the hierarchical allocation. Hierarchical partitioning splits shared variance equally between groups, which hides the overlap. Under collinearity the unique fractions become small and unstable, but the shared fraction is still well defined, and it is the quantity of interest.
  - Degrees of freedom: fit it only where the extra predictor leaves the residual-df minimum intact. Temporal H1 (many records per slice) is unaffected; in spatial H1 report how many records drop out. Use the same shared climate selection as the single-proxy runs, so S3 differs from them only by the added human group.
- **Dropped:** S2 (pooled block), for the df and block-size reasons above. S4 (composite index) is not needed, because every model already has one human degree of freedom. S7 (agricultural-onset natural experiment) is out of scope for this paper; the whole-Holocene argument rests on the proxy-native windows instead. S6 (events as a benchmark) is dropped; the events stay only as the supplementary sensitivity in the proxy set.
- **Still required:** the human ∩ time diagnostic for spatial H1 and the human ∩ space diagnostic for temporal H1, now for each proxy.

## 9. What one model per proxy gains scientifically

1. **Each model answers a well-posed question.** "Does climate outweigh demography?" and "does climate outweigh land use?" are answerable. "Does climate outweigh human impact?" is not, while human impact is an undefined mixture of proxies. Claims then map onto constructs, which answers the reviewers' doubts about what SPD represents.
2. **The proxy is the only thing that varies.** With one human predictor, the same climate selection and the same cohort, the runs differ only in how people are represented. Differences in attribution can be traced to the representation itself, not to model size, filtering, or sample composition. This is a controlled comparison rather than a set of loosely related sensitivity analyses.
3. **Triangulation across different error structures.** SPD carries archaeological biases (research intensity, taphonomy, calibration). KK10 and HYDE carry modelling biases (population hindcasts, per-capita land-use rules, allocation maps). A conclusion that holds across proxies whose errors differ is unlikely to be an artefact of any one proxy's bias. This is the logic of triangulation (Munafò and Davey Smith 2018, Nature 553: 399–401). A conclusion that fails for one proxy points to which assumption drives it. The triangulation is between two families of evidence, archaeological and modelled, not three; KK10 and HYDE together add a scenario bracket, not a third line of evidence.
4. **Disagreement becomes a finding.** Where proxies agree and where they diverge (the agreement map) is informative in itself. In the previews, temporal attribution agrees across proxies and spatial attribution does not. If that holds, it says the climate signal through time is robust, and that among-site patterns depend on whether people are counted (demography) or their land use is mapped. That distinction bears directly on the human-centred diversity literature.
5. **Fair and estimable models.**
   - Every model has one human degree of freedom, so the human block never gains capacity just by being larger.
   - Within-block collinearity disappears, so unique fractions are stable and negative components rarer.
   - More residual degrees of freedom keep more records eligible.
   - "Human" means the same thing in every unit of a run, because there is no local filtering of the human block.
6. **Scenario uncertainty becomes visible.** KK10 (high early land use) and HYDE 3.5 (low early land use) bracket current uncertainty about prehistoric land use. The reader sees how much the attribution depends on that unresolved question, which links our result to the early-anthropogenic debate (Reviewer 3).
7. **Each proxy can use its full coverage.** The common window keeps the comparison controlled, and each proxy's native window extends the inference (SPD to 11.7 ka, land use to 0.5 ka) without forcing every analysis into the shortest shared interval.
8. **Protection against choosing the convenient proxy.** With the headline rule fixed in advance, the result cannot be steered by which proxy is called primary. Every representation is reported.
9. **Extensibility.** A new proxy (for example, future land-cover reconstructions independent of pollen) is one more run of the same pipeline, and H2 follows the same logic.

### Limits of these gains

The stress test (`Data/Plans/single_proxy_strategy_review_2026-10-23.md`) qualifies them:

- The design only answers the falsifiability objection if the per-construct hypotheses and the headline rule are fixed before the runs (issue 1).
- "The proxy is the only thing that varies" holds between proxy runs, but the shared climate reduction can still shift every human–climate balance; the climate-representation sensitivity is what makes the comparison with climate fair (issue 3).
- Shares remain associations that include correlated, omitted influences (issue 2).
- The design does not remove problems carried by the proxies themselves (climate in the gridded proxies, smoothness from interpolation, linearity, unpropagated uncertainty); those are handled by the required sensitivities in section 8 (issues 5–9). Showing each proxy with its full data in the temporal figure mixes construct with sample composition, which the common-cohort check addresses (issue 11).
- Several partly diverging answers need a clear message; the main figures and abstract are built around where the proxies agree (issue 10).

## 10. Questions for the co-authors

1. Which claim do we want to make: about human pressure, land use, realised impact, or human influence broadly?
2. Do we agree to drop joint human models in favour of one proxy per model, with a shared, response-free climate selection, and to fix the headline rule before seeing the results?
3. Are we comfortable treating KK10's spatial pattern (present-day suitability) and early-Holocene trend (GLUES) as human signal? If not, what diagnostic result would change our minds?
4. Do we restate H1 and H2 per construct (demography, land use) with predictions fixed before the runs (review, issue 1)?
5. How should climate be represented: the filtered selection, all four variables, or principal components (review, issue 3)?
6. Was the R1 event comparison restricted to records with observed events? Should it be redone with expert-confirmed zeros included?
