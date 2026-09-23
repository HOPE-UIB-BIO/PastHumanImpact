# SPD human-proxy convergence

## Objective

Implement Issue 342 as a reproducible H1 sensitivity analysis testing whether
the canonical archaeological SPD converges with KK10 anthropogenic land cover
and HYDE 3.2 population estimates over their shared 8.0--2.0 ka BP interval.

The workflow tests convergence without assuming a strong correlation. It does
not replace the canonical SPD, add KK10 or HYDE to the HVarPart models, or run
the protected temporal-model fitting workflow.

## Analysis contract

- Use `data_spd_250_with_500_fallback` as the focal H1 SPD product.
- Match KK10 and HYDE to pollen-record locations and the selected SPD radius.
- Use a 500-year primary time grid from 8.0 to 2.0 ka BP without extrapolation.
- Use HYDE 3.2 baseline total population count after locking the exact point
  release, raw filename, units, and checksum from its source documentation.
- Follow Gordon et al. by defining bins from the focal SPD, reporting medians
  and empirical 95% ranges, and calculating Kendall rank correlations between
  matched bin medians.
- Retain raw values and use square-root SPD and HYDE values for the primary
  Gordon-style display.
- Treat correlations as descriptive convergence, not causal validation.
- Keep raw global rasters and target stores in configured external storage.
- Add no new R package dependency.

## Implementation phases

### Phase 1 - Source and protocol lock

Record source version, variable, units, CRS, temporal coverage, licence,
filename, size, and checksum. Pre-register coverage, transformation, binning,
time-alignment, and sensitivity rules before inspecting correlations. Provide
an opt-in acquisition runner with restartable downloads and selective extraction
of the nested HYDE population raster.

Validation: validate direct endpoints, source-manifest fields, nested-archive
extraction, file fingerprints, raster metadata, calendar conversion,
non-negative values, and absence of unreported source circularity. The analysis
pipeline must stop with the exact acquisition command when a source is absent.

### Phase 2 - Spatial and temporal matching

Import the public SPD target, build geodesic dataset-centred buffers using its
selected radius, aggregate KK10 land-use fraction and HYDE population, align
the three sources on the common age grid, and preserve interpolation and
coverage metadata.

Validation: use synthetic rasters and hand-checkable fixtures, enforce unique
dataset-age keys, prohibit extrapolation, and reconcile exclusions by source,
region, and age.

### Phase 3 - Decile correlations

Create overall and eligible regional SPD-defined quantile bins, calculate
medians and empirical 2.5--97.5% ranges, compute Kendall tau between the median
series, and add dataset-clustered bootstrap intervals.

Validation: test ties, zeros, duplicate cut points, missing values, coverage
rules, deterministic bootstrap results, and exact reconstruction of every
correlation from the exported bin table.

### Phase 4 - Sensitivity analysis

Repeat the comparison for first differences, alternative bin counts, native
HYDE snapshots, strict 250/500 km cohorts, and point-cell extraction.

Validation: verify change direction, matched cohorts, unchanged source rows,
and complete scenario metadata for each sensitivity result.

### Phase 5 - Evidence products

Export matched values, bin summaries, correlations, coverage/provenance, and
semantic PNG/PDF figures from a dedicated sensitivity target store.

Validation: compare all plotted points and annotations with exported tables,
inspect both rendered formats, and register public targets in the pipeline
contract and evidence manifest.

### Phase 6 - Manuscript and reviewer response

Add a live-calculation reviewer-response module, update the Reviewer 2 SPD-bias
response, add manuscript and supplementary insertions, and register revision
assets without hardcoding manuscript figure numbers in analytical paths.

Validation: render the response and manuscript, assemble revision assets, and
confirm that prose, captions, figures, and tables report identical versions,
coverage, transformations, and numerical results.

## Reproducibility boundaries

- Use `Targets_data/sensitivity_analyses/human_proxy_convergence` for target
  state.
- Do not overwrite canonical H1 stores, historical SPD artifacts, or temporal
  model fits.
- Do not commit raw KK10 or HYDE rasters.
- Leave repository changes uncommitted and unpushed for human review.
