# Analysis Structure and Output Naming

This file defines the canonical ownership of reusable functions, visualisation scripts, and generated figures in PastHumanImpact.

## General principles

Organise code and outputs by stable scientific or computational purpose. Do not organise canonical paths by manuscript position, publication status, GitHub issue, pull request, or a temporary development phase.

Keep directory trees shallow. Add a second-level folder only when it separates multiple coherent responsibilities; do not create a folder for a single function without a clear expectation that the responsibility will grow.

Use `dataset` rather than `core` in new code and path names. Retain `core` only when referring specifically to the physical sediment core or when an external schema requires that spelling.

## Function ownership

The canonical function tree is:

```text
R/functions/
├── climate/
├── data/
│   ├── factors/
│   ├── pollen/
│   ├── preparation/
│   └── storage/
├── human_impact/
│   ├── events/
│   ├── paps/
│   └── spd/
├── hvarpart/
│   ├── components/
│   ├── diagnostics/
│   ├── fitting/
│   └── preparation/
├── modelling/
│   ├── configuration/
│   ├── fitting/
│   ├── prediction/
│   ├── preparation/
│   └── provenance/
├── procrustes/
├── spatial/
│   ├── dbmem/
│   ├── diagnostics/
│   ├── hvarpart/
│   └── thinning/
├── temporal/
├── visualisation/
│   ├── common/
│   ├── data/
│   ├── diagnostics/
│   ├── h1_spatial/
│   ├── h1_temporal/
│   └── h2/
└── workflow/
    ├── configuration/
    └── testing/
```

Assign a function according to its primary responsibility, not according to the script that currently calls it. Generic distance, dbMEM, Moran, and thinning operations belong to `spatial/`; HVarPart model fitting and component extraction belong to `hvarpart/`; functions coupling those domains belong to `spatial/hvarpart/`. Generic ordered-series and temporal-dependence operations belong to `temporal/`. File and directory operations belong to `data/storage/`; do not use the opaque abbreviation `io` in repository paths.

## Visualisation scripts

Visualisation scripts mirror the analytical output tree:

```text
R/visualisations/
├── overview/
├── data/
├── h1/
│   ├── spatial/
│   ├── temporal/
│   └── dataset_trends/
├── h2/
└── diagnostics/
```

Script filenames describe their analytical product and do not contain manuscript figure numbers or publication-role abbreviations.

## Generated figures

Canonical generated figures use this shallow tree:

```text
Outputs/Figures/
├── Overview/
├── Data/
├── H1/
│   ├── Spatial/
│   ├── Temporal/
│   │   ├── Event_trends/
│   │   ├── HVarPart/
│   │   ├── PAP_trends/
│   │   └── Predictor_trends/
│   └── Dataset_trends/
├── H2/
│   └── Interrelationships/
└── Diagnostics/
    ├── HVarPart/
    └── Temporal_models/
```

Do not use `Supplementary`, `Extended`, `Extra`, or similar publication-role names in this canonical tree. A separate manuscript-assembly script may copy and rename selected outputs to `Figure1`, `FigureS1`, or another publication-specific sequence.

Manually edited comparison files are references rather than generated outputs. Keep them outside the canonical generated-output tree and never overwrite them from an analytical script.

## Analysis-profile vocabulary

Use names that state how a result was calculated:

| Name | Meaning |
| --- | --- |
| `human_climate_only` | Human and climate model without the new structural control group. |
| `untruncated_hierarchical_contributions` | Hierarchical contributions retaining their original signs. |
| `zero_truncated_hierarchical_composition` | Non-positive hierarchical contributions removed and the remaining contributions normalised to sum to one. |
| `unique_adjusted_r2` | Unique conditional adjusted R-squared contributions, which may be negative and need not sum to one. |

Avoid generic canonical filename suffixes such as `baseline`, `signed`, `allocation`, `pure`, or `pure_fractions`. Internal mathematical variables may use shorter terminology when the longer form would obscure an established calculation, but exported tables, figures, targets, and report labels must use the descriptive vocabulary.
