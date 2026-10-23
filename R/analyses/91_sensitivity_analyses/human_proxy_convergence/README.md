# SPD convergence with external human-impact proxies

This sensitivity analysis compares the canonical non-normalised archaeological SPD (250 km with a 500 km fallback) with KK10 anthropogenic land-cover fraction and HYDE 3.2 population. It also evaluates first differences, 5/20-bin alternatives, point extraction, and strict 250/500 km spatial supports.

Raw rasters are external inputs and must not be committed. Place them under the configured `data_storage_path` as follows:

```text
Human_impact/
└── External_proxies/
    ├── KK10/
    │   └── KK10.nc
    └── HYDE_3_2/
        └── popc.tif
```

## Automated source acquisition

From the repository root, run:

```powershell
Rscript R/analyses/91_sensitivity_analyses/human_proxy_convergence/download_sources.R
```

The script creates the required directories, downloads both public sources, and
extracts only `popc.tif` from the nested HYDE archive. Downloads use `.part`
files and resume after interruption when the same command is run again. Existing
completed files are left unchanged. To deliberately restart and replace them,
run:

```powershell
Rscript R/analyses/91_sensitivity_analyses/human_proxy_convergence/download_sources.R --overwrite
```

KK10 is approximately 17 GB and the HYDE archive is approximately 850 MB, so
check available disk space before starting. The completed rasters are opened and
validated for the documented layer count and geographic coordinate system.

The sources are the PANGAEA KK10 record
<https://doi.org/10.1594/PANGAEA.871369> and the HYDE-containing reproducibility
archive <https://doi.org/10.7910/DVN/E3H3AK>. The checked-in
`proxy_sources.csv` records both landing pages and direct download URLs. The
pipeline repeats the file-presence check and, if necessary, reports the exact
acquisition command above.

## Run

First build the canonical SPD and H1-input stores. Then run:

```r
source("R/analyses/91_sensitivity_analyses/human_proxy_convergence/00_run.R")
```

The pipeline caches only the required source layers as external GeoTIFFs. Buffer summaries include cells whose centres fall within the geodesic 250 or 500 km support; KK10 uses latitude-area-weighted means and HYDE uses population sums.

Reviewer-facing tables are written below `Outputs/Tables/H1/Sensitivity/Human_proxy_convergence/`; figures are written below `Outputs/Figures/H1/Sensitivity/Human_proxy_convergence/`. The matched dataset-age table and decile table are exported so every reported Kendall correlation can be recomputed without the raw rasters.
