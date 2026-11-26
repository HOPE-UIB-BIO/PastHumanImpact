# Human proxies at their native temporal coverage

This pipeline publishes the main human-proxy data layer: SPD, KK10 land use, and HYDE 3.5 land use and population, each at its own native temporal coverage and at several spatial supports around each pollen record. Its public targets are `data_human_proxy_values` (one row per record, age, proxy, and support) and `data_human_proxy_regional` (means by continental region, climate zone, age, proxy, and support). It reads the SPD store `data_preparation/spd` and the record metadata in `Assembly/data_meta_*.rds`, and writes the store `data_preparation/human_proxies`.

## Proxies, ages, and supports

| Proxy | Ages (years BP, 500-year steps) | Supports | Transformation |
| --- | --- | --- | --- |
| `spd` | 2000 to 8500 | `spd_buffer` | square root |
| `kk10` | 500 to 8000 | `cell`, `radius_10km`, `radius_25km`, `radius_50km`, `spd_buffer` | none |
| `hyde_3_5_land_use` | 500 to 11500 | as KK10 | none |
| `hyde_3_5_population` | 500 to 11500 | as KK10 | square root |

- `cell` is the value of the grid cell that contains the record. The buffer supports are area-weighted means of the cells whose centre lies within a geodesic buffer. `spd_buffer` uses each record's SPD radius (250 km, or 500 km when fewer than 50 radiocarbon dates fall within 250 km).
- `is_native_layer` is `TRUE` when the value comes from an exact source layer and `FALSE` when it is linearly interpolated between the two neighbouring source layers. Values are never extrapolated beyond a proxy's coverage. HYDE 3.5 has 1000-year steps from 10000 BCE to 1000 BCE, a year-0 step, and 100-year steps after it (11950, 10950, ..., 2950, 1950, 1850, ... years BP), so no 500-year target age falls on a HYDE layer and every HYDE value is interpolated. Before 2000 BP their effective resolution is therefore 1000 years.
- HYDE 3.5 land use is (cropland + grazing land) / total cell area. HYDE 3.5 population is the population density (people per km²).

The settings used are exported with every run in `Outputs/Tables/Data/Human_proxies/human_proxies__settings.csv`, and the source files, checksums, and source time layers in `human_proxies__provenance.csv`.

## External sources

Raw rasters are external inputs and are never committed. Place them under the configured `data_storage_path`:

```text
Human_impact/
└── External_proxies/
    ├── KK10/
    │   └── KK10.nc
    └── HYDE_3_5/
        ├── cropland.nc
        ├── grazing_land.nc
        ├── population_density.nc
        ├── readme_HYDE_3.5.txt
        └── general_files/
            └── general_files/
                └── garea_cr.asc
```

- KK10 is the PANGAEA record <https://doi.org/10.1594/PANGAEA.871369>. It is shared with the convergence sensitivity, whose `download_sources.R` downloads it.
- HYDE 3.5 is "HYDE data for the Global Carbon Budget (2025)", <https://doi.org/10.24416/UU01-F45D44>. The repository blocks automated downloads, so download the files in a browser. The base scenario is in the folder `hyde35_c9_apr2025[1749214444]/original/gbc2025_7apr_base/NetCDF/`. The cell-area grid `garea_cr.asc` and the readme come from `general_files.zip` and `readme_HYDE_3.5.txt` in the package's `original/` folder. Unzip `general_files.zip` into `HYDE_3_5/general_files/`.

`proxy_sources.csv` records the version, file name, size, and MD5 checksum of every source. The pipeline stops with an error if a file is missing or has a different size, and reports whether each checksum matches in the provenance table.

## Run

Build the SPD store first. The data-preparation runner `R/analyses/01_data_preparation/00_run.R` runs this pipeline after the other data-preparation pipelines. To run only this pipeline:

```r
source("R/00_Config_file.R")

run_target_pipeline(
  script = "R/analyses/01_data_preparation/07_human_proxies/pipeline.R",
  store = resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/human_proxies"
  )
)
```

Selected source layers are cached as GeoTIFFs in `Human_impact/External_proxies/cache/human_proxies/`, separately from the caches of the convergence sensitivity.
