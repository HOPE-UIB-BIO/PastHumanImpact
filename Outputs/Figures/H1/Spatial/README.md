# H1 spatial figures

The figure tree first identifies the human-impact proxy. SPD-only supporting
analyses are nested under `SPD/`; shared predictor-level diagnostics remain at
the spatial-analysis level.

- `SPD/` contains the eight SPD spatial results and three supporting folders:
  `Predictor_collinearity/`, `Spatial_dependence/`, and
  `Spatiotemporal_composition/`.
- `Events/` contains the eight equivalent event-based spatial results.
- `Predictor_collinearity/` contains the proxy-independent PAP correlation
  diagnostic.

Main SPD and event filenames use four double-underscore-separated segments:

`<proxy>__<scientific_quantity>__<calculation_profile>__<structural_control>`

For example,
`spd__human_climate_balance__zero_truncated_hierarchical_composition__time_and_space_control`
identifies the proxy, displayed quantity, transformation, and controls without
requiring the directory context. Individual-predictor figures follow the same
scheme, such as `events__human__unique_adjusted_r2__time_control`.

`time_control` means that the within-dataset HVarPart model includes age.
`time_and_space_control` means that the human-climate balance also uses the
subsequent spatial dbMEM aggregation control. The calculation-profile segments
retain the canonical definitions in `.ai/analysis-structure.md`.