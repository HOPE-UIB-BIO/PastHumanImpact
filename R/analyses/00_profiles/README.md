# Analysis profiles

`analysis_profiles.csv` lists only intentionally supported analysis variants.
Pipelines select the rows relevant to their scientific operation and may branch
over those rows. They must not construct an implicit Cartesian product of
proxies, predictor sets, spatial scales, or controls.

Add a profile when an existing scientific operation needs another supported
parameter or predictor variant. Add a pipeline only when the scientific
operation itself is new.

All enabled profiles are validated by `validate_analysis_profiles()` before a
model pipeline reads them. Canonical and sensitivity profiles use the same
schema but write to separate stores and output paths.

`pipeline_contracts.csv` documents the owner, external store, public targets,
and prerequisite runner for every target graph. Downstream code may import
only these public targets.
