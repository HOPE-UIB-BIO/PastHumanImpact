# Generated Tables

This directory contains tabular products exported by the analysis,
visualisation, sensitivity, and diagnostic scripts.

The folder hierarchy follows scientific purpose rather than manuscript figure
number. Filenames use double underscores to separate the analysed subject,
reported quantity, calculation profile, and structural control.

- `Dataset_summaries/` contains dataset preparation and coverage summaries.
- `H1/` contains human-climate analysis products.
- `H2/` contains predictor-interrelationship products.
- `Diagnostics/` contains validation, provenance, and dependence checks.

Canonical table producers write directly to these paths. Files here
should not be renamed manually without updating their producers and consumers.

Non-tabular generated data, including RDS objects and SQLite databases, belongs
under `Outputs/Data` instead.
