# H1 reciprocal spatiotemporal HVarPart analyses

The pipelines in this directory form the canonical H1 analysis. Shared inputs
are prepared once, then proxy-specific scientific operations run in independent
target stores. Downstream pipelines may read only the public targets documented
by their upstream pipeline.

Figure 2 uses the SPD time-control and spatial-aggregation stores. Figure 3
combines the SPD and event spatial-control stores. Robustness analyses run
separately under `91_sensitivity_analyses`.
