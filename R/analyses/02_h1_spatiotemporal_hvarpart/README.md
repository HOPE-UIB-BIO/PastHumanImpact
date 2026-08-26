# H1 reciprocal spatiotemporal HVarPart analyses

The pipelines in this directory form the canonical H1 analysis. Shared inputs
are prepared once, then proxy-specific scientific operations run in independent
target stores. Downstream pipelines may read only the public targets documented
by their upstream pipeline.

The spatial H1 figures use the matching SPD or event time-control and
spatial-aggregation stores. The temporal H1 figures combine the SPD and event
spatial-control stores. Robustness analyses run separately under
`91_sensitivity_analyses`.
