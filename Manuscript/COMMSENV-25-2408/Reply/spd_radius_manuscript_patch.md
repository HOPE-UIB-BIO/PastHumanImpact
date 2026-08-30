# SPD-radius sensitivity manuscript patch

## Methods

To test whether the spatial scale used to summarize archaeological evidence
affected our conclusions, we recalculated the radiocarbon summed probability
distribution (SPD) for every pollen record using strict 250 km and 500 km
search radii. Both calculations used the same radiocarbon dataset, calibration
choices, 0--12 ka time range, 100-year smoothing, and minimum requirement of
more than 50 radiocarbon dates. All vegetation, climate, and analytical
settings were held constant, so the only difference between the two analyses
was the archaeological search radius. We compared results for the same pollen
records and for the same continental time steps at both radii. The primary
analysis used the 250 km SPD whenever sufficient dates were available and the
500 km SPD otherwise, thereby favouring the most local archaeological context
while retaining records from data-sparse regions.

## Results

Usable archaeological SPDs were available for 1,044 of 1,270 pollen records at
250 km and 1,181 at 500 km. For the 999 pollen records that could be compared
at both radii, expanding the archaeological neighbourhood produced a median
change of only 0.015 in the human-minus-climate balance (IQR -0.015 to 0.054 on
a -1 to 1 scale). Although 100 individual records crossed the boundary between
climate and human dominance, the continent- and climate-zone summaries
remained close to zero and showed no consistent directional shift. Across 70
comparable continent-by-time results, the median change in the human
contribution was 0.006 (IQR 0.001 to 0.015), with only three reversals in the
relative importance of climate and human activity. The broad spatial and
temporal patterns were therefore insensitive to the tested archaeological
search radius.

## Discussion

The archaeological radius represents a trade-off between spatial proximity to
the pollen record and the amount of available evidence. Neither tested radius
should be interpreted as the pollen source area, which varies among sites
according to basin size, pollen dispersal, and the surrounding vegetation.
Nevertheless, expanding the archaeological neighbourhood from 250 km to
500 km caused only small changes in the estimated climate-human balance and
did not alter its broad spatial or temporal structure. We therefore retain the
250 km radius wherever it provides sufficient radiocarbon evidence and use
500 km only as a fallback in data-sparse settings. This approach preserves the
most local archaeological context supported by the data while maximizing
geographical coverage: 250 km is retained for 1,044 pollen records, and the
500 km fallback recovers usable archaeological information for a further 137.
The sensitivity analysis confirms that the main large-scale inference---that
climate is a stronger predictor than human activity of broad-scale vegetation
patterns---is robust to this choice.

## Figure captions

**Sensitivity of the climate-human balance to archaeological search radius.**
(A) Each line connects the same pollen record analysed with archaeological
evidence from within 250 km and 500 km. Negative values (teal) indicate that
climate explains more variation in vegetation than human activity, positive
values (gold) indicate a stronger human contribution, and white marks equal
contributions. (B) Median changes after increasing the radius from 250 km to
500 km are shown for continents and climate zones; points close to zero
indicate little overall change.

**Human contribution to vegetation patterns through time under the two
archaeological search radii.** Circles show estimates based on archaeological
evidence within 250 km and squares show estimates within 500 km. Continents are
displayed as separate rows and the blue arrow indicates the direction from
older to younger ages. The close overlap of the two trajectories shows that
widening the archaeological neighbourhood has little effect on the inferred
temporal pattern of human influence.

**Change in the estimated human contribution after increasing the
archaeological search radius from 250 km to 500 km.** Positive values indicate
a larger human contribution at 500 km, negative values indicate a smaller
contribution, and zero indicates no change. Continents are displayed as
separate rows and the blue arrow indicates the direction from older to younger
ages. Values clustered around zero show that the temporal pattern is only
weakly sensitive to search radius.

The corresponding live-calculation source is
`response_spd_radius_sensitivity.qmd`; its exported tables remain the numerical
source of truth.
