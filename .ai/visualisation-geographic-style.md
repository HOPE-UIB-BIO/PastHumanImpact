# Geographical Visualisation Style

This companion to `.ai/visualisation-style.md` defines the geographical ordering, climate-zone tones, and short labels for PastHumanImpact figures. It applies whenever a figure shows continents or Köppen climate zones.

## Continent order

Use this order for rows, panels, legends, and grouped summaries:

1. North America
2. Central & South America (`Latin America` in data)
3. Europe
4. Asia
5. Oceania

Use `region_labeller` for displayed names. Do not alphabetise regions or use the data name `Latin America` in a publication-facing label.

## Climate-zone order, labels, and colours

Use the display order in `data_climate_zones`, not alphabetical order, whenever climate zones form figure columns, x-axis labels, or legends. Use `resolve_climatezone_label()` for the short labels and `palette_ecozones` for the colours.

| Order | Climate-zone label | Short label | Colour |
| --- | --- | --- | --- |
| 1 | Polar | POL | `#907A8E` |
| 2 | Cold - Cold Summer | CCS | `#8C4418` |
| 3 | Cold - Warm Summer | CWS | `#DC702E` |
| 4 | Cold - Hot Summer | CHS | `#AA6133` |
| 5 | Cold - Dry Winter | CDW | `#CB8152` |
| 6 | Cold - Dry Summer | CDS | `#E59463` |
| 7 | Temperate | TMP | `#371E71` |
| 8 | Temperate - Dry Winter | TDW | `#562FB1` |
| 9 | Temperate - Dry Summer | TDS | `#9A7EDD` |
| 10 | Tropical | TRO | `#D68FD6` |
| 11 | Arid | ARD | `#DDDF78` |

Climate-zone tones are categorical colours. They must not be repurposed for human, climate, time, space, zero, or missing-data encodings.

## Applying the geographical system

- Keep the configured continent and climate-zone order even if a subset of categories is absent; drop only categories that have no relevant data.
- Use short labels where panel width is constrained. Use the full climate-zone label in legends or where space permits.
- Ensure that a climate-zone colour, its full label, and its abbreviation agree wherever they appear in the same figure.
