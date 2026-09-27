# Geographical Visualisation Style

This companion to `.ai/visualisation-style.md` defines the geographical
ordering, region colours, short labels, and required terminology for
PastHumanImpact figures. It applies whenever a figure shows continental
regions or Köppen-derived regions.

## Required geographical terminology

- Use **continental region** for the broad groups North America, Central &
  South America, Europe, Asia, and Oceania.
- Use **region** for the Köppen-derived classes POL, CCS, CWS, CHS, CDW, CDS,
  TMP, TDW, TDS, TRO, and ARD.
- Do not use **climate zone** or **continent** as publication-facing synonyms
  for these levels in figures, captions, filenames, table names, or new
  function interfaces.
- The legacy internal data columns have the opposite-looking names:
  `region` stores the continental region and `climatezone` stores the
  region. Preserve those columns for pipeline compatibility, but translate
  them at every public interface and label.

## Continental-region order

Use this order for rows, panels, legends, and grouped summaries:

1. North America
2. Central & South America (`Latin America` in data)
3. Europe
4. Asia
5. Oceania

Use `region_labeller` for displayed names. Do not alphabetise continental
regions or use the data name `Latin America` in a publication-facing label.
The legacy internal `region` column stores continental-region membership.

## Region order, labels, and colours

Use the display order in `data_climate_zones`, not alphabetical order, whenever regions form figure columns, x-axis labels, or legends. Use `resolve_climatezone_label()` for the short labels and `palette_ecozones` for the colours.

| Order | Region class label | Short label | Colour |
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

Region colours are categorical colours. The legacy internal `climatezone`
column stores the present-day Köppen–Geiger class used to define each region
within a continental region. They must not be repurposed for human, climate,
time, space, zero, or missing-data encodings.

## Applying the geographical system

- Keep the configured continental-region and region order even if a subset of
  categories is absent; drop only categories that have no relevant data.
- Use short labels where panel width is constrained. Use the full region label in legends or where space permits.
- Ensure that a region colour, its full label, and its abbreviation agree wherever they appear in the same figure.
