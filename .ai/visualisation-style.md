# Visualisation Style Guide

This guide defines the semantic colour system for figures in PastHumanImpact. It applies to new and modified figures, particularly the H1 human--climate analyses. Use it with `.ai/r-coding.md` and `.ai/analysis-structure.md`.

## Core principle

Colour represents a scientific role, not a plotting convenience. A role keeps the same colour across figures, tables, legends, maps, and manuscript assembly. Do not repurpose a colour already assigned to another role.

## Canonical semantic colours

| Role | Colour | Configuration source | Use |
| --- | --- | --- | --- |
| Human influence | gold `#C99B38` | `palette_predictors[["human"]]` | Human HVarPart contribution and the human end of a human--climate balance. |
| Climate influence | teal `#1F6F6F` | `palette_predictors[["climate"]]` | Climate HVarPart contribution and the climate end of a human--climate balance. |
| Time | dark blue `#1337A3` | `paletete_age[["old"]]` | Time control, age direction, and time-specific HVarPart contribution. |
| Young-to-old time scale | light-to-dark blue | `paletete_age` | Ordered age legends, arrows, and other chronological scales. |
| Space | muted purple `#6F5B8B` | local structural-control palette | Spatial dbMEM control and spatial HVarPart contribution. |
| Space in a composition | light muted purple `#A79BB8` | local structural-control palette | Filled spatial segment when a lighter stack is needed. |
| Event-specific human series | orange `#DC702E` | local event palette | Human contribution for events only when it is paired with the SPD human series. |
| Neutral reference | grey `#636363` | `common_gray` | Zero, negative-endpoint, reference, and non-substantive annotations. |

Purple is reserved for space. Blue is reserved for time. In particular, do not use purple for a temporal control, age gradient, or temporal residual.

## Applying the palette

- Use `palette_predictors` and `paletete_age` rather than repeating their hexadecimal values in figure code.
- A local colour value is allowed only for a documented semantic variant, such as the lighter spatial stack or the event-specific human series. Name the argument or object after that role.
- Use gold and teal, with white at zero, for signed human--climate balances. Do not introduce time or space colours into this two-ended scale.
- For a one-component signed spatial profile, use a neutral negative endpoint, white at zero, and the component colour for positive values. The map points, distribution, and legend must use the same scale.
- For zero-truncated stacked HVarPart compositions, use human, climate, and the relevant structural control in the same order as the legend. The structural group remains a control, not a causal competitor in the figure wording.
- Keep signed hierarchical contributions, zero-truncated compositions, and unique adjusted R-squared fractions visually distinct. Their colours may be shared; their scale, title, and legend must state the measure.

## Legends and supporting encodings

- A legend must name the scientific role, not only the variable code.
- Do not rely on colour alone when a figure contrasts proxy types. Use the established SPD/event labels and, where appropriate, line type or the paired-bar layout as a second cue.
- Use neutral greys for frames, basemaps, zero lines, and unavailable data so they do not compete with the scientific palette.
- Climate-zone colours come only from `palette_ecozones`; do not reuse a human, climate, time, or space colour for climate zones.

## Figure review checklist

Before saving a changed figure, verify that:

1. each substantive role follows the canonical palette;
2. purple appears only for space and blue only for time;
3. every map, distribution, stack, and legend agrees on the role-to-colour mapping;
4. signed scales visibly identify zero and do not assign a substantive hue to negative values of a one-component profile; and
5. the rendered figure remains interpretable in greyscale through labels, ordering, and non-colour cues.
