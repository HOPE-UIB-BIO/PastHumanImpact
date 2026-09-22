# Quarto manuscript scaffold

This project reconstructs the submitted manuscript from one Quarto entry point while keeping the scientific text divided into stable, reviewable sections.

## Render

From the repository root:

```powershell
quarto render Manuscript/article
```

The command creates:

- `rendered/Felde_Mottl_Flantua_et_al_HumanImpact_ms.pdf`
- `rendered/Felde_Mottl_Flantua_et_al_HumanImpact_ms.docx`

## Source layout

- `manuscript.qmd` defines metadata and assembles the complete document.
- `_sections/` contains the submitted wording in manuscript order.
- `figures/` contains the 13 figures extracted from the submitted Word file and renamed by meaning.
- `references/` contains citation-keyed records for the two continuous submitted reference groups.
- `filters/split-bibliography.lua` runs citation processing once and places references 1--50 and 51--75 at their original positions.
- `reference/submitted-style-reference.docx` is a content-free style reference derived from the submitted Word file.
- `styles/submission.tex` provides a corresponding source-like PDF style.
- `scripts/configure_reference_docx.py` reapplies the source-derived Word styles, A4 geometry, page numbering, and continuous line numbering to the DOCX reference file.

The legacy bibliography entries intentionally store each submitted reference string in the BibTeX `note` field. This preserves the exact submitted list while making all in-text citations key-based and renumberable. Records can be enriched with structured metadata during later revision work.

## Editing rules

Edit the relevant file in `_sections/`; do not paste reviewer-response text directly into the baseline. Replace a figure by keeping its semantic filename, or update the path and cross-reference identifier together. Keep main figures and tables on the built-in counters and extended items on the `extfig` and `exttbl` counters.

The submitted PDF in `Manuscript/_internal/Submission_Nature_Comm_EENV/` remains the authority for baseline wording, ordering, and visible content.
