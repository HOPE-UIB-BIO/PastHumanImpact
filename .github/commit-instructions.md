# Commit Message Instructions

Return exactly one commit message line. No body, bullets, quotes, code fences,
labels, or period.

Format:

```text
<subject>: <short summary>
```

Keep the line under 72 characters.

## Subjects

Use the narrowest meaningful subject.

For one function, use the function name:

- get_spd(): validate empty radiocarbon input

For one script or pipeline, use the file stem:

- 03_combine_spd: update combined SPD input paths
- 01_pipeline_pollen_data: correct record filtering target

For a coherent analysis area, use the analysis name:

- SPD calculation: update radiocarbon merge logic
- Predictor models: adjust posterior export
- HVAR analysis: update summary tables
- Manuscript: clarify reviewer response plan

For repository support files, use a short conventional label:

- agents: tighten instruction routing
- renv: update package lockfile
- data: update derived input files
- docs: update setup notes
- tests: add function edge cases
- vscode: update workspace settings

## Wording

Start the summary with a specific verb such as `add`, `adjust`, `correct`,
`remove`, `replace`, `split`, `switch`, or `update`.

Do not use `feat`, `feature`, `fix`, or `enhance`.
