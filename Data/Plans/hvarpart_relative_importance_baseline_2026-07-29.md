# HVarPart Relative-Importance Baseline

This snapshot was recorded from commit
`9f84fd2b54875010f3ac245f46f047d642a9ce6c` before implementing issue #333.

## Target audit

| Analysis | Model rows | Complete | Positive total | Any negative individual | Positive total with negative individual |
| --- | ---: | ---: | ---: | ---: | ---: |
| H1 spatial SPD | 1,262 | 1,156 | 1,118 | 265 | 227 |
| H1 spatial events | 1,262 | 263 | 257 | 34 | 28 |
| H1 temporal SPD | 85 | 85 | 84 | 9 | 8 |
| H1 temporal events | 85 | 76 | 76 | 10 | 10 |
| H2 SPD | 31 | 31 | 31 | 0 | 0 |

The H1 manifest contained 17 targets and the H2 manifest contained 5 targets.
Both manifests loaded successfully from a fresh R process.

After adding only downstream targets, the H1 manifest contains 28 targets and
the H2 manifest contains 10 targets. A focused `tar_make()` was intentionally
not allowed to proceed: the external stores were written by `{targets}` 1.2.2,
whereas the available project library loads `{targets}` 1.12.0, which marks the
cached fitted targets as outdated across that compatibility boundary. The
downstream exporters instead read the cached fitted targets without mutation,
and the same commands are represented in the new manifests. Re-running the new
targets should wait until the repository's `{targets}`/`renv` mismatch is
resolved deliberately.

## Environment

The project reports that its library is out of sync before the already
installed project library is supplied. Baseline validation reused that library
without changing `renv/library_list.lock` or adding dependencies.

## Artifact SHA-256 hashes

| Artifact | SHA-256 |
| --- | --- |
| `Outputs/Figures/H1/Spatial/human_climate_balance_time_and_space_controlled.png` | `16CD6D4C339A3C41C6AA5C1951E124306CB4F1CA4180D4DA3AFF36C95A3A5B15` |
| `Outputs/Figures/H1/Spatial/human_climate_balance_time_and_space_controlled.pdf` | `BB06DC4B37E23FB12666D46B224ADD8A0F1AC55E5B65C7E4D58CBEB2F39B05EE` |
| `Outputs/Figures/H1/Temporal/HVarPart/human_climate_space_zero_truncated_hierarchical_composition.png` | `CA40D22A30331E80B268FF6C05460784FD375740AAD4433577E30370A8C8179F` |
| `Outputs/Figures/H1/Temporal/HVarPart/human_climate_space_zero_truncated_hierarchical_composition.pdf` | `975B1DF1BF6A4426A946EA6F660CDBE30CCEDF85A6A5993B62000F7ADFFFD3A6` |
| `Outputs/Figures/H2/Interrelationships/predictor_interrelationships.png` | `07075BB47808BFFAC880586CB8A52F2EF3BA3CBC2CD2FA22C6348267D73EEB47` |
| `Outputs/Figures/H2/Interrelationships/predictor_interrelationships.pdf` | `9CF3A84E632A59BB10A672B5853628563CDFD7FBE7E2E3E87092F436E20B5F87` |
| `Outputs/Figures/H1/Dataset_trends/HVarPart_core_temporal_examples.png` | `2D5F1FC5D9B2606A4C50C5514D09302F152A104E071CB9FB506B17CA30B73F0F` |
| `Outputs/Figures/H1/Dataset_trends/HVarPart_core_temporal_examples.pdf` | `B386B547C3D2D39DAB1EAEF78AD8B3B7A45725CF27D0F7C5271DEAD917C7B562` |
| `Outputs/Figures/H1/Spatial/pap_collinearity_hvarpart_influence.png` | `E21525E467AD835D15DC792198D8FFD4A6BEF3F29C9D31CF9614AE29286C17F6` |
| `Outputs/Figures/H1/Spatial/pap_collinearity_hvarpart_influence.pdf` | `4C6B5F4E82868E0DCBF57C078355362CE1700CD2DF0E0184A8059523565AA5C1` |
| `Outputs/Figures/H1/Spatial/human_climate_balance_reduced_predictors.png` | `4B41BF04AAE043A78CB6C10A3D2554153804338C3DA28832D2D670D58A68006A` |
| `Outputs/Figures/H1/Spatial/human_climate_balance_reduced_predictors.pdf` | `B93D1A3A9DB4DEA381A07555F331CD905C171768B929FCDB2719386BDCD962F6` |
