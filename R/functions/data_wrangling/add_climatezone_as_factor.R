add_climatezone_as_factor <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      climatezone = dplyr::case_when(
        .default = climatezone,
        climatezone == "Cold_Without_dry_season_Cold_Summer" ~ "Cold - Cold Summer",
        climatezone == "Cold_Without_dry_season_Warm_Summer" ~ "Cold - Warm Summer",
        climatezone == "Cold_Without_dry_season_Hot_Summer" ~ "Cold - Hot Summer",
        climatezone == "Cold_Dry_Winter" ~ "Cold - Dry Winter",
        climatezone == "Cold_Dry_Summer" ~ "Cold - Dry Summer",
        climatezone == "Temperate_Without_dry_season" ~ "Temperate",
        climatezone == "Temperate_Dry_Winter" ~ "Temperate - Dry Winter",
        climatezone == "Temperate_Dry_Summer" ~ "Temperate - Dry Summer"
      ),
      climatezone = factor(
        climatezone,
        levels = data_climate_zones$climatezone_label # [config criteria]
      ),
      climatezone_label = get_climatezone_label(climatezone),
      climatezone_label = factor(
        climatezone_label,
        levels = c(
          "POL",
          "CCS", "CWS", "CHS", "CDW", "CDS",
          "TMP", "TDW", "TDS",
          "TRO",
          "ARD"
        )
      )
    ) %>%
    return()
}
