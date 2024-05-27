get_climatezone_label <- function(x) {
  dplyr::case_when(
    .default = NA_character_,
    x == "Polar" ~ "POL",
    x == "Cold - Cold Summer" ~ "CCS",
    x == "Cold - Warm Summer" ~ "CWS",
    x == "Cold - Hot Summer" ~ "CHS",
    x == "Cold - Dry Winter" ~ "CDW",
    x == "Cold - Dry Summer" ~ "CDS",
    x == "Temperate" ~ "TMP",
    x == "Temperate - Dry Winter" ~ "TDW",
    x == "Temperate - Dry Summer" ~ "TDS",
    x == "Tropical" ~ "TRO",
    x == "Arid" ~ "ARD"
  )
}
