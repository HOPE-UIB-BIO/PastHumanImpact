#' @title Add climatezone factor columns
#' @description
#' Recode climatezone names, cast to canonical factor levels, and append
#' compact climatezone labels.
#' @param data_source Data frame with column `climatezone`.
#' @return Data frame with `climatezone` and `climatezone_label` as factors.
add_climatezone_as_factor <- function(data_source) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    "climatezone" %in% names(data_source),
    msg = "`data_source` must contain `climatezone`."
  )

  assertthat::assert_that(
    exists("data_climate_zones"),
    msg = "`data_climate_zones` must be available in the session."
  )

  assertthat::assert_that(
    "climatezone_label" %in% names(data_climate_zones),
    msg = "`data_climate_zones` must contain `climatezone_label`."
  )

  res_data <-
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
      climatezone_label = get_climatezone_label(as.character(climatezone)),
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
    )

  return(res_data)
}
