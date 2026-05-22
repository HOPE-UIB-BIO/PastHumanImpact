#' @title Get short climatezone labels
#' @description Map long climatezone labels to short code labels.
#' @param x Character vector of climatezone labels.
#' @return Character vector with mapped short labels.
get_climatezone_label <- function(x) {
  assertthat::assert_that(
    is.character(x),
    msg = "`x` must be a character vector."
  )

  res_label <-
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

  return(res_label)
}
