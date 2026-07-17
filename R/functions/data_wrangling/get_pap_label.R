#' @title Get display labels for PAP variables
#' @description Map PAP variable names to concise figure labels.
#' @param x Character vector of PAP variable names.
#' @return Character vector with mapped labels. Unknown names are unchanged.
#' @examples
#' get_pap_label(c("n0", "roc"))
get_pap_label <- function(x) {
  assertthat::assert_that(
    is.character(x),
    msg = "`x` must be a character vector."
  )

  res_label <-
    dplyr::case_when(
      x == "n0" ~ "Taxonomic richness",
      x == "n1" ~ "Shannon diversity",
      x == "n2" ~ "Simpson diversity",
      x == "n1_minus_n2" ~ "Shannon - Simpson diversity",
      x == "n2_divided_by_n1" ~ "Simpson / Shannon diversity",
      x == "n1_divided_by_n0" ~ "Shannon diversity / richness",
      x == "roc" ~ "Rate of change",
      x == "dcca_axis_1" ~ "DCCA axis 1",
      x == "density_diversity" ~ "Diversity density",
      x == "density_turnover" ~ "Turnover density",
      .default = x
    )

  return(res_label)
}
