# get scores dbrda
#' @title Get tidy scores from dbRDA model
#' @description Extract tidy ordination scores from a score-able dbRDA object.
#' @param dbrda_mod A dbRDA model object or `NULL`.
#' @return Tibble of tidy scores, or `NA` when `dbrda_mod` is `NULL`.
get_scores_dbrda <- function(dbrda_mod) {
  assertthat::assert_that(
    is.null(dbrda_mod) || !is.atomic(dbrda_mod),
    msg = "`dbrda_mod` must be `NULL` or a non-atomic model-like object."
  )

  if (!is.null(dbrda_mod)) {
    dbrda_score <-
      vegan::scores(dbrda_mod, tidy = TRUE) %>%
      dplyr::as_tibble()

    return(dbrda_score)
  } else {
    return(NA)
  }
}
