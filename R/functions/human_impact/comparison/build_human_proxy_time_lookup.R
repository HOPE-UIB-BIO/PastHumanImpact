#' @title Build source-layer ages for external human proxies
#' @description
#' Build the documented layer ordering for the KK10 annual land-use product or
#' the 75-layer HYDE 3.2 population product.
#' @param source_id Character scalar. One of `kk10` or `hyde_3_2`.
#' @param n_layers Integer number of layers in the source raster.
#' @return Tibble with `source_id`, `layer_index`, and `age_bp`.
#' @details
#' HYDE calendar years are converted to years before 1950. The HYDE sequence
#' contains no calendar year zero: 1 CE follows 1000 BCE in the coarse part of
#' the source product.
#' @examples
#' lookup <- build_human_proxy_time_lookup("kk10", 7901L)
build_human_proxy_time_lookup <- function(source_id, n_layers) {
  assertthat::assert_that(
    assertthat::is.string(source_id),
    source_id %in% c("kk10", "hyde_3_2"),
    is.numeric(n_layers),
    length(n_layers) == 1L,
    is.finite(n_layers),
    n_layers == as.integer(n_layers),
    msg = "Human-proxy source and layer count are invalid."
  )

  if (
    identical(source_id, "kk10")
  ) {
    assertthat::assert_that(
      n_layers == 7901L,
      msg = "KK10 must contain 7,901 annual layers from 8000 to 100 BP."
    )

    vec_age_bp <-
      seq(from = 8000L, to = 100L, by = -1L)
  } else {
    assertthat::assert_that(
      n_layers == 75L,
      msg = "HYDE 3.2 must contain the documented 75 time layers."
    )

    vec_calendar_year <-
      c(
        seq(from = -10000L, to = -1000L, by = 1000L),
        1L,
        seq(from = 100L, to = 1700L, by = 100L),
        seq(from = 1710L, to = 1990L, by = 10L),
        seq(from = 2000L, to = 2017L, by = 1L)
      )

    vec_age_bp <-
      1950L - vec_calendar_year
  }

  res_lookup <-
    tibble::tibble(
      source_id = source_id,
      layer_index = seq_along(vec_age_bp),
      age_bp = as.numeric(vec_age_bp)
    )

  return(res_lookup)
}
