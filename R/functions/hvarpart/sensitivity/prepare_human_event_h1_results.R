#' Bind one table from human-event H1 scenario results
#'
#' @param data_results List of scenario result lists.
#' @param result_name Name of the table to extract.
#'
#' @return Combined data frame ordered by cohort and proxy variant.
#'
#' @export
prepare_human_event_h1_results <- function(data_results, result_name) {
  assertthat::assert_that(
    is.list(data_results),
    length(data_results) > 0L,
    assertthat::is.string(result_name),
    all(purrr::map_lgl(data_results, is.list)),
    all(purrr::map_lgl(
      data_results,
      ~ result_name %in% names(.x)
    )),
    msg = "Human-event branch results do not satisfy the contract."
  )

  data_results |>
    purrr::map(result_name) |>
    dplyr::bind_rows() |>
    dplyr::arrange(
      .data[["cohort"]],
      factor(
        .data[["proxy_variant"]],
        levels = c("spd", "spd_events", "events")
      )
    )
}
