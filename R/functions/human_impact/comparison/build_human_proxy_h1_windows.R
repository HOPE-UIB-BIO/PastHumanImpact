#' @title Build the age windows of single-proxy H1 runs
#' @description
#' Combine the common-cohort window with each human proxy's native window
#' into one lookup table of age windows for the single-proxy H1 runs.
#' @param data_proxy_windows
#' Data frame with one row per gridded or SPD proxy and columns `proxy`,
#' `age_min_bp`, and `age_max_bp` (the native window in years BP).
#' @param common_window_bp
#' Numeric vector of length two: youngest and oldest age (years BP) of the
#' common-cohort window shared by every proxy.
#' @param events_window_bp
#' Numeric vector of length two: youngest and oldest age (years BP) of the
#' native window of the expert-coded events.
#' @return
#' A tibble with one row per human proxy and window, and columns
#' `human_proxy`, `age_window` (`"common_cohort"` or `"native"`), `window_id`,
#' `age_min_bp`, and `age_max_bp`. `window_id` is
#' `"<age_window>_<age_min_bp>_<age_max_bp>"`, so proxies with the same window
#' share one identifier.
#' @details
#' The events proxy (`"events"`) is added with `events_window_bp`. The function
#' stops with an error when the common window is not inside every native
#' window, because the common cohort must be observed by every proxy.
#' @examples
#' build_human_proxy_h1_windows(
#'   data_proxy_windows = tibble::tibble(
#'     proxy = "kk10", age_min_bp = 500, age_max_bp = 8000
#'   )
#' )
build_human_proxy_h1_windows <- function(
  data_proxy_windows,
  common_window_bp = c(2000, 8000),
  events_window_bp = c(500, 11500)
) {
  assertthat::assert_that(
    is.data.frame(data_proxy_windows),
    all(c("proxy", "age_min_bp", "age_max_bp") %in%
      names(data_proxy_windows)),
    nrow(data_proxy_windows) > 0L,
    !anyDuplicated(data_proxy_windows[["proxy"]]),
    !"events" %in% data_proxy_windows[["proxy"]],
    is.numeric(common_window_bp),
    length(common_window_bp) == 2L,
    all(is.finite(common_window_bp)),
    common_window_bp[1] < common_window_bp[2],
    is.numeric(events_window_bp),
    length(events_window_bp) == 2L,
    all(is.finite(events_window_bp)),
    events_window_bp[1] < events_window_bp[2],
    msg = "Human-proxy windows require valid proxy windows and age bounds."
  )

  data_native <-
    data_proxy_windows |>
    dplyr::transmute(
      human_proxy = as.character(.data[["proxy"]]),
      age_min_bp = as.numeric(.data[["age_min_bp"]]),
      age_max_bp = as.numeric(.data[["age_max_bp"]])
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        human_proxy = "events",
        age_min_bp = events_window_bp[1],
        age_max_bp = events_window_bp[2]
      )
    )

  vec_outside <-
    data_native |>
    dplyr::filter(
      .data[["age_min_bp"]] > common_window_bp[1] |
        .data[["age_max_bp"]] < common_window_bp[2]
    ) |>
    dplyr::pull("human_proxy")

  if (
    length(vec_outside) > 0L
  ) {
    cli::cli_abort(
      c(
        "The common window is not inside every native window.",
        "x" = "Proxies not covering it: {.val {vec_outside}}."
      )
    )
  }

  data_common <-
    data_native |>
    dplyr::mutate(
      age_min_bp = common_window_bp[1],
      age_max_bp = common_window_bp[2]
    )

  res <-
    dplyr::bind_rows(
      data_common |>
        dplyr::mutate(age_window = "common_cohort"),
      data_native |>
        dplyr::mutate(age_window = "native")
    ) |>
    dplyr::mutate(
      window_id = stringr::str_c(
        .data[["age_window"]],
        .data[["age_min_bp"]],
        .data[["age_max_bp"]],
        sep = "_"
      )
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        "human_proxy", "age_window", "window_id", "age_min_bp", "age_max_bp"
      ))
    ) |>
    dplyr::arrange(.data[["human_proxy"]], .data[["age_window"]])

  return(res)
}
