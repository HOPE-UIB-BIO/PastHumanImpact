#' @title Build single-proxy H1 model specifications from profiles
#' @description
#' Translate single-proxy H1 analysis profiles into model specifications with
#' one human predictor block each, its age window, and its spatial support.
#' @param data_profiles
#' Analysis-profile rows with columns `profile_id`, `analytical_unit`,
#' `human_proxy`, `proxy_support`, and `age_window`.
#' @param data_windows
#' Output of `build_human_proxy_h1_windows()`.
#' @param event_columns
#' Character vector of event-group columns that form the human block of the
#' `"events"` proxy.
#' @return
#' A tibble with one row per profile and columns `model_id` (the profile
#' identifier), `model_label`, `human_candidates` (list), `human_preference`
#' (list), `exploratory`, `analytical_unit`, `human_proxy`, `proxy_support`,
#' `age_window`, `window_id`, `age_min_bp`, and `age_max_bp`.
#' @details
#' A gridded or SPD proxy has one human column named
#' `"<human_proxy>__<proxy_support>"`, as produced by
#' `prepare_human_proxy_predictor_columns()`. The events proxy uses
#' `event_columns`, in that preference order. `exploratory` is `FALSE` for
#' every profile. The function stops with an error when a profile has no
#' matching window.
#' @examples
#' \dontrun{
#' build_human_proxy_h1_specifications(profiles, windows, c("fi", "fc"))
#' }
build_human_proxy_h1_specifications <- function(
  data_profiles,
  data_windows,
  event_columns
) {
  vec_profile_columns <-
    c(
      "profile_id", "analytical_unit", "human_proxy", "proxy_support",
      "age_window"
    )

  assertthat::assert_that(
    is.data.frame(data_profiles),
    all(vec_profile_columns %in% names(data_profiles)),
    nrow(data_profiles) > 0L,
    !anyDuplicated(data_profiles[["profile_id"]]),
    is.data.frame(data_windows),
    all(c("human_proxy", "age_window", "window_id", "age_min_bp",
      "age_max_bp") %in% names(data_windows)),
    is.character(event_columns),
    length(event_columns) > 0L,
    msg = "Human-proxy specification inputs do not satisfy the contract."
  )

  data_specs <-
    data_profiles |>
    dplyr::select(dplyr::all_of(vec_profile_columns)) |>
    dplyr::left_join(
      data_windows,
      by = c("human_proxy", "age_window"),
      relationship = "many-to-one"
    )

  vec_without_window <-
    data_specs |>
    dplyr::filter(is.na(.data[["window_id"]])) |>
    dplyr::pull("profile_id")

  if (
    length(vec_without_window) > 0L
  ) {
    cli::cli_abort(
      c(
        "Some human-proxy profiles have no age window.",
        "x" = "Profiles: {.val {vec_without_window}}."
      )
    )
  }

  res <-
    data_specs |>
    dplyr::mutate(
      model_id = .data[["profile_id"]],
      model_label = stringr::str_c(
        .data[["human_proxy"]], " (", .data[["proxy_support"]], ", ",
        .data[["age_window"]], " window)"
      ),
      human_candidates = purrr::map2(
        .data[["human_proxy"]],
        .data[["proxy_support"]],
        .f = ~ {
          if (
            identical(.x, "events")
          ) {
            event_columns
          } else {
            stringr::str_c(.x, .y, sep = "__")
          }
        }
      ),
      human_preference = .data[["human_candidates"]],
      exploratory = FALSE
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        "model_id", "model_label", "human_candidates", "human_preference",
        "exploratory", "analytical_unit", "human_proxy", "proxy_support",
        "age_window", "window_id", "age_min_bp", "age_max_bp"
      ))
    )

  return(res)
}
