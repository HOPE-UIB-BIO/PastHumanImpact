#' @title Select the source layers that bracket each target age
#' @description
#' For each target age, select the nearest source layer at or younger than
#' it and the nearest at or older than it, so values can be interpolated
#' without extrapolation.
#' @param data_lookup
#' Data frame with `source_id`, `layer_index`, and `age_bp`, one row per
#' source layer, as from `build_human_proxy_time_lookup()`.
#' @param target_ages_bp
#' Numeric vector of finite target ages in years BP.
#' @return
#' Tibble with the selected layers and the columns `source_id`,
#' `source_layer_index` (the layer's index in the source), `layer_index`
#' (`1..n` in output order), and `age_bp`, ordered from oldest to youngest.
#' A target that equals a source age selects only that layer.
#' @details
#' A target age older than the oldest or younger than the youngest source
#' layer is an error, because it could only be extrapolated.
#' @examples
#' \dontrun{
#' select_human_proxy_bracketing_layers(lookup, seq(500, 11500, by = 500))
#' }
select_human_proxy_bracketing_layers <- function(
  data_lookup,
  target_ages_bp
) {
  assertthat::assert_that(
    is.data.frame(data_lookup),
    all(c("source_id", "layer_index", "age_bp") %in% names(data_lookup)),
    nrow(data_lookup) > 0L,
    !anyDuplicated(data_lookup[["age_bp"]]),
    is.numeric(target_ages_bp),
    length(target_ages_bp) > 0L,
    all(is.finite(target_ages_bp)),
    msg = "Bracketing-layer inputs do not satisfy the contract."
  )

  vec_source_ages <-
    data_lookup[["age_bp"]]

  vec_outside <-
    target_ages_bp[
      target_ages_bp > max(vec_source_ages) |
        target_ages_bp < min(vec_source_ages)
    ]

  if (
    length(vec_outside) > 0L
  ) {
    cli::cli_abort(
      c(
        "Target ages lie outside the source layers and would be extrapolated.",
        "x" = "Ages outside {min(vec_source_ages)}-{max(vec_source_ages)} BP:
          {vec_outside}"
      )
    )
  }

  vec_selected_ages <-
    target_ages_bp |>
    purrr::map(
      .f = ~ c(
        min(vec_source_ages[vec_source_ages >= .x]),
        max(vec_source_ages[vec_source_ages <= .x])
      )
    ) |>
    unlist() |>
    unique()

  res_layers <-
    data_lookup |>
    dplyr::filter(.data[["age_bp"]] %in% vec_selected_ages) |>
    dplyr::arrange(dplyr::desc(.data[["age_bp"]])) |>
    dplyr::mutate(
      source_layer_index = .data[["layer_index"]],
      layer_index = dplyr::row_number()
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("source_id", "source_layer_index", "layer_index", "age_bp")
      )
    )

  return(res_layers)
}
