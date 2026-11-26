#' @title Validate the canonical human-proxy value table
#' @description
#' Enforce the public contract of `data_human_proxy_values`: column types,
#' unique keys, supports, and ages inside each proxy's declared window.
#' @param data_values
#' Data frame with `dataset_id`, `age_bp`, `proxy`, `support`, `radius_km`,
#' `value`, `value_transformed`, and `is_native_layer`.
#' @param data_proxy_windows
#' Data frame with `proxy`, `age_min_bp`, and `age_max_bp`, one row per
#' proxy.
#' @return
#' Invisibly returns `data_values`.
#' @details
#' The function aborts when:
#' - `dataset_id`, `proxy`, or `support` is not character, or `age_bp`,
#'   `radius_km`, `value`, or `value_transformed` is not numeric, or
#'   `is_native_layer` is not logical or contains `NA`;
#' - a `dataset_id` x `age_bp` x `proxy` x `support` key is duplicated;
#' - a proxy has no declared window, or an age lies outside its window
#'   (values are never extrapolated beyond the declared coverage);
#' - `support` is not `cell`, `radius_<r>km`, or `spd_buffer`, or
#'   `radius_km` does not match it (`NA` for `cell`, `r` for
#'   `radius_<r>km`, positive and finite for `spd_buffer`);
#' - exactly one of `value` and `value_transformed` is `NA`.
#' @examples
#' \dontrun{
#' validate_human_proxy_values(data_values, data_proxy_windows)
#' }
validate_human_proxy_values <- function(
  data_values,
  data_proxy_windows
) {
  vec_columns <-
    c(
      "dataset_id",
      "age_bp",
      "proxy",
      "support",
      "radius_km",
      "value",
      "value_transformed",
      "is_native_layer"
    )

  assertthat::assert_that(
    is.data.frame(data_values),
    all(vec_columns %in% names(data_values)),
    is.data.frame(data_proxy_windows),
    all(
      c("proxy", "age_min_bp", "age_max_bp") %in% names(data_proxy_windows)
    ),
    !anyDuplicated(data_proxy_windows[["proxy"]]),
    msg = "Human-proxy value validation inputs do not satisfy the contract."
  )

  assertthat::assert_that(
    is.character(data_values[["dataset_id"]]),
    is.numeric(data_values[["age_bp"]]),
    is.character(data_values[["proxy"]]),
    is.character(data_values[["support"]]),
    is.numeric(data_values[["radius_km"]]),
    is.numeric(data_values[["value"]]),
    is.numeric(data_values[["value_transformed"]]),
    is.logical(data_values[["is_native_layer"]]),
    !anyNA(data_values[["is_native_layer"]]),
    msg = "Human-proxy values do not satisfy the column-type contract."
  )

  n_duplicated_keys <-
    data_values |>
    dplyr::select(
      dplyr::all_of(c("dataset_id", "age_bp", "proxy", "support"))
    ) |>
    duplicated() |>
    sum()

  if (
    n_duplicated_keys > 0L
  ) {
    cli::cli_abort(
      c(
        "Human-proxy values contain duplicate keys.",
        "x" = "{n_duplicated_keys} duplicated dataset-age-proxy-support keys."
      )
    )
  }

  data_windows_checked <-
    data_values |>
    dplyr::distinct(.data[["proxy"]], .data[["age_bp"]]) |>
    dplyr::left_join(
      data_proxy_windows,
      by = dplyr::join_by("proxy"),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(
      is.na(.data[["age_min_bp"]]) |
        .data[["age_bp"]] < .data[["age_min_bp"]] |
        .data[["age_bp"]] > .data[["age_max_bp"]]
    )

  if (
    nrow(data_windows_checked) > 0L
  ) {
    vec_outside <-
      data_windows_checked |>
      dplyr::mutate(
        label = stringr::str_c(.data[["proxy"]], " at ", .data[["age_bp"]])
      ) |>
      dplyr::pull("label")

    cli::cli_abort(
      c(
        "Human-proxy ages lie outside the declared window of their proxy.",
        "x" = "{vec_outside}"
      )
    )
  }

  mat_support_radius <-
    stringr::str_match(
      string = data_values[["support"]],
      pattern = "^radius_([0-9.]+)km$"
    )

  vec_support_radius <-
    as.numeric(mat_support_radius[, 2])

  vec_support_valid <-
    dplyr::case_when(
      data_values[["support"]] == "cell" ~
        is.na(data_values[["radius_km"]]),
      data_values[["support"]] == "spd_buffer" ~
        is.finite(data_values[["radius_km"]]) &
          data_values[["radius_km"]] > 0,
      !is.na(vec_support_radius) ~
        is.finite(data_values[["radius_km"]]) &
          abs(data_values[["radius_km"]] - vec_support_radius) < 1e-9,
      .default = FALSE
    )

  if (
    !all(vec_support_valid)
  ) {
    vec_invalid <-
      data_values[["support"]][!vec_support_valid] |>
      unique()

    cli::cli_abort(
      c(
        "Human-proxy supports or their radii are invalid.",
        "x" = "Invalid support values: {vec_invalid}",
        "i" = stringr::str_c(
          "Use 'cell' (radius NA), 'radius_<r>km' (radius r), or ",
          "'spd_buffer' (positive radius)."
        )
      )
    )
  }

  vec_missing_mismatch <-
    is.na(data_values[["value"]]) !=
    is.na(data_values[["value_transformed"]])

  if (
    any(vec_missing_mismatch)
  ) {
    cli::cli_abort(
      c(
        "Human-proxy values and transformed values are missing in",
        " " = "different rows ({sum(vec_missing_mismatch)} rows)."
      )
    )
  }

  return(invisible(data_values))
}
