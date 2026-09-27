#' @title Prepare matched human-proxy values for a spatial atlas
#' @description
#' Convert the exact within-dataset sensitivity inputs to a long table for
#' mapping square-root SPD, KK10 land-use fraction, and square-root HYDE at
#' pollen-sequence locations. Colour limits are fixed within each proxy over
#' all ages and capped at a documented upper quantile for legibility.
#' @param data_within Nested within-dataset input with `dataset_id` and
#'   `data_merge` columns.
#' @param data_metadata Dataset metadata containing unique coordinates.
#' @param age_min Youngest included age in cal yr BP.
#' @param age_max Oldest included age in cal yr BP.
#' @param colour_quantile Upper quantile used as the fixed colour maximum.
#' @return A named list with `values`, `scales`, and `coverage` tables.
#' @examples
#' \dontrun{
#' prepare_human_proxy_spatial_atlas_data(within_inputs, metadata)
#' }
prepare_human_proxy_spatial_atlas_data <- function(
  data_within,
  data_metadata,
  age_min = 2000,
  age_max = 8000,
  colour_quantile = 0.99
) {
  required_metadata <- c("dataset_id", "long", "lat", "region")
  assertthat::assert_that(
    is.data.frame(data_within),
    all(c("dataset_id", "data_merge") %in% names(data_within)),
    is.data.frame(data_metadata),
    all(required_metadata %in% names(data_metadata)),
    is.numeric(age_min), length(age_min) == 1L,
    is.numeric(age_max), length(age_max) == 1L,
    age_min < age_max,
    is.numeric(colour_quantile), length(colour_quantile) == 1L,
    colour_quantile > 0, colour_quantile <= 1,
    msg = "Human-proxy spatial-atlas inputs do not satisfy the contract."
  )

  duplicate_metadata <- data_metadata |>
    dplyr::count(.data[["dataset_id"]]) |>
    dplyr::filter(.data[["n"]] > 1L)
  if (nrow(duplicate_metadata) > 0L) {
    cli::cli_abort(
      "Spatial-atlas metadata must contain one row per `dataset_id`."
    )
  }

  proxy_columns <- c("spd_sqrt", "kk10_fraction", "hyde_sqrt")
  values_wide <- data_within |>
    tidyr::unnest(cols = dplyr::all_of("data_merge"))
  assertthat::assert_that(
    all(c("dataset_id", "age", proxy_columns) %in% names(values_wide)),
    msg = "Nested spatial-atlas inputs lack the required proxy columns."
  )

  duplicate_keys <- values_wide |>
    dplyr::count(.data[["dataset_id"]], .data[["age"]]) |>
    dplyr::filter(.data[["n"]] > 1L)
  if (nrow(duplicate_keys) > 0L) {
    cli::cli_abort(
      "Spatial-atlas inputs contain duplicate `dataset_id`-`age` keys."
    )
  }

  coordinates <- data_metadata |>
    dplyr::select(dplyr::all_of(required_metadata))
  values <- values_wide |>
    dplyr::filter(dplyr::between(.data[["age"]], age_min, age_max)) |>
    dplyr::select(
      dplyr::all_of(c("dataset_id", "age", proxy_columns))
    ) |>
    dplyr::left_join(
      coordinates,
      by = "dataset_id",
      relationship = "many-to-one"
    )
  if (
    nrow(values) == 0L ||
      any(!is.finite(values[["long"]])) ||
      any(!is.finite(values[["lat"]]))
  ) {
    cli::cli_abort(
      "Every spatial-atlas observation must have finite coordinates."
    )
  }

  proxy_labels <- c(
    spd_sqrt = "sqrt(SPD)",
    kk10_fraction = "KK10 land-use fraction",
    hyde_sqrt = "sqrt(HYDE population)"
  )
  values <- values |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(proxy_columns),
      names_to = "proxy",
      values_to = "value"
    ) |>
    dplyr::filter(is.finite(.data[["value"]])) |>
    dplyr::mutate(
      proxy = factor(.data[["proxy"]], levels = proxy_columns),
      proxy_label = factor(
        unname(proxy_labels[as.character(.data[["proxy"]])]),
        levels = unname(proxy_labels)
      )
    )

  scales <- values |>
    dplyr::summarise(
      value_min = min(.data[["value"]]),
      value_max = max(.data[["value"]]),
      colour_max = as.numeric(stats::quantile(
        .data[["value"]], colour_quantile, names = FALSE, na.rm = TRUE
      )),
      .by = c("proxy", "proxy_label")
    ) |>
    dplyr::mutate(
      colour_max = pmax(.data[["colour_max"]], .Machine$double.eps),
      colour_quantile = colour_quantile
    )

  values <- values |>
    dplyr::left_join(
      scales |>
        dplyr::select(dplyr::all_of(c("proxy", "colour_max"))),
      by = "proxy",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      colour_value = pmin(.data[["value"]], .data[["colour_max"]]),
      colour_capped = .data[["value"]] > .data[["colour_max"]]
    ) |>
    dplyr::arrange(
      dplyr::desc(.data[["age"]]),
      .data[["proxy"]],
      .data[["colour_value"]]
    )

  coverage <- values |>
    dplyr::summarise(
      observations = dplyr::n(),
      datasets = dplyr::n_distinct(.data[["dataset_id"]]),
      capped_observations = sum(.data[["colour_capped"]]),
      .by = c("age", "proxy", "proxy_label")
    ) |>
    dplyr::arrange(dplyr::desc(.data[["age"]]), .data[["proxy"]])

  return(list(values = values, scales = scales, coverage = coverage))
}
