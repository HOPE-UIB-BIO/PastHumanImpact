#' @title Prepare human-proxy values in the canonical data-layer contract
#' @description
#' Standardise one proxy's record-level values to the canonical columns,
#' add the transformed value, and flag values that come from an exact
#' native source layer.
#' @param data_source
#' Data frame with `dataset_id`, `support`, `radius_km`, `age_bp`, and
#' `value`. Other columns are dropped.
#' @param proxy
#' Character scalar proxy name, for example `kk10`.
#' @param native_ages_bp
#' Numeric vector of the ages (years BP) of the proxy's source layers. A
#' value is native when its `age_bp` is one of them.
#' @param transformation
#' One of `identity` (default) or `square_root`, giving `value_transformed`.
#' @return
#' Tibble with the columns `dataset_id` (character), `age_bp`, `proxy`,
#' `support`, `radius_km`, `value`, `value_transformed`, and
#' `is_native_layer` (logical, never `NA`), in that order, sorted by
#' `dataset_id`, `support`, and decreasing `age_bp`. `NA` values stay `NA`
#' after transformation.
#' @details
#' A negative value with `transformation = "square_root"` is an error.
#' @examples
#' \dontrun{
#' prepare_human_proxy_values(
#'   data_source = data_kk10,
#'   proxy = "kk10",
#'   native_ages_bp = seq(8000, 100),
#'   transformation = "identity"
#' )
#' }
prepare_human_proxy_values <- function(
  data_source,
  proxy,
  native_ages_bp,
  transformation = "identity"
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(
      c("dataset_id", "support", "radius_km", "age_bp", "value") %in%
        names(data_source)
    ),
    assertthat::is.string(proxy),
    is.numeric(native_ages_bp),
    assertthat::is.string(transformation),
    transformation %in% c("identity", "square_root"),
    msg = "Human-proxy value inputs do not satisfy the contract."
  )

  vec_values <-
    as.numeric(data_source[["value"]])

  if (
    identical(transformation, "square_root") &&
      any(vec_values < 0, na.rm = TRUE)
  ) {
    cli::cli_abort(
      c(
        "Cannot take the square root of negative {proxy} values.",
        "i" = "Square-root transformation needs non-negative values."
      )
    )
  }

  res_values <-
    data_source |>
    dplyr::mutate(
      dataset_id = as.character(.data[["dataset_id"]]),
      age_bp = as.numeric(.data[["age_bp"]]),
      proxy = proxy,
      support = as.character(.data[["support"]]),
      radius_km = as.numeric(.data[["radius_km"]]),
      value = as.numeric(.data[["value"]]),
      value_transformed = if (
        identical(transformation, "square_root")
      ) {
        sqrt(.data[["value"]])
      } else {
        .data[["value"]]
      },
      is_native_layer = .data[["age_bp"]] %in% native_ages_bp
    ) |>
    dplyr::select(
      dplyr::all_of(
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
      )
    ) |>
    dplyr::arrange(
      .data[["dataset_id"]],
      .data[["support"]],
      dplyr::desc(.data[["age_bp"]])
    )

  return(res_values)
}
