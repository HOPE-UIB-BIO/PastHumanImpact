#' @title Prepare wide human-proxy predictor columns
#' @description
#' Turn the long human-proxy data layer into one column per proxy and
#' spatial support, ready to join to the H1 record histories.
#' @param data_proxy_values
#' Long proxy table (`data_human_proxy_values`) with columns `dataset_id`,
#' `age_bp`, `proxy`, `support`, and `value_transformed`.
#' @param data_column_specs
#' Data frame with columns `proxy` and `support` naming the proxy and
#' support combinations to keep.
#' @param data_unavailable
#' Optional data frame with columns `dataset_id` and `proxy`. Values of these
#' proxies for these records are set to `NA` at every support, for records
#' whose stored value is a fill value rather than an observation.
#' @return
#' A tibble with columns `dataset_id` (character), `age` (numeric), and one
#' numeric column per row of `data_column_specs`, named
#' `"<proxy>__<support>"` and holding `value_transformed`.
#' @details
#' Rows are every record and age with a value for at least one kept
#' combination; a combination without a value at that age is `NA`. The
#' function stops with an error when a record, age, proxy, and support occur
#' more than once.
#' @examples
#' \dontrun{
#' prepare_human_proxy_predictor_columns(
#'   values, tibble::tibble(proxy = "spd", support = "spd_buffer")
#' )
#' }
prepare_human_proxy_predictor_columns <- function(
  data_proxy_values,
  data_column_specs,
  data_unavailable = NULL
) {
  assertthat::assert_that(
    is.data.frame(data_proxy_values),
    all(c("dataset_id", "age_bp", "proxy", "support", "value_transformed")
      %in% names(data_proxy_values)),
    is.data.frame(data_column_specs),
    all(c("proxy", "support") %in% names(data_column_specs)),
    nrow(data_column_specs) > 0L,
    is.null(data_unavailable) ||
      (is.data.frame(data_unavailable) &&
        all(c("dataset_id", "proxy") %in% names(data_unavailable))),
    msg = "Human-proxy predictor-column inputs do not satisfy the contract."
  )

  data_specs <-
    data_column_specs |>
    dplyr::distinct(.data[["proxy"]], .data[["support"]])

  data_kept <-
    data_proxy_values |>
    dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
    dplyr::inner_join(data_specs, by = c("proxy", "support"))

  data_duplicates <-
    data_kept |>
    dplyr::count(
      .data[["dataset_id"]], .data[["age_bp"]], .data[["proxy"]],
      .data[["support"]]
    ) |>
    dplyr::filter(.data[["n"]] > 1L)

  if (
    nrow(data_duplicates) > 0L
  ) {
    cli::cli_abort(
      "A record, age, proxy, and support occur more than once."
    )
  }

  data_mask <-
    if (
      is.null(data_unavailable)
    ) {
      tibble::tibble(
        dataset_id = character(),
        proxy = character(),
        is_unavailable = logical()
      )
    } else {
      data_unavailable |>
        dplyr::transmute(
          dataset_id = as.character(.data[["dataset_id"]]),
          proxy = as.character(.data[["proxy"]]),
          is_unavailable = TRUE
        ) |>
        dplyr::distinct()
    }

  res <-
    data_kept |>
    dplyr::left_join(data_mask, by = c("dataset_id", "proxy")) |>
    dplyr::mutate(
      value = dplyr::if_else(
        .data[["is_unavailable"]] %in% TRUE,
        NA_real_,
        as.numeric(.data[["value_transformed"]])
      ),
      column_name = stringr::str_c(
        .data[["proxy"]], .data[["support"]],
        sep = "__"
      ),
      age = as.numeric(.data[["age_bp"]])
    ) |>
    dplyr::select(
      dplyr::all_of(c("dataset_id", "age", "column_name", "value"))
    ) |>
    tidyr::pivot_wider(
      names_from = "column_name",
      values_from = "value"
    ) |>
    dplyr::arrange(.data[["dataset_id"]], .data[["age"]])

  vec_expected <-
    stringr::str_c(data_specs[["proxy"]], data_specs[["support"]], sep = "__")

  vec_absent <-
    setdiff(vec_expected, names(res))

  list_absent <-
    rlang::set_names(
      rep(list(NA_real_), length(vec_absent)),
      vec_absent
    )

  res_complete <-
    res |>
    dplyr::mutate(!!!list_absent) |>
    dplyr::select(dplyr::all_of(c("dataset_id", "age", vec_expected)))

  return(res_complete)
}
