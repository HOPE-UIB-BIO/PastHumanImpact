#' @title Prepare dataset groups for SPD calculation
#' @description
#' Expand filtered radiocarbon inputs to every metadata dataset, represent
#' datasets without nearby dates with an explicit empty radiocarbon table, and
#' split the result into deterministic one-dataset groups. Larger branches are
#' scheduled first to reduce the parallel calculation's long tail.
#' @param data_source Filtered radiocarbon input containing `dataset_id`,
#'   `curve_name`, and nested `rc` data.
#' @param data_meta Metadata containing every expected `dataset_id` and its
#'   assigned `curve_name`.
#' @return Named list of one-row tibbles ordered by decreasing radiocarbon-date
#'   count and then dataset identifier.
#' @examples
#' \dontrun{
#' groups <- prepare_spd_calculation_groups(data_filtered_c14, data_meta)
#' }
prepare_spd_calculation_groups <- function(data_source, data_meta) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(c("dataset_id", "curve_name", "rc") %in% names(data_source)),
    is.list(data_source[["rc"]]),
    nrow(data_source) > 0L,
    !anyDuplicated(data_source[["dataset_id"]]),
    is.data.frame(data_meta),
    all(c("dataset_id", "curve_name") %in% names(data_meta)),
    nrow(data_meta) > 0L,
    !anyDuplicated(data_meta[["dataset_id"]]),
    msg = "SPD calculation groups do not satisfy the required contract."
  )

  if (
    !all(data_source[["dataset_id"]] %in% data_meta[["dataset_id"]])
  ) {
    cli::cli_abort(
      "Filtered radiocarbon data contain datasets absent from metadata."
    )
  }

  data_expected <-
    data_meta |>
    dplyr::select(dplyr::all_of(c("dataset_id", "curve_name")))
  data_missing <-
    data_expected |>
    dplyr::anti_join(
      data_source |>
        dplyr::select(dplyr::all_of("dataset_id")),
      by = "dataset_id"
    ) |>
    dplyr::mutate(
      rc = rep(
        list(data_source[["rc"]][[1]][0, , drop = FALSE]),
        dplyr::n()
      )
    )
  data_ordered <-
    dplyr::bind_rows(data_source, data_missing) |>
    dplyr::mutate(
      n_radiocarbon_dates = purrr::map_int(.data[["rc"]], nrow)
    ) |>
    dplyr::arrange(
      dplyr::desc(.data[["n_radiocarbon_dates"]]),
      .data[["dataset_id"]]
    ) |>
    dplyr::select(-dplyr::all_of("n_radiocarbon_dates"))

  vec_dataset_ids <-
    as.character(data_ordered[["dataset_id"]])

  res_groups <-
    seq_len(nrow(data_ordered)) |>
    purrr::map(
      .f = ~ dplyr::slice(data_ordered, .x)
    ) |>
    rlang::set_names(vec_dataset_ids)

  return(res_groups)
}
