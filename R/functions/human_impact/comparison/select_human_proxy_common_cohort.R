#' @title Select the common cohort of single-proxy H1 runs
#' @description
#' Keep the pollen records whose histories have a finite value for every
#' required human-proxy column at every age, so all common-cohort runs share
#' the same records.
#' @param data_record_units
#' Record histories for one window, with columns `dataset_id` and
#' `data_merge`.
#' @param required_columns
#' Character vector of proxy columns that must be complete.
#' @return
#' A list with `cohort`, a tibble with the column `dataset_id` of the
#' retained records, and `audit`, a tibble with one row per record and
#' columns `dataset_id`, `n_rows`, `is_complete`, and `incomplete_columns`
#' (the required columns that are missing or not finite at some age,
#' separated by `";"`, or `NA` when complete).
#' @details
#' A record with no rows is incomplete. A column missing from a record's
#' history counts as incomplete.
#' @examples
#' \dontrun{
#' select_human_proxy_common_cohort(units, c("spd__spd_buffer"))
#' }
select_human_proxy_common_cohort <- function(
  data_record_units,
  required_columns
) {
  assertthat::assert_that(
    is.data.frame(data_record_units),
    all(c("dataset_id", "data_merge") %in% names(data_record_units)),
    is.list(data_record_units[["data_merge"]]),
    !anyDuplicated(data_record_units[["dataset_id"]]),
    is.character(required_columns),
    length(required_columns) > 0L,
    msg = "Human-proxy common-cohort inputs do not satisfy the contract."
  )

  res_audit <-
    data_record_units |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      n_rows = purrr::map_int(.data[["data_merge"]], nrow),
      incomplete_columns = purrr::map_chr(
        .data[["data_merge"]],
        .f = ~ {
          data_record <- .x

          vec_incomplete <-
            required_columns |>
            purrr::keep(
              .p = ~ !.x %in% names(data_record) ||
                nrow(data_record) == 0L ||
                !all(is.finite(data_record[[.x]]))
            )

          if (
            length(vec_incomplete) == 0L
          ) {
            NA_character_
          } else {
            stringr::str_c(vec_incomplete, collapse = ";")
          }
        }
      ),
      is_complete = is.na(.data[["incomplete_columns"]])
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        "dataset_id", "n_rows", "is_complete", "incomplete_columns"
      ))
    )

  res_cohort <-
    res_audit |>
    dplyr::filter(.data[["is_complete"]]) |>
    dplyr::select(dplyr::all_of("dataset_id"))

  res <-
    list(
      cohort = res_cohort,
      audit = res_audit
    )

  return(res)
}
