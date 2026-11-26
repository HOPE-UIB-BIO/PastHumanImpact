#' @title Prepare record histories for single-proxy H1 runs
#' @description
#' Combine pollen-assemblage properties, climate and event predictors, and
#' wide human-proxy columns into one history per pollen record and age
#' window, with the same record filter and age collapse as the H1 inputs.
#' @param data_properties
#' Nested properties with columns `dataset_id` and `data_merge` (one data
#' frame per record with `age` and the response columns).
#' @param data_predictors
#' Nested predictors with columns `dataset_id` and `data_merge` (one data
#' frame per record with `age` and the predictor columns).
#' @param data_proxy_columns
#' Output of `prepare_human_proxy_predictor_columns()`.
#' @param data_meta
#' Record metadata with columns `dataset_id`, `region`, and
#' `data_publicity`.
#' @param data_windows
#' Data frame with columns `window_id`, `age_min_bp`, and `age_max_bp`; one
#' row per distinct window.
#' @param response_vars
#' Character vector of response columns.
#' @param predictor_vars
#' Character vector of predictor columns taken from `data_predictors`.
#' @return
#' A list with `data`, a tibble with columns `window_id`, `age_min_bp`,
#' `age_max_bp`, `dataset_id` (character), and `data_merge` (one data frame
#' per record and window, ordered by age), and `audit`, the age-collapse audit
#' of `aggregate_hvar_dataset_ages()`.
#' @details
#' Records are filtered with `prepare_filtered_hvarpart_data()` and
#' `remove_private = TRUE` over the union of all windows, properties and
#' predictors are joined by age with `prepare_combined_data()`, and the proxy
#' columns are left-joined by record and age, so ages without a proxy value
#' keep `NA`. Repeated ages are collapsed once, then each window keeps the
#' ages within its bounds. Records with no age in a window are dropped from
#' that window.
#' @examples
#' \dontrun{
#' prepare_human_proxy_h1_record_units(
#'   properties, predictors, proxy_columns, meta, windows, "n0", "temp_annual"
#' )
#' }
prepare_human_proxy_h1_record_units <- function(
  data_properties,
  data_predictors,
  data_proxy_columns,
  data_meta,
  data_windows,
  response_vars,
  predictor_vars
) {
  assertthat::assert_that(
    is.data.frame(data_properties),
    all(c("dataset_id", "data_merge") %in% names(data_properties)),
    is.data.frame(data_predictors),
    all(c("dataset_id", "data_merge") %in% names(data_predictors)),
    is.data.frame(data_proxy_columns),
    all(c("dataset_id", "age") %in% names(data_proxy_columns)),
    !anyDuplicated(data_proxy_columns[c("dataset_id", "age")]),
    is.data.frame(data_meta),
    all(c("dataset_id", "region", "data_publicity") %in% names(data_meta)),
    is.data.frame(data_windows),
    all(c("window_id", "age_min_bp", "age_max_bp") %in%
      names(data_windows)),
    nrow(data_windows) > 0L,
    !anyDuplicated(data_windows[["window_id"]]),
    is.character(response_vars),
    length(response_vars) > 0L,
    is.character(predictor_vars),
    length(predictor_vars) > 0L,
    msg = "Human-proxy record-unit inputs do not satisfy the contract."
  )

  age_min_all <-
    min(data_windows[["age_min_bp"]])

  age_max_all <-
    max(data_windows[["age_max_bp"]])

  data_properties_window <-
    prepare_filtered_hvarpart_data(
      data_source = data_properties,
      data_meta = data_meta,
      age_from = age_min_all,
      age_to = age_max_all,
      remove_private = TRUE
    )

  data_predictors_window <-
    prepare_filtered_hvarpart_data(
      data_source = data_predictors,
      data_meta = data_meta,
      age_from = age_min_all,
      age_to = age_max_all,
      remove_private = TRUE
    )

  data_combined <-
    prepare_combined_data(
      data_source_properties = data_properties_window,
      data_source_predictors = data_predictors_window
    ) |>
    dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]]))

  data_proxy_nested <-
    data_proxy_columns |>
    dplyr::mutate(
      dataset_id = as.character(.data[["dataset_id"]]),
      age = as.numeric(.data[["age"]])
    ) |>
    tidyr::nest(proxy_values = -dplyr::all_of("dataset_id"))

  vec_proxy_columns <-
    setdiff(names(data_proxy_columns), c("dataset_id", "age"))

  # Zero-row template, so records without proxy values still receive every
  #    proxy column (as NA) from the join.
  data_proxy_empty <-
    data_proxy_columns |>
    dplyr::mutate(age = as.numeric(.data[["age"]])) |>
    dplyr::select(dplyr::all_of(c("age", vec_proxy_columns))) |>
    dplyr::slice(0)

  data_with_all_columns <-
    data_combined |>
    dplyr::left_join(
      data_proxy_nested,
      by = "dataset_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      data_merge = purrr::map2(
        .x = .data[["data_merge"]],
        .y = .data[["proxy_values"]],
        .f = ~ {
          data_proxy_record <-
            if (
              is.null(.y)
            ) {
              data_proxy_empty
            } else {
              .y
            }

          .x |>
            dplyr::left_join(
              data_proxy_record,
              by = "age",
              relationship = "many-to-one"
            )
        }
      )
    ) |>
    dplyr::select(dplyr::all_of(c("dataset_id", "data_merge")))

  res_collapse <-
    aggregate_hvar_dataset_ages(
      data_source = data_with_all_columns,
      response_vars = response_vars,
      predictor_vars = unique(c(predictor_vars, vec_proxy_columns))
    )

  data_units <-
    data_windows |>
    dplyr::select(
      dplyr::all_of(c("window_id", "age_min_bp", "age_max_bp"))
    ) |>
    purrr::pmap(
      .f = ~ {
        window_id <- ..1
        age_min_bp <- ..2
        age_max_bp <- ..3

        res_collapse[["data"]] |>
          dplyr::mutate(
            data_merge = purrr::map(
              .data[["data_merge"]],
              .f = ~ .x |>
                dplyr::filter(
                  .data[["age"]] >= age_min_bp,
                  .data[["age"]] <= age_max_bp
                ) |>
                dplyr::arrange(.data[["age"]])
            )
          ) |>
          dplyr::filter(purrr::map_int(.data[["data_merge"]], nrow) > 0L) |>
          dplyr::mutate(
            window_id = window_id,
            age_min_bp = age_min_bp,
            age_max_bp = age_max_bp,
            .before = 1L
          )
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::select(
      dplyr::all_of(c(
        "window_id", "age_min_bp", "age_max_bp", "dataset_id", "data_merge"
      ))
    )

  res <-
    list(
      data = data_units,
      audit = res_collapse[["audit"]]
    )

  return(res)
}
