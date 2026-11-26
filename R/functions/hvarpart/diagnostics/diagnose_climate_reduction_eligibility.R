#' @title Diagnose record eligibility with selected versus all climate
#' @description
#' For every time-controlled design, report the model status and residual
#' degrees of freedom with the shared climate selection and with all climate
#' candidates, without fitting any model.
#' @param data_designs
#' Output of `prepare_shared_climate_hvarpart_designs()` for records, with
#' columns `dataset_id`, `model_id`, `selection_status`, `data_merge`, and
#' `predictor_vars`.
#' @param response_vars
#' Character vector of response columns.
#' @param climate_candidates
#' Character vector of all climate candidates.
#' @param min_unique_ages
#' Minimum number of unique ages.
#' @param min_residual_df
#' Minimum residual degrees of freedom.
#' @param age_col
#' Age column of each record history.
#' @return
#' A tibble with two rows per design (`climate_set` `"selected"` and
#' `"all_candidates"`) and columns `dataset_id`, `model_id`, `climate_set`,
#' `n_climate` (active climate columns), `status`, `residual_df`, and
#' `n_unique_ages`.
#' @details
#' The checks are those of `fit_temporal_hvarpart_dataset()` before fitting:
#' non-finite or repeated ages, too few unique ages, then
#' `diagnose_temporal_hvarpart_design()` with the human block, the climate
#' set, and scaled age as time. `status` is `"estimable"` when the design
#' passes. A design without a selected human block keeps its
#' `selection_status` for both climate sets; with the selected climate set,
#' any selection status other than `"eligible_for_design_check"` is kept.
#' @examples
#' \dontrun{
#' diagnose_climate_reduction_eligibility(designs, responses)
#' }
diagnose_climate_reduction_eligibility <- function(
  data_designs,
  response_vars,
  climate_candidates = c(
    "temp_annual", "temp_cold", "prec_summer", "prec_win"
  ),
  min_unique_ages = 10L,
  min_residual_df = 5L,
  age_col = "age"
) {
  assertthat::assert_that(
    is.data.frame(data_designs),
    all(c("dataset_id", "model_id", "selection_status", "data_merge",
      "predictor_vars") %in% names(data_designs)),
    is.character(response_vars),
    length(response_vars) > 0L,
    is.character(climate_candidates),
    length(climate_candidates) > 0L,
    msg = "Climate-reduction eligibility inputs do not satisfy the contract."
  )

  res <-
    seq_len(nrow(data_designs)) |>
    purrr::map(
      .f = ~ {
        index_design <- .x

        data_record <-
          data_designs[["data_merge"]][[index_design]]

        list_predictors <-
          data_designs[["predictor_vars"]][[index_design]]

        selection_status <-
          data_designs[["selection_status"]][[index_design]]

        list_climate_sets <-
          list(
            selected = list_predictors[["climate"]],
            all_candidates = climate_candidates
          )

        purrr::imap(
          list_climate_sets,
          .f = ~ {
            vec_climate <- .x

            climate_set <- .y

            vec_ages <-
              data_record[[age_col]]

            n_unique_ages <-
              dplyr::n_distinct(vec_ages[is.finite(vec_ages)])

            status_before_design <-
              dplyr::case_when(
                length(list_predictors[["human"]]) == 0L ~ selection_status,
                climate_set == "selected" &
                  selection_status != "eligible_for_design_check" ~
                  selection_status,
                any(!is.finite(vec_ages)) ~ "incomplete_ages",
                anyDuplicated(vec_ages) > 0L ~ "repeated_ages",
                n_unique_ages < min_unique_ages ~ "insufficient_unique_ages",
                .default = NA_character_
              )

            if (
              !is.na(status_before_design)
            ) {
              return(
                tibble::tibble(
                  climate_set = climate_set,
                  n_climate = NA_integer_,
                  status = status_before_design,
                  residual_df = NA_integer_,
                  n_unique_ages = as.integer(n_unique_ages)
                )
              )
            }

            data_time <-
              scale_temporal_age(
                data_source = data_record,
                age_col = age_col,
                output_col = "time"
              )

            list_design <-
              diagnose_temporal_hvarpart_design(
                data_source = data_time,
                response_vars = response_vars,
                predictor_vars = list(
                  human = list_predictors[["human"]],
                  climate = vec_climate,
                  time = "time"
                ),
                age_col = age_col,
                min_unique_ages = min_unique_ages,
                min_residual_df = min_residual_df
              )

            tibble::tibble(
              climate_set = climate_set,
              n_climate = length(list_design[["predictor_vars"]][["climate"]]),
              status = list_design[["status"]],
              residual_df = as.integer(list_design[["residual_df"]]),
              n_unique_ages = as.integer(list_design[["n_unique_ages"]])
            )
          }
        ) |>
          dplyr::bind_rows() |>
          dplyr::mutate(
            dataset_id = data_designs[["dataset_id"]][[index_design]],
            model_id = data_designs[["model_id"]][[index_design]],
            .before = 1L
          )
      }
    ) |>
    dplyr::bind_rows()

  return(res)
}
