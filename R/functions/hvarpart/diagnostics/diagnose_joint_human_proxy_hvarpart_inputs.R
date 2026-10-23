#' @title Diagnose joint human-proxy HVarPart inputs
#' @description
#' Audit temporal within-dataset and spatial region-by-age design matrices
#' before running the joint human-proxy sensitivity fits.
#' @param data_within_dataset Prepared nested within-dataset data.
#' @param data_time_slices Prepared nested region-by-age data.
#' @param response_vars Response variable names.
#' @param predictor_vars Named human and climate predictor groups.
#' @param min_unique_ages Minimum unique ages for temporal models.
#' @param min_temporal_residual_df Minimum temporal residual degrees of freedom.
#' @param min_spatial_residual_df Minimum spatial residual degrees of freedom.
#' @return A list containing `within_dataset` and `time_slices` audit tables.
#' @examples
#' \dontrun{
#' diagnose_joint_human_proxy_hvarpart_inputs(within, slices, responses, preds)
#' }
diagnose_joint_human_proxy_hvarpart_inputs <- function(
  data_within_dataset,
  data_time_slices,
  response_vars,
  predictor_vars,
  min_unique_ages = 10L,
  min_temporal_residual_df = 4L,
  min_spatial_residual_df = 10L
) {
  assertthat::assert_that(
    is.data.frame(data_within_dataset),
    all(c("dataset_id", "data_merge") %in% names(data_within_dataset)),
    is.data.frame(data_time_slices),
    all(c("region", "age", "data_merge") %in% names(data_time_slices)),
    is.character(response_vars),
    is.list(predictor_vars),
    identical(sort(names(predictor_vars)), c("climate", "human")),
    msg = "Joint human-proxy design diagnostics do not satisfy the contract."
  )

  predictor_vars_time <- c(predictor_vars, list(time = "time"))
  data_within_audit <-
    purrr::map2_dfr(
      data_within_dataset[["data_merge"]],
      data_within_dataset[["dataset_id"]],
      function(data_dataset, dataset_id) {
        ages <- data_dataset[["age"]]
        n_unique_ages <- dplyr::n_distinct(ages[is.finite(ages)])
        requested_columns <- unique(unlist(predictor_vars, use.names = FALSE))
        data_requested <-
          data_dataset |>
          dplyr::filter(
            stats::complete.cases(
              dplyr::across(
                dplyr::all_of(c("age", requested_columns))
              )
            )
          )
        requested_design <-
          if (
            nrow(data_requested) > 1L &&
              stats::sd(data_requested[["age"]]) > 0
          ) {
            cbind(
              intercept = 1,
              as.matrix(data_requested[requested_columns]),
              time = as.numeric(scale(data_requested[["age"]]))
            )
          } else {
            matrix(numeric(), nrow = nrow(data_requested), ncol = 0L)
          }
        requested_rank <-
          if (ncol(requested_design) > 0L) qr(requested_design)[["rank"]] else 0L
        requested_residual_df <- nrow(data_requested) - requested_rank
        requested_full_rank <-
          ncol(requested_design) > 0L && requested_rank == ncol(requested_design)
        requested_estimable <-
          nrow(data_requested) >= min_unique_ages &&
            requested_full_rank &&
            requested_residual_df >= min_temporal_residual_df
        initial_status <-
          dplyr::case_when(
            any(!is.finite(ages)) ~ "incomplete_ages",
            anyDuplicated(ages) > 0L ~ "repeated_ages",
            n_unique_ages < min_unique_ages ~ "insufficient_unique_ages",
            .default = NA_character_
          )
        if (!is.na(initial_status)) {
          return(
            tibble::tibble(
              dataset_id = as.character(dataset_id),
              status = initial_status,
              n_rows = nrow(data_dataset),
              n_unique_ages = n_unique_ages,
              design_rank = NA_integer_,
              design_full_rank = NA,
              residual_df = NA_integer_,
              requested_n_rows = nrow(data_requested),
              requested_design_rank = requested_rank,
              requested_design_columns = ncol(requested_design),
              requested_design_full_rank = requested_full_rank,
              requested_residual_df = requested_residual_df,
              requested_estimable = requested_estimable
            )
          )
        }

        diagnostic <-
          data_dataset |>
          scale_temporal_age(age_col = "age", output_col = "time") |>
          diagnose_temporal_hvarpart_design(
            response_vars = response_vars,
            predictor_vars = predictor_vars_time,
            age_col = "age",
            min_unique_ages = min_unique_ages,
            min_residual_df = min_temporal_residual_df
          )

        tibble::tibble(
          dataset_id = as.character(dataset_id),
          status = diagnostic[["status"]],
          n_rows = diagnostic[["n_rows"]],
          n_unique_ages = diagnostic[["n_unique_ages"]],
          design_rank = diagnostic[["design_rank"]],
          design_full_rank = diagnostic[["design_full_rank"]],
          residual_df = diagnostic[["residual_df"]],
          requested_n_rows = nrow(data_requested),
          requested_design_rank = requested_rank,
          requested_design_columns = ncol(requested_design),
          requested_design_full_rank = requested_full_rank,
          requested_residual_df = requested_residual_df,
          requested_estimable = requested_estimable
        )
      }
    )

  predictor_columns <- unique(unlist(predictor_vars, use.names = FALSE))
  data_spatial_audit <-
    purrr::pmap_dfr(
      data_time_slices[c("region", "age", "data_merge")],
      function(region, age, data_merge) {
        active_predictors <-
          predictor_vars |>
          purrr::map(
            ~ .x[purrr::map_lgl(
              .x,
              function(column) {
                values <- data_merge[[column]]
                finite <- values[is.finite(values)]
                length(finite) > 1L && dplyr::n_distinct(finite) > 1L
              }
            )]
          )
        active_columns <- unique(unlist(active_predictors, use.names = FALSE))
        required_columns <- unique(c(response_vars, active_columns))
        data_complete <-
          data_merge |>
          dplyr::filter(
            stats::complete.cases(
              dplyr::across(dplyr::all_of(required_columns))
            )
          )
        design <-
          cbind(
            intercept = 1,
            as.matrix(data_complete[active_columns])
          )
        design_rank <- qr(design)[["rank"]]
        residual_df <- nrow(data_complete) - design_rank
        missing_group <- any(purrr::map_int(active_predictors, length) == 0L)
        status <-
          dplyr::case_when(
            missing_group ~ "missing_predictor_group",
            design_rank < ncol(design) ~ "rank_deficient",
            residual_df < min_spatial_residual_df ~
              "insufficient_residual_df",
            .default = "estimable"
          )

        tibble::tibble(
          region = as.character(region),
          age = as.numeric(age),
          status = status,
          n_rows = nrow(data_complete),
          n_predictors_requested = length(predictor_columns),
          n_predictors_active = length(active_columns),
          design_rank = design_rank,
          design_columns = ncol(design),
          design_full_rank = design_rank == ncol(design),
          residual_df = residual_df
        )
      }
    )

  res <-
    list(
      within_dataset = data_within_audit,
      time_slices = data_spatial_audit
    )

  return(res)
}
