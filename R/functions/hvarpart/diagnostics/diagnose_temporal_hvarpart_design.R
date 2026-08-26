#' @title Diagnose a within-dataset temporal HVarPart design
#' @description
#' Remove unusable response and predictor columns, retain complete rows, and
#' enforce unique-age and residual-degree-of-freedom requirements.
#' @param data_source One dataset-level data frame containing a time predictor.
#' @param response_vars Candidate response columns.
#' @param predictor_vars Named human, climate, and time predictor groups.
#' @param age_col Age column.
#' @param min_unique_ages Minimum unique ages.
#' @param min_residual_df Minimum residual degrees of freedom.
#' @return A list with prepared data, active variables, diagnostics, and status.
#' @examples
#' \dontrun{
#' diagnose_temporal_hvarpart_design(
#'   data_source = dataset_data,
#'   response_vars = c("n0", "n1"),
#'   predictor_vars = list(
#'     human = "spd",
#'     climate = "temperature",
#'     time = "time"
#'   )
#' )
#' }
diagnose_temporal_hvarpart_design <- function(
  data_source,
  response_vars,
  predictor_vars,
  age_col = "age",
  min_unique_ages = 10L,
  min_residual_df = 5L
) {
  required_columns <-
    unique(c(age_col, response_vars, unlist(predictor_vars)))
  assertthat::assert_that(
    is.data.frame(data_source),
    all(required_columns %in% names(data_source)),
    is.list(predictor_vars),
    identical(
      sort(names(predictor_vars)),
      c("climate", "human", "time")
    ),
    is.numeric(min_unique_ages),
    min_unique_ages >= 3L,
    is.numeric(min_residual_df),
    min_residual_df >= 1L,
    msg = "Temporal design inputs do not satisfy the required contract."
  )

  active_responses <-
    response_vars |>
    purrr::keep(
      .p = ~ {
        vec_values <- data_source[[.x]]
        all(is.finite(vec_values)) &&
          dplyr::n_distinct(vec_values) > 1L
      }
    )
  active_predictors <-
    predictor_vars |>
    purrr::map(
      .f = ~ .x |>
        purrr::keep(
          .p = ~ {
            vec_values <- data_source[[.x]]
            vec_finite <- vec_values[is.finite(vec_values)]
            length(vec_finite) == length(vec_values) &&
              dplyr::n_distinct(vec_finite) > 1L
          }
        )
    )
  model_columns <-
    unique(c(age_col, active_responses, unlist(active_predictors)))
  data_model <-
    data_source |>
    dplyr::select(dplyr::all_of(model_columns)) |>
    dplyr::filter(
      stats::complete.cases(
        dplyr::across(dplyr::everything())
      )
    )
  mat_design <-
    data_model |>
    dplyr::select(dplyr::all_of(unlist(active_predictors))) |>
    as.matrix() |>
    cbind(intercept = 1)
  design_rank <- qr(mat_design)[["rank"]]
  design_full_rank <- design_rank == ncol(mat_design)
  residual_df <- nrow(data_model) - design_rank
  n_unique_ages <- dplyr::n_distinct(data_model[[age_col]])
  missing_groups <-
    names(active_predictors)[purrr::map_int(active_predictors, length) == 0L]
  status <-
    dplyr::case_when(
      length(active_responses) == 0L ~ "no_response_variation",
      length(missing_groups) > 0L ~ "missing_predictor_group",
      n_unique_ages < min_unique_ages ~ "insufficient_unique_ages",
      !design_full_rank ~ "rank_deficient",
      residual_df < min_residual_df ~ "insufficient_residual_df",
      .default = "estimable"
    )

  res <-
    list(
      status = status,
      data = data_model,
      response_vars = active_responses,
      predictor_vars = active_predictors,
      n_rows = nrow(data_model),
      n_unique_ages = n_unique_ages,
      design_rank = design_rank,
      design_full_rank = design_full_rank,
      residual_df = residual_df,
      missing_groups = missing_groups
    )

  return(res)
}
