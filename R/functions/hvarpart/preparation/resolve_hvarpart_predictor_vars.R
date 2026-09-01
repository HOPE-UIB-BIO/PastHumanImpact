#' Resolve fixed or region-specific HVarPart predictor groups
#'
#' @param predictor_vars A named predictor list or a function accepting
#'   `region` and `available_columns` and returning such a list.
#' @param region Character scalar identifying the model's region.
#' @param available_columns Character vector of columns available to the fit.
#'
#' @return A named list with non-overlapping `human` and `climate` vectors.
#'
#' @export
resolve_hvarpart_predictor_vars <-
  function(predictor_vars, region, available_columns) {
    resolved <-
      if (is.function(predictor_vars)) {
        predictor_vars(
          region = region,
          available_columns = available_columns
        )
      } else {
        predictor_vars
      }

    if (
      !is.list(resolved) ||
        !all(c("human", "climate") %in% names(resolved)) ||
        !is.character(resolved[["human"]]) ||
        !is.character(resolved[["climate"]])
    ) {
      cli::cli_abort(
        "Resolved predictors must contain character human and climate sets."
      )
    }

    duplicated_predictors <- c(
      resolved[["human"]][duplicated(resolved[["human"]])],
      resolved[["climate"]][duplicated(resolved[["climate"]])],
      intersect(resolved[["human"]], resolved[["climate"]])
    ) |>
      unique()
    if (length(duplicated_predictors) > 0L) {
      cli::cli_abort(
        c(
          "Predictor mappings must be unique and non-overlapping.",
          "x" = "Duplicated predictors: {.val {duplicated_predictors}}."
        )
      )
    }

    missing_predictors <- setdiff(
      c(resolved[["human"]], resolved[["climate"]]),
      available_columns
    )
    if (length(missing_predictors) > 0L) {
      cli::cli_abort(
        c(
          "Resolved predictors are missing from the model data.",
          "x" = "Missing: {.val {missing_predictors}}."
        )
      )
    }

    list(
      human = resolved[["human"]],
      climate = resolved[["climate"]]
    )
  }
