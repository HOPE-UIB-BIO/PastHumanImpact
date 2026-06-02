#' @title Extract ratio-based hvar importance
#' @description
#' Extract predictor-level importance from nested `varhp` outputs and compute
#' ratio-based importance aligned with Figure 2 logic (`ratio_ind`).
#' @param data_hvar Data frame with `dataset_id` and `varhp` list-column.
#' @param model_label Character scalar identifying the model version.
#' @return
#' Tibble with columns `dataset_id`, `predictor`, `model`,
#' `importance_ratio`, `sum_importance`, `individual`, and `unique`.
extract_hvar_importance <- function(data_hvar, model_label) {
  assertthat::assert_that(
    is.data.frame(data_hvar),
    msg = "`data_hvar` must be a data frame."
  )

  assertthat::assert_that(
    assertthat::has_name(data_hvar, c("dataset_id", "varhp")),
    msg = "`data_hvar` must contain `dataset_id` and `varhp` columns."
  )

  assertthat::assert_that(
    is.character(model_label),
    length(model_label) == 1,
    msg = "`model_label` must be a single character value."
  )

  res_importance <- data_hvar

  res_importance$summary_table <-
    purrr::map(
      .x = res_importance$varhp,
      .f = ~ {
        if (is.null(.x)) {
          return(
            tibble::tibble(
              predictor = c("human", "climate"),
              Unique = NA_real_,
              Individual = NA_real_
            )
          )
        }

        summary_table <-
          purrr::pluck(
            .x,
            "summary_table",
            .default = NULL
          )

        if (is.null(summary_table)) {
          return(
            tibble::tibble(
              predictor = c("human", "climate"),
              Unique = NA_real_,
              Individual = NA_real_
            )
          )
        }

        res_summary <-
          summary_table |>
          dplyr::select(
            dplyr::any_of(
              c(
                "predictor",
                "Unique",
                "Individual"
              )
            )
          )

        if (!"predictor" %in% names(res_summary)) {
          return(
            tibble::tibble(
              predictor = c("human", "climate"),
              Unique = NA_real_,
              Individual = NA_real_
            )
          )
        }

        return(res_summary)
      }
    )

  res_importance <-
    tidyr::unnest(
      data = res_importance,
      cols = c("summary_table")
    )

  sel_cols <-
    c(
      "dataset_id",
      "predictor",
      intersect(
        c("Unique", "Individual"),
        names(res_importance)
      )
    )

  res_importance <-
    res_importance[
      res_importance$predictor %in% c("human", "climate"),
      sel_cols,
      drop = FALSE
    ]

  if (!"Unique" %in% names(res_importance)) {
    res_importance$Unique <- NA_real_
  }

  if (!"Individual" %in% names(res_importance)) {
    res_importance$Individual <- NA_real_
  }

  names(res_importance)[names(res_importance) == "Unique"] <- "unique"
  names(res_importance)[names(res_importance) == "Individual"] <- "individual"

  # Align with Figure 2 logic: negatives are treated as near-zero before ratios.
  res_importance$unique[res_importance$unique < 0] <- 0.000001
  res_importance$individual[res_importance$individual < 0] <- 0.000001

  res_importance$sum_importance <-
    stats::ave(
      res_importance$individual,
      res_importance$dataset_id,
      FUN = function(x) sum(x, na.rm = TRUE)
    )

  res_importance$importance_ratio <-
    res_importance$individual / res_importance$sum_importance

  res_importance$importance_ratio[res_importance$sum_importance <= 0] <- NA_real_
  res_importance$model <- model_label

  res_importance <-
    dplyr::relocate(
      res_importance,
      dplyr::all_of(
        c(
          "dataset_id",
          "predictor",
          "model",
          "importance_ratio",
          "sum_importance",
          "individual",
          "unique"
        )
      )
    )

  res_importance <-
    tibble::as_tibble(res_importance)

  return(res_importance)
}
