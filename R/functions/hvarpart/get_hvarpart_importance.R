#' @title Extract HVarPart predictor importance
#' @description
#' Extract predictor importance percentages and total explained variation from
#' nested HVarPart results for each dataset.
#' @param data_hvarpart Data frame containing `dataset_id` and nested `varhp`
#' results.
#' @return A tibble with `dataset_id`, `predictor`, `importance_percent`, and
#' `total_explained_variation`, and `n_hvarpart_results` columns. Duplicate
#' dataset-predictor results are consolidated using their arithmetic means.
#' @examples
#' \dontrun{
#' data_importance <-
#'   get_hvarpart_importance(data_hvarpart = data_h1_results)
#' }
get_hvarpart_importance <- function(data_hvarpart) {
  assertthat::assert_that(
    is.data.frame(data_hvarpart),
    all(c("dataset_id", "varhp") %in% names(data_hvarpart)),
    is.list(data_hvarpart[["varhp"]]),
    msg = "HVarPart data must contain dataset IDs and nested results."
  )

  data_importance <-
    data_hvarpart %>%
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      importance = purrr::map(
        .data[["varhp"]],
        .f = ~ {
          if (
            !is.list(.x) ||
              !all(c("summary_table", "varhp_output") %in% names(.x))
          ) {
            return(tibble::tibble())
          }

          summary_table <-
            .x[["summary_table"]]
          varhp_output <-
            .x[["varhp_output"]]

          if (
            !is.list(varhp_output) ||
              !"Total_explained_variation" %in% names(varhp_output)
          ) {
            return(tibble::tibble())
          }

          total_explained_variation <-
            varhp_output[["Total_explained_variation"]]

          if (
            !is.data.frame(summary_table) ||
              !all(
                c("predictor", "I.perc(%)") %in% names(summary_table)
              ) ||
              length(total_explained_variation) != 1L
          ) {
            return(tibble::tibble())
          }

          res_summary <-
            summary_table %>%
            dplyr::transmute(
              predictor = as.character(.data[["predictor"]]),
              importance_percent = as.numeric(.data[["I.perc(%)"]]),
              total_explained_variation = as.numeric(
                total_explained_variation
              )
            )

          return(res_summary)
        }
      )
    ) %>%
    tidyr::unnest(cols = importance) %>%
    dplyr::filter(
      is.finite(.data[["importance_percent"]]),
      is.finite(.data[["total_explained_variation"]])
    ) %>%
    dplyr::group_by(
      .data[["dataset_id"]],
      .data[["predictor"]]
    ) %>%
    dplyr::summarise(
      importance_percent = mean(.data[["importance_percent"]]),
      total_explained_variation = mean(
        .data[["total_explained_variation"]]
      ),
      n_hvarpart_results = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data[["dataset_id"]]) %>%
    dplyr::mutate(
      total_explained_variation = mean(
        .data[["total_explained_variation"]]
      )
    ) %>%
    dplyr::ungroup()

  assertthat::assert_that(
    nrow(data_importance) > 0L,
    msg = "No valid HVarPart importance results were found."
  )

  return(data_importance)
}
