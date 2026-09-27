#' @title Summarise spatial HVarPart models with local selections
#' @description Apply the canonical spatial result summariser by model and
#' retain model metadata on every exported table.
#' @param data_results Output of `fit_local_spatial_hvarpart_models()`.
#' @return A named list of bound canonical spatial summary tables.
#' @examples
#' \dontrun{summarise_local_spatial_hvarpart_models(results)}
summarise_local_spatial_hvarpart_models <- function(data_results) {
  assertthat::assert_that(
    is.data.frame(data_results),
    all(c("model_id", "region", "age", "result") %in% names(data_results)),
    msg = "Local spatial HVarPart summaries do not satisfy the contract."
  )
  split_results <- split(data_results, data_results[["model_id"]])
  summaries <- purrr::imap(split_results, .f = ~ {
    data_model <- .x
    model_id <- .y
    canonical_input <- data_model |>
      dplyr::transmute(
        analysis = stringr::str_c(
          "temporal_human_proxy_collinearity_managed", model_id, sep = "__"
        ),
        region = .data[["region"]], age = .data[["age"]],
        result = .data[["result"]]
      )
    summarise_spatial_hvarpart_results(canonical_input)
  })
  table_names <- c(
    "status", "selection", "dbmem_diagnostics", "components",
    "unique_adjusted_r2", "residual_moran", "remaining_spatial_test"
  )
  res <- purrr::map(table_names, .f = ~ {
    table_name <- .x
    purrr::imap_dfr(summaries, .f = ~ {
      summary <- .x
      model_id <- .y
      summary[[table_name]] |>
        dplyr::mutate(model_id = model_id, .before = 1L)
    })
  }) |>
    rlang::set_names(table_names)

  return(res)
}
