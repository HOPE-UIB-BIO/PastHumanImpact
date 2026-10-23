#' @title Summarise temporal HVarPart models with local selections
#' @description Apply the canonical temporal result summariser by model and
#' retain model metadata on every exported table.
#' @param data_results Output of `fit_local_temporal_hvarpart_models()`.
#' @param analysis_prefix Prefix used for canonical analysis identifiers.
#' @return A named list of bound status, contribution, unique fraction, and
#'   residual Moran tables.
#' @examples
#' \dontrun{summarise_local_temporal_hvarpart_models(results)}
summarise_local_temporal_hvarpart_models <- function(
  data_results,
  analysis_prefix = "spatial_human_proxy_collinearity_managed"
) {
  assertthat::assert_that(
    is.data.frame(data_results),
    all(c("model_id", "dataset_id", "result") %in% names(data_results)),
    assertthat::is.string(analysis_prefix),
    msg = "Local temporal HVarPart summaries do not satisfy the contract."
  )
  split_results <- split(data_results, data_results[["model_id"]])
  summaries <- purrr::imap(split_results, .f = ~ {
    data_model <- .x
    model_id <- .y
    canonical_input <- data_model |>
      dplyr::select(dplyr::all_of(c("dataset_id", "result")))
    summarise_temporal_hvarpart_results(
      data_results = canonical_input,
      analysis = stringr::str_c(analysis_prefix, model_id, sep = "__")
    )
  })
  table_names <- c("status", "components", "unique_adjusted_r2", "residual_moran")
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
