#' @title Fit spatial aggregations of local human-proxy balances
#' @description Apply the canonical spatial importance aggregation separately
#' to each proxy specification on the reconciled common cohort.
#' @param data_records Common-cohort dataset balance records with `model_id`.
#' @param seed Base random seed.
#' @param ... Arguments passed to `fit_spatial_importance()`.
#' @return A tibble with one aggregation result per model.
#' @examples
#' \dontrun{fit_human_proxy_spatial_balance_models(records)}
fit_human_proxy_spatial_balance_models <- function(
  data_records,
  seed = 1234L,
  ...
) {
  assertthat::assert_that(
    is.data.frame(data_records),
    all(c("model_id", "dataset_id", "zero_balance") %in% names(data_records)),
    msg = "Human-proxy spatial balance inputs do not satisfy the contract."
  )
  extra <- rlang::list2(...)
  model_ids <- unique(data_records$model_id)
  res <- tibble::tibble(model_id = model_ids) |>
    dplyr::mutate(
      result = purrr::map2(
        .data[["model_id"]], seq_along(model_ids),
        .f = ~ {
          model_id <- .x
          index <- .y
          rlang::exec(
            fit_spatial_importance,
            data_records = data_records |>
              dplyr::filter(.data[["model_id"]] == .env[["model_id"]]) |>
              dplyr::mutate(
                model_id = stringr::str_c(
                  .env[["model_id"]], .data[["dataset_id"]], sep = "|"
                )
              ),
            seed = as.integer(seed + index),
            !!!extra
          )
        }
      )
    )

  return(res)
}
