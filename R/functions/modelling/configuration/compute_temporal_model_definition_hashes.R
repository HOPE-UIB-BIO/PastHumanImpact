#' @title Compute temporal-model definition hashes
#' @description
#' Add deterministic hashes for the fields that define temporal-model fits.
#' @param data_config Temporal model configuration data frame.
#' @param definition_columns Character vector of definition columns.
#' @param hash_column Character scalar output hash-column name.
#' @return Tibble containing `data_config` and `definition_hash`.
#' @examples
#' \dontrun{
#' config <- compute_temporal_model_definition_hashes(data_config = config)
#' }
compute_temporal_model_definition_hashes <- function(
  data_config,
  definition_columns = c(
    "model_id",
    "analysis",
    "variable",
    "region",
    "climatezone",
    "family_key",
    "model_profile",
    "formula_text",
    "input_data_hash",
    "configuration_reference_hash",
    "total_iterations",
    "min_iterations_per_chain",
    "max_chains",
    "adapt_delta",
    "max_treedepth",
    "seed_base",
    "seed_attempt",
    "sampling_seed"
  ),
  hash_column = "definition_hash"
) {
  assertthat::assert_that(
    is.data.frame(data_config),
    is.character(definition_columns),
    length(definition_columns) > 0L,
    all(definition_columns %in% names(data_config)),
    is.character(hash_column),
    length(hash_column) == 1L,
    !is.na(hash_column),
    nzchar(hash_column),
    msg = "Temporal model definitions are missing required hash columns."
  )

  list_definition_rows <-
    data_config |>
    dplyr::select(dplyr::all_of(definition_columns)) |>
    purrr::transpose()

  vec_definition_hashes <-
    list_definition_rows |>
    purrr::map_chr(.f = rlang::hash)

  res_config <-
    data_config |>
    dplyr::mutate(
      !!hash_column := vec_definition_hashes
    )

  return(res_config)
}
