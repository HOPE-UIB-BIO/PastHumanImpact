#' @title Summarise HVarPart commonality fractions
#' @description
#' Extract the commonality (variation-partitioning) fractions that
#' `fit_varhp()` stores in `varhp_output$Var.part`, one row per fraction and
#' fitted model, without changing any fitted object.
#' @param data_fits
#' A data frame with identifier columns and a list column of fitted results,
#' such as the output of `fit_local_temporal_hvarpart_models()`.
#' @param id_columns
#' Character vector of identifier columns copied to the output.
#' @param fit_element
#' Name of the `fit_varhp()` result inside each result object, for example
#' `"temporal_hvarpart"`.
#' @param result_col
#' Name of the list column holding the result objects.
#' @return
#' A tibble with the `id_columns` and `fraction_label` (trimmed rdacca.hp
#' label), `groups` (the predictor groups of the fraction joined by `&`, in
#' label order), `n_groups`, `fraction` (adjusted R-squared fraction),
#' `percent_total`, and `total_fraction` (the `Total` row, equal to the sum
#' of the fractions). The `Total` row itself is not returned as a fraction.
#' @details
#' Results whose `fit_element` is `NULL` (models that were not estimated) are
#' skipped. Labels and column names are padded by rdacca.hp and are trimmed
#' before matching. rdacca.hp rounds fractions to four decimals.
#' @examples
#' \dontrun{
#' summarise_hvarpart_commonality_fractions(
#'   data_fits = temporal_fits,
#'   id_columns = c("model_id", "dataset_id")
#' )
#' }
summarise_hvarpart_commonality_fractions <- function(
  data_fits,
  id_columns = c("model_id", "dataset_id"),
  fit_element = "temporal_hvarpart",
  result_col = "result"
) {
  assertthat::assert_that(
    is.data.frame(data_fits),
    is.character(id_columns),
    assertthat::is.string(fit_element),
    assertthat::is.string(result_col),
    all(c(id_columns, result_col) %in% names(data_fits)),
    is.list(data_fits[[result_col]]),
    msg = "Commonality extraction inputs do not satisfy the contract."
  )

  data_empty <-
    data_fits[0, id_columns, drop = FALSE] |>
    tibble::as_tibble() |>
    dplyr::mutate(
      fraction_label = character(),
      groups = character(),
      n_groups = integer(),
      fraction = numeric(),
      percent_total = numeric(),
      total_fraction = numeric()
    )

  list_fractions <-
    seq_len(nrow(data_fits)) |>
    purrr::map(
      .f = ~ {
        index_fit <- .x

        var_part <-
          data_fits[[result_col]][[index_fit]][[fit_element]][[
            "varhp_output"
          ]][["Var.part"]]

        if (
          is.null(var_part)
        ) {
          return(NULL)
        }

        mat_var_part <- as.matrix(var_part)

        vec_labels <- stringr::str_squish(rownames(mat_var_part))

        vec_columns <- stringr::str_squish(colnames(mat_var_part))

        if (
          !all(c("Fractions", "% Total") %in% vec_columns) ||
            !"Total" %in% vec_labels
        ) {
          cli::cli_abort(
            "Var.part of row {index_fit} lacks the expected fractions."
          )
        }

        vec_fraction <-
          as.numeric(mat_var_part[, match("Fractions", vec_columns)])

        vec_percent <-
          as.numeric(mat_var_part[, match("% Total", vec_columns)])

        flag_total <- vec_labels == "Total"

        vec_groups <-
          vec_labels[!flag_total] |>
          stringr::str_remove("^(Unique|Common) to ") |>
          stringr::str_replace_all(",\\s*and\\s+|,\\s*|\\s+and\\s+", "&")

        data_values <-
          tibble::tibble(
            fraction_label = vec_labels[!flag_total],
            groups = vec_groups,
            n_groups = as.integer(stringr::str_count(vec_groups, "&") + 1L),
            fraction = vec_fraction[!flag_total],
            percent_total = vec_percent[!flag_total],
            total_fraction = vec_fraction[flag_total][[1]]
          )

        data_ids <-
          data_fits[
            rep(index_fit, nrow(data_values)),
            id_columns,
            drop = FALSE
          ] |>
          tibble::as_tibble()

        dplyr::bind_cols(data_ids, data_values)
      }
    )

  res <-
    dplyr::bind_rows(
      data_empty,
      purrr::compact(list_fractions)
    )

  return(res)
}
