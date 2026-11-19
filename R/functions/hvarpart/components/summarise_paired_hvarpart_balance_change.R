#' @title Summarise paired change in time-controlled HVarPart balances
#' @description
#' Pair the balance records of a reference and a comparison model on the
#' same records and summarise how the human, climate, and time
#' contributions, the signed difference, and the zero-truncated balance
#' change between them.
#' @param data_records
#' Matched balance records, such as the `records` element of
#' `prepare_matched_hvarpart_balance_records()`, with `model_id`, the
#' `id_column`, `human`, `climate`, `time`, `total_adjusted_r_squared`,
#' `signed_difference`, `signed_balance`, `zero_balance`, `signed_ranking`,
#' and `zero_ranking`.
#' @param reference_model_id
#' Identifier of the reference model.
#' @param comparison_model_id
#' Identifier of the comparison model.
#' @param id_column
#' Name of the record identifier column.
#' @return
#' A list with `pairs` (one row per record with `reference_*`,
#' `comparison_*`, and `change_*` columns for each value, and the logical
#' `signed_ranking_changed` and `zero_ranking_changed`), `quantity_summary`
#' (one row per value: `n_records` and the median, first, and third quartile
#' of the reference, comparison, and change), and `ranking_summary` (one row
#' per profile, `signed` and `zero_truncated`: `n_records`, the number of
#' records ranked human and climate in each model, and
#' `n_ranking_changed`). Every table carries `reference_model_id` and
#' `comparison_model_id`.
#' @details
#' Change is comparison minus reference. Both models must cover exactly the
#' same records; match them first with
#' `prepare_matched_hvarpart_balance_records()`.
#' @examples
#' \dontrun{
#' summarise_paired_hvarpart_balance_change(
#'   data_records = matched$records,
#'   reference_model_id = "spd__full_climate",
#'   comparison_model_id = "spd__filtered_climate"
#' )
#' }
summarise_paired_hvarpart_balance_change <- function(
  data_records,
  reference_model_id,
  comparison_model_id,
  id_column = "dataset_id"
) {
  vec_value_columns <-
    c(
      "human", "climate", "time", "total_adjusted_r_squared",
      "signed_difference", "signed_balance", "zero_balance"
    )

  vec_ranking_columns <-
    c("signed_ranking", "zero_ranking")

  assertthat::assert_that(
    is.data.frame(data_records),
    assertthat::is.string(id_column),
    all(c("model_id", id_column, vec_value_columns, vec_ranking_columns) %in%
      names(data_records)),
    assertthat::is.string(reference_model_id),
    assertthat::is.string(comparison_model_id),
    reference_model_id != comparison_model_id,
    all(c(reference_model_id, comparison_model_id) %in%
      data_records[["model_id"]]),
    msg = "Paired balance-change inputs do not satisfy the contract."
  )

  data_reference <-
    data_records |>
    dplyr::filter(.data[["model_id"]] == reference_model_id) |>
    dplyr::select(
      dplyr::all_of(c(id_column, vec_value_columns, vec_ranking_columns))
    )

  data_comparison <-
    data_records |>
    dplyr::filter(.data[["model_id"]] == comparison_model_id) |>
    dplyr::select(
      dplyr::all_of(c(id_column, vec_value_columns, vec_ranking_columns))
    )

  if (
    anyDuplicated(data_reference[[id_column]]) > 0L ||
      anyDuplicated(data_comparison[[id_column]]) > 0L ||
      !setequal(data_reference[[id_column]], data_comparison[[id_column]])
  ) {
    cli::cli_abort(c(
      "Both models must cover the same records exactly once.",
      "i" = "Match them first with prepare_matched_hvarpart_balance_records()."
    ))
  }

  data_joined <-
    data_reference |>
    dplyr::rename_with(
      .fn = ~ stringr::str_c("reference_", .x),
      .cols = -dplyr::all_of(id_column)
    ) |>
    dplyr::inner_join(
      data_comparison |>
        dplyr::rename_with(
          .fn = ~ stringr::str_c("comparison_", .x),
          .cols = -dplyr::all_of(id_column)
        ),
      by = id_column
    )

  data_changes <-
    vec_value_columns |>
    purrr::map(
      .f = ~ data_joined[[stringr::str_c("comparison_", .x)]] -
        data_joined[[stringr::str_c("reference_", .x)]]
    ) |>
    rlang::set_names(stringr::str_c("change_", vec_value_columns)) |>
    tibble::as_tibble()

  data_pairs <-
    dplyr::bind_cols(data_joined, data_changes) |>
    dplyr::mutate(
      signed_ranking_changed =
        .data[["reference_signed_ranking"]] !=
          .data[["comparison_signed_ranking"]],
      zero_ranking_changed =
        .data[["reference_zero_ranking"]] !=
          .data[["comparison_zero_ranking"]],
      reference_model_id = reference_model_id,
      comparison_model_id = comparison_model_id,
      .before = 1L
    )

  data_quantity_summary <-
    vec_value_columns |>
    purrr::map(
      .f = ~ {
        quantity_name <- .x

        list_values <-
          list(
            reference = data_pairs[[
              stringr::str_c("reference_", quantity_name)
            ]],
            comparison = data_pairs[[
              stringr::str_c("comparison_", quantity_name)
            ]],
            change = data_pairs[[stringr::str_c("change_", quantity_name)]]
          )

        list_values |>
          purrr::imap(
            .f = ~ tibble::tibble(
              "{.y}_median" := stats::median(.x, na.rm = TRUE),
              "{.y}_q25" := stats::quantile(
                .x, probs = 0.25, na.rm = TRUE, names = FALSE
              ),
              "{.y}_q75" := stats::quantile(
                .x, probs = 0.75, na.rm = TRUE, names = FALSE
              )
            )
          ) |>
          unname() |>
          dplyr::bind_cols() |>
          dplyr::mutate(
            quantity = quantity_name,
            n_records = nrow(data_pairs),
            .before = 1L
          )
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      reference_model_id = reference_model_id,
      comparison_model_id = comparison_model_id,
      .before = 1L
    )

  data_ranking_summary <-
    tibble::tibble(
      profile = c("signed", "zero_truncated"),
      ranking_column = vec_ranking_columns
    ) |>
    dplyr::mutate(
      n_records = nrow(data_pairs),
      reference_n_human = purrr::map_int(
        .data[["ranking_column"]],
        .f = ~ sum(data_pairs[[stringr::str_c("reference_", .x)]] == "human",
          na.rm = TRUE)
      ),
      reference_n_climate = purrr::map_int(
        .data[["ranking_column"]],
        .f = ~ sum(data_pairs[[stringr::str_c("reference_", .x)]] == "climate",
          na.rm = TRUE)
      ),
      comparison_n_human = purrr::map_int(
        .data[["ranking_column"]],
        .f = ~ sum(data_pairs[[stringr::str_c("comparison_", .x)]] == "human",
          na.rm = TRUE)
      ),
      comparison_n_climate = purrr::map_int(
        .data[["ranking_column"]],
        .f = ~ sum(
          data_pairs[[stringr::str_c("comparison_", .x)]] == "climate",
          na.rm = TRUE
        )
      ),
      n_ranking_changed = purrr::map_int(
        .data[["ranking_column"]],
        .f = ~ sum(
          data_pairs[[stringr::str_replace(.x, "ranking", "ranking_changed")]],
          na.rm = TRUE
        )
      ),
      reference_model_id = reference_model_id,
      comparison_model_id = comparison_model_id,
      .before = 1L
    ) |>
    dplyr::select(-dplyr::all_of("ranking_column"))

  res <-
    list(
      pairs = data_pairs,
      quantity_summary = data_quantity_summary,
      ranking_summary = data_ranking_summary
    )

  return(res)
}
