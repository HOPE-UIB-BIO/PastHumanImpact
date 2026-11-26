#' @title Diagnose reproduction of a reference record-level H1 result
#' @description
#' Compare record-level human-climate balances of a new run with a reference
#' run record by record, and give every reference record an outcome: the
#' record is reproduced, differs, is explained as missing, or is unexplained.
#' @param data_records
#' Estimable record balances of the new run with columns `dataset_id`,
#' `human`, `climate`, `time`, `zero_balance`, and `signed_difference`.
#' @param data_status
#' Model statuses of the new run with columns `dataset_id`, `status`, and
#' `residual_df`.
#' @param data_reference
#' Reference records with columns `dataset_id`, `human_contribution`,
#' `climate_contribution`, `control_contribution`, `zero_balance`,
#' `signed_difference`, and `dominance` (`"Human"` or `"Climate"`).
#' @param data_reference_status
#' Reference statuses with columns `dataset_id` and `residual_df`.
#' @param data_cohort
#' Data frame with the column `dataset_id` of the new run's input cohort.
#' @param min_residual_df
#' Residual degree-of-freedom minimum of the new run.
#' @param tolerance
#' Largest absolute difference accepted as a reproduction.
#' @return
#' A list with `records`, one row per reference record with columns
#' `dataset_id`, `outcome`, `in_new_cohort`, `new_status`,
#' `reference_residual_df`, `new_residual_df`, `reference_zero_balance`,
#' `new_zero_balance`, `max_abs_difference`, `reference_dominance`, and
#' `new_dominance`, and `summary`, one row with the counts per outcome,
#' `n_reference`, `n_reference_human`, `n_compared`, `n_human_new`,
#' `n_human_reference_compared`, `max_abs_difference`, `tolerance`, and
#' `is_reproduced`.
#' @details
#' `outcome` is `"reproduced"` when the record is in the new run and the
#' largest absolute difference across human, climate, time, zero balance,
#' and signed difference is at most `tolerance`; `"differs"` when it is
#' larger; `"excluded_residual_df"` when the record is missing, is in the
#' new cohort, has the new status `"insufficient_residual_df"`, and its
#' reference residual df is below `min_residual_df`;
#' `"not_in_common_cohort"` when it is not in the new cohort;
#' `"estimated_without_balance"` when the record is missing although its new
#' model was estimated (no positive human or climate contribution, so no
#' zero-truncated balance); and `"unexplained"` otherwise. Dominance is
#' `"Human"` for a positive zero balance, `"Climate"` for a negative one, and
#' `"Tie"` for zero. `is_reproduced` is `TRUE` when at least one record is
#' compared and no record differs, is estimated without a balance, or is
#' unexplained.
#' @examples
#' \dontrun{
#' diagnose_human_proxy_reference_reproduction(
#'   records, status, reference, reference_status, cohort
#' )
#' }
diagnose_human_proxy_reference_reproduction <- function(
  data_records,
  data_status,
  data_reference,
  data_reference_status,
  data_cohort,
  min_residual_df = 5L,
  tolerance = 1e-8
) {
  assertthat::assert_that(
    is.data.frame(data_records),
    all(c("dataset_id", "human", "climate", "time", "zero_balance",
      "signed_difference") %in% names(data_records)),
    !anyDuplicated(data_records[["dataset_id"]]),
    is.data.frame(data_status),
    all(c("dataset_id", "status", "residual_df") %in% names(data_status)),
    is.data.frame(data_reference),
    all(c("dataset_id", "human_contribution", "climate_contribution",
      "control_contribution", "zero_balance", "signed_difference",
      "dominance") %in% names(data_reference)),
    !anyDuplicated(data_reference[["dataset_id"]]),
    is.data.frame(data_reference_status),
    all(c("dataset_id", "residual_df") %in% names(data_reference_status)),
    is.data.frame(data_cohort),
    "dataset_id" %in% names(data_cohort),
    is.numeric(tolerance),
    length(tolerance) == 1L,
    msg = "Reproduction inputs do not satisfy the contract."
  )

  vec_cohort <-
    as.character(data_cohort[["dataset_id"]])

  data_new <-
    data_records |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      new_human = .data[["human"]],
      new_climate = .data[["climate"]],
      new_time = .data[["time"]],
      new_zero_balance = .data[["zero_balance"]],
      new_signed_difference = .data[["signed_difference"]],
      is_in_new_run = TRUE
    )

  data_new_status <-
    data_status |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      new_status = .data[["status"]],
      new_residual_df = as.numeric(.data[["residual_df"]])
    ) |>
    dplyr::distinct(.data[["dataset_id"]], .keep_all = TRUE)

  data_compared <-
    data_reference |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      reference_human = .data[["human_contribution"]],
      reference_climate = .data[["climate_contribution"]],
      reference_time = .data[["control_contribution"]],
      reference_zero_balance = .data[["zero_balance"]],
      reference_signed_difference = .data[["signed_difference"]],
      reference_dominance = .data[["dominance"]]
    ) |>
    dplyr::left_join(
      data_reference_status |>
        dplyr::transmute(
          dataset_id = as.character(.data[["dataset_id"]]),
          reference_residual_df = as.numeric(.data[["residual_df"]])
        ),
      by = "dataset_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      data_new,
      by = "dataset_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      data_new_status,
      by = "dataset_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      in_new_cohort = .data[["dataset_id"]] %in% vec_cohort,
      is_in_new_run = .data[["is_in_new_run"]] %in% TRUE,
      max_abs_difference = pmax(
        abs(.data[["new_human"]] - .data[["reference_human"]]),
        abs(.data[["new_climate"]] - .data[["reference_climate"]]),
        abs(.data[["new_time"]] - .data[["reference_time"]]),
        abs(.data[["new_zero_balance"]] - .data[["reference_zero_balance"]]),
        abs(
          .data[["new_signed_difference"]] -
            .data[["reference_signed_difference"]]
        )
      ),
      outcome = dplyr::case_when(
        .data[["is_in_new_run"]] &
          .data[["max_abs_difference"]] <= tolerance ~ "reproduced",
        .data[["is_in_new_run"]] ~ "differs",
        !.data[["in_new_cohort"]] ~ "not_in_common_cohort",
        .data[["new_status"]] %in%
          c("estimated", "estimated_residual_temporal_dependence") ~
          "estimated_without_balance",
        .data[["new_status"]] %in% "insufficient_residual_df" &
          .data[["reference_residual_df"]] < min_residual_df ~
          "excluded_residual_df",
        .default = "unexplained"
      ),
      new_dominance = dplyr::case_when(
        !.data[["is_in_new_run"]] ~ NA_character_,
        .data[["new_zero_balance"]] > 0 ~ "Human",
        .data[["new_zero_balance"]] < 0 ~ "Climate",
        .default = "Tie"
      )
    )

  res_records <-
    data_compared |>
    dplyr::select(
      dplyr::all_of(c(
        "dataset_id", "outcome", "in_new_cohort", "new_status",
        "reference_residual_df", "new_residual_df", "reference_zero_balance",
        "new_zero_balance", "max_abs_difference", "reference_dominance",
        "new_dominance"
      ))
    )

  data_compared_only <-
    data_compared |>
    dplyr::filter(.data[["is_in_new_run"]])

  vec_outcomes <-
    c(
      "reproduced", "differs", "excluded_residual_df",
      "estimated_without_balance",
      "not_in_common_cohort", "unexplained"
    )

  list_counts <-
    vec_outcomes |>
    rlang::set_names(stringr::str_c("n_", vec_outcomes)) |>
    purrr::map(
      .f = ~ as.integer(sum(data_compared[["outcome"]] == .x))
    )

  value_max_difference <-
    if (
      nrow(data_compared_only) == 0L
    ) {
      NA_real_
    } else {
      max(data_compared_only[["max_abs_difference"]])
    }

  res_summary <-
    tibble::tibble(
      n_reference = nrow(data_compared),
      n_reference_human = as.integer(
        sum(data_compared[["reference_dominance"]] == "Human")
      ),
      n_compared = nrow(data_compared_only),
      !!!list_counts,
      n_human_new = as.integer(
        sum(data_compared_only[["new_dominance"]] == "Human")
      ),
      n_human_reference_compared = as.integer(
        sum(data_compared_only[["reference_dominance"]] == "Human")
      ),
      max_abs_difference = value_max_difference,
      tolerance = tolerance
    ) |>
    dplyr::mutate(
      is_reproduced = .data[["n_compared"]] > 0L &
        .data[["n_estimated_without_balance"]] == 0L &
        .data[["n_differs"]] == 0L &
        .data[["n_unexplained"]] == 0L
    )

  res <-
    list(
      records = res_records,
      summary = res_summary
    )

  return(res)
}
