#' @title Prepare filtered and unfiltered joint HVarPart contrasts
#' @description Calculate filtered-minus-unfiltered differences for the joint
#' human block from the exported spatial and temporal plot-source tables.
#' @param data_balance Filtered joint spatial balance records.
#' @param data_composition Filtered joint temporal compositions.
#' @param data_unfiltered_balance Optional unfiltered joint balance records.
#' @param data_unfiltered_composition Optional unfiltered joint compositions.
#' @return A named list of balance and composition contrast tables.
#' @examples
#' \dontrun{prepare_human_proxy_model_contrasts(balance, composition)}
prepare_human_proxy_model_contrasts <- function(
  data_balance,
  data_composition,
  data_unfiltered_balance = tibble::tibble(),
  data_unfiltered_composition = tibble::tibble()
) {
  assertthat::assert_that(
    is.data.frame(data_balance),
    is.data.frame(data_composition),
    is.data.frame(data_unfiltered_balance),
    is.data.frame(data_unfiltered_composition),
    msg = "Human-proxy contrast inputs do not satisfy the contract."
  )
  joint_balance <- tibble::tibble()
  if (nrow(data_unfiltered_balance) > 0L) {
    joint_balance <- data_balance |>
      dplyr::filter(.data[["model_id"]] == "joint_filtered") |>
      dplyr::select(dplyr::all_of(c("dataset_id", "zero_balance"))) |>
      dplyr::rename(filtered_zero_balance = "zero_balance") |>
      dplyr::inner_join(
        data_unfiltered_balance |>
          dplyr::select(dplyr::all_of(c("dataset_id", "zero_balance"))) |>
          dplyr::rename(unfiltered_zero_balance = "zero_balance"),
        by = "dataset_id", relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        filtered_minus_unfiltered =
          .data[["filtered_zero_balance"]] -
          .data[["unfiltered_zero_balance"]]
      )
  }
  joint_composition <- tibble::tibble()
  if (nrow(data_unfiltered_composition) > 0L) {
    joint_composition <- data_composition |>
      dplyr::filter(.data[["model_id"]] == "joint_filtered") |>
      dplyr::select(dplyr::all_of(c(
        "region", "age", "predictor", "allocation"
      ))) |>
      dplyr::rename(filtered_allocation = "allocation") |>
      dplyr::inner_join(
        data_unfiltered_composition |>
          dplyr::select(dplyr::all_of(c(
            "region", "age", "predictor", "allocation"
          ))) |>
          dplyr::rename(unfiltered_allocation = "allocation"),
        by = c("region", "age", "predictor"),
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        filtered_minus_unfiltered =
          .data[["filtered_allocation"]] -
          .data[["unfiltered_allocation"]]
      )
  }
  res <- list(
    filtered_unfiltered_joint_balance = joint_balance,
    filtered_unfiltered_joint_composition = joint_composition
  )

  return(res)
}
