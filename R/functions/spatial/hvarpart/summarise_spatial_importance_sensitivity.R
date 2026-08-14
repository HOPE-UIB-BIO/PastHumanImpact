#' @title Summarise thinned and leave-out importance sensitivities
#' @description
#' Recalculate signed and zero-truncated importance balances for the unthinned,
#' repeated spatial thinning, and deterministic region and climate-zone
#' omissions using the canonical HVarPart denominators as weights.
#' @param data_records Core-level spatial importance records.
#' @param data_thinning Retention ledger from `select_spatial_thinning()`.
#' @return
#' A common-schema table with balances, rankings, sample counts, and deviations
#' from the matching unthinned unthinned.
#' @examples
#' \dontrun{
#' summarise_spatial_importance_sensitivity(
#'   data_records = spatial_balance_records,
#'   data_thinning = thinning_ledger
#' )
#' }
summarise_spatial_importance_sensitivity <- function(
  data_records,
  data_thinning
) {
  required_records <-
    c(
      "model_id",
      "region",
      "climatezone",
      "signed_balance",
      "signed_weight",
      "zero_balance",
      "zero_weight"
    )
  required_thinning <-
    c("model_id", "distance_km", "repetition")
  assertthat::assert_that(
    is.data.frame(data_records),
    all(required_records %in% names(data_records)),
    is.data.frame(data_thinning),
    all(required_thinning %in% names(data_thinning)),
    msg = "Spatial sensitivity inputs do not satisfy the required contract."
  )

  data_unthinned <-
    summarise_spatial_importance_subset(
      data_subset = data_records,
      sensitivity_type = "unthinned"
    )

  list_thinning <-
    data_thinning |>
    dplyr::distinct(
      .data[["distance_km"]],
      .data[["repetition"]]
    ) |>
    purrr::pmap(
      .f = ~ {
        data_ids <-
          data_thinning |>
          dplyr::filter(
            .data[["distance_km"]] == ..1,
            .data[["repetition"]] == ..2
          ) |>
          dplyr::select(dplyr::all_of("model_id"))
        summarise_spatial_importance_subset(
          data_subset = dplyr::semi_join(
            data_records,
            data_ids,
            by = "model_id"
          ),
          sensitivity_type = "thinning",
          distance_value = ..1,
          repetition_value = as.integer(..2)
        )
      }
    )

  list_leave_region <-
    unique(data_records[["region"]]) |>
    purrr::map(
      .f = ~ summarise_spatial_importance_subset(
        data_subset = data_records |>
          dplyr::filter(.data[["region"]] != .x),
        sensitivity_type = "leave_region_out",
        omitted_group = as.character(.x)
      ) |>
        dplyr::filter(.data[["aggregation_level"]] == "overall")
    )
  list_leave_climate <-
    unique(data_records[["climatezone"]]) |>
    purrr::map(
      .f = ~ summarise_spatial_importance_subset(
        data_subset = data_records |>
          dplyr::filter(.data[["climatezone"]] != .x),
        sensitivity_type = "leave_climatezone_out",
        omitted_group = as.character(.x)
      ) |>
        dplyr::filter(.data[["aggregation_level"]] == "overall")
    )

  data_sensitivity <-
    dplyr::bind_rows(
      list(data_unthinned),
      list_thinning,
      list_leave_region,
      list_leave_climate
    ) |>
    dplyr::mutate(
      region = dplyr::coalesce(
        as.character(.data[["region"]]),
        "All"
      ),
      climatezone = dplyr::coalesce(
        as.character(.data[["climatezone"]]),
        "All"
      ),
      ranking = dplyr::case_when(
        .data[["importance_balance"]] > 0 ~ "human",
        .data[["importance_balance"]] < 0 ~ "climate",
        .default = "tie"
      )
    )
  data_reference <-
    data_unthinned |>
    dplyr::transmute(
      aggregation_level = .data[["aggregation_level"]],
      profile = .data[["profile"]],
      region = .data[["region"]],
      climatezone = .data[["climatezone"]],
      unthinned_balance = .data[["importance_balance"]]
    ) |>
    dplyr::mutate(
      region = dplyr::coalesce(as.character(.data[["region"]]), "All"),
      climatezone = dplyr::coalesce(
        as.character(.data[["climatezone"]]),
        "All"
      )
    )
  data_sensitivity <-
    data_sensitivity |>
    dplyr::left_join(
      data_reference,
      by = c(
        "aggregation_level",
        "profile",
        "region",
        "climatezone"
      )
    ) |>
    dplyr::mutate(
      absolute_deviation = abs(
        .data[["importance_balance"]] - .data[["unthinned_balance"]]
      )
    )

  return(data_sensitivity)
}
