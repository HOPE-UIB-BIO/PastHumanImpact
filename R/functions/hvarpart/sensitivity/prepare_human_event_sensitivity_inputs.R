#' Prepare chronology cohorts and audit human-event sensitivity inputs
#'
#' @param data_predictors Canonical filtered nested predictor data.
#' @param events_temporal_subset Event chronology data with `have_events`.
#' @param data_meta Dataset metadata with continent in `region`.
#'
#' @return A list containing nested predictor cohorts, a dataset-level model
#'   audit, and chronology-availability summaries.
#'
#' @export
prepare_human_event_sensitivity_inputs <-
  function(data_predictors, events_temporal_subset, data_meta) {
    assertthat::assert_that(
      is.data.frame(data_predictors),
      all(c("dataset_id", "data_merge") %in% names(data_predictors)),
      is.list(data_predictors[["data_merge"]]),
      is.data.frame(events_temporal_subset),
      all(c("dataset_id", "have_events") %in%
        names(events_temporal_subset)),
      is.logical(events_temporal_subset[["have_events"]]),
      !anyDuplicated(events_temporal_subset[["dataset_id"]]),
      is.data.frame(data_meta),
      all(c("dataset_id", "region") %in% names(data_meta)),
      msg = "Human-event sensitivity inputs do not satisfy the contract."
    )

    availability <-
      events_temporal_subset |>
      dplyr::select(dplyr::all_of(c("dataset_id", "have_events"))) |>
      dplyr::left_join(
        data_meta |>
          dplyr::select(dplyr::all_of(c("dataset_id", "region"))) |>
          dplyr::distinct(),
        by = "dataset_id",
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        chronology_availability = dplyr::if_else(
          .data[["have_events"]],
          "observed_events",
          "default_filled"
        ),
        included_in_filtered_predictors =
          .data[["dataset_id"]] %in% data_predictors[["dataset_id"]]
      )

    invalid_regions <-
      availability |>
      dplyr::filter(is.na(.data[["region"]]))
    if (nrow(invalid_regions) > 0L) {
      cli::cli_abort("Every event-availability record requires a data region.")
    }

    cohort_membership <-
      tidyr::crossing(
        availability,
        cohort = c("as_coded", "observed_events_only")
      ) |>
      dplyr::mutate(
        included_in_cohort =
          .data[["included_in_filtered_predictors"]] &
          (
            .data[["cohort"]] == "as_coded" |
              .data[["have_events"]]
          )
      )

    proxy_variants <- c("spd", "spd_events", "events")
    model_audit <-
      tidyr::crossing(
        cohort_membership,
        proxy_variant = proxy_variants
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        predictor_specification = list(
          resolve_region_event_predictor_specification(
            region = .data[["region"]],
            proxy_variant = .data[["proxy_variant"]]
          )
        ),
        requested_regional_events = stringr::str_c(
          .data[["predictor_specification"]][["events"]],
          collapse = ";"
        ),
        retained_human_predictors = stringr::str_c(
          .data[["predictor_specification"]][["human"]],
          collapse = ";"
        ),
        retained_climate_predictors = stringr::str_c(
          .data[["predictor_specification"]][["climate"]],
          collapse = ";"
        ),
        excluded_event_variables = stringr::str_c(
          .data[["predictor_specification"]][["excluded_events"]],
          collapse = ";"
        ),
        reference_category =
          .data[["predictor_specification"]][["reference"]]
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-dplyr::all_of("predictor_specification"))

    data_cohorts <-
      tibble::tibble(
        cohort = c("as_coded", "observed_events_only"),
        data_predictors = list(
          data_predictors,
          data_predictors |>
            dplyr::filter(
              .data[["dataset_id"]] %in%
                availability[["dataset_id"]][availability[["have_events"]]]
            )
        )
      )
    availability_summary <-
      availability |>
      dplyr::count(
        .data[["chronology_availability"]],
        name = "n_datasets"
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          chronology_availability = "total",
          n_datasets = nrow(availability)
        )
      )

    list(
      cohorts = data_cohorts,
      model_audit = model_audit,
      availability_summary = availability_summary
    )
  }
