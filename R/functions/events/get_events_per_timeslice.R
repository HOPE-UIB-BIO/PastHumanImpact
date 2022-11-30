get_events_per_timeslice <- function(data_source_events,
                                         data_source_dummy_time) {

  # wragler data so that there are grouped by event type
  data_to_fit <-
    data_source_events %>%
    tidyr::unnest(events_updated) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age),
      names_to = "event_type",
      values_to = "var"
    ) %>%
    tidyr::drop_na(var) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, var))

  # fit GAM for each dataset of reach type
  event_gams <-
    data_to_fit$data_to_fit %>%
    purrr::set_names(data_to_fit$event_type) %>%
    purrr::map_dfr(
      .x = .,
      .id = "event_type",
      .f = ~ REcopol:::fit_multiple_gams(
        data_source = .x,
        x_var = "age",
        y_var = "var",
        smooth_basis = "cr",
        max_k = 24,
        error_family = "stats::binomial(link = 'logit')"
      )
    )

  # predict each GAM using `data_source_dummy_time`
  events_predicted <-
    event_gams %>%
    dplyr::mutate(
      event_pred_data = purrr::map(
        .x = mod,
        .f = ~ REcopol::predic_model(
          data_source = data_source_dummy_time,
          model_source = .x
        )
      )
    ) %>%
    tidyr::unnest(event_pred_data) %>%
    dplyr::select(dataset_id, age, event_type, var = fit) %>%
    dplyr::mutate(
      var = round(var, 3)
    ) %>%
    tidyr::pivot_wider(
      names_from = "event_type",
      values_from = "var"
    ) %>%
    tidyr::nest(events_nested = -c(dataset_id))

  events_prepared <-
    events_predicted %>%
    dplyr::select(dataset_id, events = events_nested)

  return(events_prepared)
}
