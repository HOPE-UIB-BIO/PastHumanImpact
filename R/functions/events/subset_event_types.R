#' @title Subset Region-Specific Event Types
#' @description
#' Keep only region-relevant event columns per dataset. If a dataset has no
#' event table, return a region-specific default event table over the allowed
#' age range.
#' @param data_source_events A data frame with columns `dataset_id` and `data`
#'   where `data` is a list-column of event tables.
#' @param data_source_meta A data frame with columns `dataset_id`, `region`,
#'   `age_min`, and `age_max`.
#' @param data_source_dummy_time A data frame with column `age` used for
#'   creating default no-event records.
#' @return
#' A data frame with columns `dataset_id`, `have_events`, and `events`
#' (list-column with region-specific event tables).
subset_event_types <- function(data_source_events,
                               data_source_meta,
                               data_source_dummy_time) {
  assertthat::assert_that(
    is.data.frame(data_source_events),
    msg = "`data_source_events` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_meta),
    msg = "`data_source_meta` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_dummy_time),
    msg = "`data_source_dummy_time` must be a data frame."
  )

  req_events <- c("dataset_id", "data")
  req_meta <- c("dataset_id", "region", "age_min", "age_max")

  assertthat::assert_that(
    all(req_events %in% names(data_source_events)),
    msg = "`data_source_events` must contain dataset_id and data."
  )
  assertthat::assert_that(
    all(req_meta %in% names(data_source_meta)),
    msg = "`data_source_meta` must contain dataset_id, region, age_min, and age_max."
  )
  assertthat::assert_that(
    "age" %in% names(data_source_dummy_time),
    msg = "`data_source_dummy_time` must contain an age column."
  )

  allowed_regions <- c(
    "Asia", "Europe", "North America", "Latin America", "Oceania"
  )
  invalid_regions <-
    setdiff(unique(data_source_meta$region), allowed_regions)

  if (length(invalid_regions) > 0) {
    cli::cli_abort(
      message = c(
        "Unsupported region(s) in `data_source_meta`.",
        "x" = paste(invalid_regions, collapse = ", ")
      )
    )
  }

  data_age_limits <-
    data_source_meta %>%
    dplyr::select(dataset_id, region, age_min, age_max) %>%
    dplyr::mutate(
      dummy_table = list(data_source_dummy_time)
    ) %>%
    dplyr::mutate(
      dummy_table = purrr::pmap(
        .l = list(dummy_table, age_min, age_max),
        .f = ~ ..1 %>%
          dplyr::filter(
            age >= ..2 & age <= ..3
          )
      )
    ) %>%
    dplyr::select(-c(age_min, age_max))

  events_prepared <-
    data_age_limits %>%
    dplyr::left_join(
      data_source_events,
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      have_events = purrr::map_lgl(
        .x = data,
        .f = is.data.frame
      )
    ) %>%
    dplyr::mutate(
      events = purrr::pmap(
        .l = list(
          have_events, # ..1
          region, # ..2
          data, # ..3
          dummy_table # ..4
        ),
        .f = ~ {
          if (isTRUE(..1)) {
            return(
              switch(
                ..2,
                "Asia" = dplyr::select(
                  .data = ..3,
                  dplyr::any_of(c("age", "bi", "fi", "fc", "ei"))
                ),
                "Europe" = dplyr::select(
                  .data = ..3,
                  dplyr::any_of(c("age", "bi", "fi", "fc", "ec", "cc"))
                ),
                "North America" = dplyr::select(
                  .data = ..3,
                  dplyr::any_of(c("age", "bi", "fc", "es"))
                ),
                "Latin America" = dplyr::select(
                  .data = ..3,
                  dplyr::any_of(c("age", "no_impact", "weak", "strong"))
                ),
                "Oceania" = dplyr::select(
                  .data = ..3,
                  dplyr::any_of(c("age", "no_impact", "weak", "medium", "strong"))
                ),
                {
                  cli::cli_abort("Unsupported region: {.val {..2}}")
                }
              )
            )
          }

          return(
            switch(
              ..2,
              "Asia" = dplyr::mutate(
                .data = ..4,
                bi = 1,
                fi = 0,
                fc = 0,
                ei = 0
              ),
              "Europe" = dplyr::mutate(
                .data = ..4,
                bi = 1,
                fi = 0,
                fc = 0,
                ec = 0,
                cc = 0
              ),
              "North America" = dplyr::mutate(
                .data = ..4,
                bi = 1,
                fc = 0,
                es = 0
              ),
              "Latin America" = dplyr::mutate(
                .data = ..4,
                no_impact = 1,
                weak = 0,
                strong = 0
              ),
              "Oceania" = dplyr::mutate(
                .data = ..4,
                no_impact = 1,
                weak = 0,
                medium = 0,
                strong = 0
              ),
              {
                cli::cli_abort("Unsupported region: {.val {..2}}")
              }
            )
          )
        }
      )
    ) %>%
    dplyr::select(dataset_id, have_events, events)

  return(events_prepared)
}
