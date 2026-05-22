#' @title Convert Event Ages To Binary Event Series
#' @description
#' For each dataset, expand pollen levels to intermediate ages and convert event
#' ages to binary indicator columns (`1` after event onset, `0` before onset).
#' @param data_source_events A data frame with columns `dataset_id`, `region`,
#'   and `events_age` (list-column of data frames with columns `name` and
#'   `age`).
#' @param data_source_pollen A data frame with columns `dataset_id` and
#'   `levels` (list-column of data frames with column `age`).
#' @param verbose Logical. If `TRUE` (default), progress messages are printed.
#' @return
#' A data frame with columns `dataset_id`, `region`, and `events_binary`
#' (list-column of data frames with an `age` column and one binary column per
#' event type).
get_events_as_binary <- function(data_source_events,
                                 data_source_pollen,
                                 verbose = TRUE) {
  assertthat::assert_that(
    is.data.frame(data_source_events),
    msg = "`data_source_events` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_pollen),
    msg = "`data_source_pollen` must be a data frame."
  )

  req_events <- c("dataset_id", "region", "events_age")
  req_pollen <- c("dataset_id", "levels")

  assertthat::assert_that(
    all(req_events %in% names(data_source_events)),
    msg = "`data_source_events` must contain dataset_id, region, and events_age."
  )
  assertthat::assert_that(
    all(req_pollen %in% names(data_source_pollen)),
    msg = "`data_source_pollen` must contain dataset_id and levels."
  )
  data_work <-
    data_source_pollen %>%
    dplyr::select(dataset_id, levels) %>%
    dplyr::inner_join(
      data_source_events,
      by = "dataset_id"
    )

  data_work_binary <-
    data_work %>%
    dplyr::mutate(
      events_binary = purrr::pmap(
        .l = list(events_age, levels, dataset_id),
        .f = ~ {
          assertthat::assert_that(
            is.data.frame(..1),
            msg = "`events_age` entries must be data frames."
          )
          assertthat::assert_that(
            is.data.frame(..2),
            msg = "`levels` entries must be data frames."
          )
          assertthat::assert_that(
            all(c("name", "age") %in% names(..1)),
            msg = "Each `events_age` table must contain columns `name` and `age`."
          )
          assertthat::assert_that(
            "age" %in% names(..2),
            msg = "Each `levels` table must contain an `age` column."
          )

          if (isTRUE(verbose)) {
            message(..3)
          }

          events_age <-
            ..1$age %>%
            rlang::set_names(nm = ..1$name)

          data_to_test <-
            REcopol:::util_get_inbetween_values(
              data_source = ..2$age,
              sel_output = "only_inbetween"
            ) %>%
            tibble::enframe(value = "age") %>%
            dplyr::select(age)

          res_w <-
            purrr::map2_dfc(
              .x = events_age,
              .y = names(events_age),
              .f = ~ {
                data_temp <-
                  data_to_test %>%
                  dplyr::mutate(
                    VAR = dplyr::case_when(
                      age <= .x ~ 1,
                      age > .x ~ 0,
                      is.na(.x) == TRUE ~ 0
                    )
                  )

                data_temp %>%
                  dplyr::select(-age) %>%
                  rlang::set_names(nm = .y) %>%
                  return()
              }
            )

          data_to_test %>%
            dplyr::bind_cols(res_w) %>%
            return()
        }
      )
    )

  data_work_binary %>%
    dplyr::select(dataset_id, region, events_binary) %>%
    return()
}
