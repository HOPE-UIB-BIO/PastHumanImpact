#' @title Apply Region-Specific Logical Rules To Binary Events
#' @description
#' For each dataset, apply region-specific event logic to harmonise binary
#' event signals (for example, in Europe `bi` is constrained by `cc` and `ec`).
#' @param data_source A data frame with columns `dataset_id`, `region`, and
#'   `events_binary` (list-column of event tables).
#' @param verbose Logical. If `TRUE` (default), progress messages are printed.
#' @return
#' A data frame with columns `dataset_id` and `events_updated` (list-column of
#' region-specific, rule-adjusted event tables).
add_logical_rules <- function(data_source, verbose = TRUE) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    all(c("dataset_id", "region", "events_binary") %in% names(data_source)),
    msg = "`data_source` must contain dataset_id, region, and events_binary."
  )

  allowed_regions <- c("Europe", "North America", "Asia", "Oceania")
  invalid_regions <-
    setdiff(unique(data_source$region), allowed_regions)

  if (length(invalid_regions) > 0) {
    cli::cli_abort(
      message = c(
        "Unsupported region(s) in `data_source`.",
        "x" = paste(invalid_regions, collapse = ", ")
      )
    )
  }

  data_source %>%
    dplyr::mutate(
      events_updated = purrr::pmap(
        .l = list(
          dataset_id, region, events_binary
        ),
        .f = ~ {
          # easier for debugging
          sel_dataset_id <- ..1
          sel_region <- ..2
          sel_events <- ..3

          assertthat::assert_that(
            is.data.frame(sel_events),
            msg = "Each `events_binary` entry must be a data frame."
          )

          if (isTRUE(verbose)) {
            message(sel_dataset_id)
          }

          switch(sel_region,
            "Europe" = {
              res <-
                sel_events %>%
                dplyr::mutate(
                  ec_w = dplyr::case_when(
                    (cc + ec) > 0 ~ 1,
                    (cc + ec) == 0 ~ 0
                  ),
                  fi_w = dplyr::case_when(
                    (cc + ec_w) > 0 ~ 0,
                    fc == 1 ~ 1,
                    (cc + ec_w) == 0 & fi == 1 ~ 1,
                    (cc + ec_w + fi + fc) == 0 ~ 0
                  ),
                  bi_w = dplyr::case_when(
                    (cc + ec_w + fc + fi_w) > 0 ~ 0,
                    (cc + ec_w + fc + fi_w) == 0 ~ 1
                  )
                ) %>%
                dplyr::select(age, bi_w, fi_w, fc, ec_w, cc) %>%
                dplyr::rename(
                  bi = bi_w,
                  fi = fi_w,
                  ec = ec_w
                )
            },
            "North America" = {
              res <-
                sel_events %>%
                dplyr::mutate(
                  fc_01_diff = ifelse(
                    test = c(diff(fc_start), 0) == -1 &
                      c(diff(fc_end), 0) == -1,
                    yes = 1,
                    no = 0
                  ),
                  fc_02_diff = ifelse(
                    test = c(diff(fc_start_02), 0) == -1 &
                      c(diff(fc_end_02), 0) == -1,
                    yes = 1,
                    no = 0
                  ),
                  fc_03_diff = ifelse(
                    test = c(diff(fc_start_03), 0) == -1 &
                      c(diff(fc_end_03), 0) == -1,
                    yes = 1,
                    no = 0
                  ),
                  fc_04_diff = ifelse(
                    test = c(diff(fc_start_04), 0) == -1 &
                      c(diff(fc_end_04), 0) == -1,
                    yes = 1,
                    no = 0
                  ),
                  fc_w = dplyr::case_when(
                    fc_start == 1 & fc_end == 0 ~ 1,
                    fc_start_02 == 1 & fc_end_02 == 0 ~ 1,
                    fc_start_03 == 1 & fc_end_03 == 0 ~ 1,
                    fc_start_04 == 1 & fc_end_04 == 0 ~ 1,
                    fc_01_diff == 1 | fc_02_diff == 1 |
                      fc_03_diff == 1 | fc_04_diff == 1 ~ 1,
                    es == 1 ~ 1,
                    TRUE ~ 0
                  ),
                  fc = ifelse(is.na(fc_w) == TRUE, 0, fc_w),
                  bi = dplyr::case_when(
                    (es + fc) > 0 ~ 0,
                    (es + fc) == 0 ~ 1
                  )
                ) %>%
                dplyr::select(age, bi, fc, es)
            },
            "Asia" = {
              res <-
                sel_events %>%
                dplyr::mutate(
                  fi_w = dplyr::case_when(
                    ei == 1 ~ 0,
                    fc == 1 ~ 1,
                    (ei + fc) == 0 & fi == 1 ~ 1,
                    (ei + fc + fi) == 0 ~ 0
                  ),
                  bi = dplyr::case_when(
                    (ei + fc + fi_w) > 0 ~ 0,
                    (ei + fc + fi_w) == 0 ~ 1
                  )
                ) %>%
                dplyr::select(age, bi, fi_w, fc, ei) %>%
                dplyr::rename(
                  fi = fi_w
                )
            },
            "Oceania" = {
              res <-
                sel_events %>%
                dplyr::mutate(
                  no_impact = ifelse(
                    (weak + medium + strong) < 1,
                    1,
                    0
                  )
                ) %>%
                dplyr::select(age, no_impact, weak, medium, strong)
            },
            {
              cli::cli_abort("Unsupported region: {.val {sel_region}}")
            }
          )

          return(res)
        }
      )
    ) %>%
    dplyr::select(dataset_id, events_updated)
}
