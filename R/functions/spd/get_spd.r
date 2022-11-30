get_spd <- function(data_source_c14,
                    data_source_dist_vec,
                    age_from = 0,
                    age_to = 12e3,
                    age_timestep = 100,
                    min_n_dates = 50) {
  # helper functions
  get_spd_density <- function(data_source,
                              sel_dist,
                              sel_calcurve,
                              min_n_dates,
                              max_age,
                              sel_smooth_size) {
    try_spd <-
      make_spd(
        data_source = data_source,
        sel_dist = sel_dist,
        sel_calcurve = sel_calcurve,
        min_n_dates = min_n_dates,
        max_age = max_age,
        sel_smooth_size = sel_smooth_size,
        normalise_to_one = TRUE
      )
    if (
      all(is.na(try_spd))
    ) {
      return(0)
    } else {
      try_spd %>%
        purrr::pluck("PrDens") %>%
        return()
    }
  }

  ### Function for SPDing
  make_spd <- function(data_source,
                       sel_dist,
                       sel_calcurve,
                       max_age,
                       sel_smooth_size = 100,
                       min_n_dates = 50,
                       normalise_to_one = TRUE) {
    data_sub <-
      data_source %>%
      dplyr::filter(dist < sel_dist) %>%
      # limit the data for reasonable time frame
      dplyr::filter(Age <= max_age * 2) %>%
      # limit the data do they only involve data within the calibratuon curve
      dplyr::filter(Age > min(sel_calcurve$C14BP))

    if (
      nrow(data_sub) <= min_n_dates
    ) {
      return(NA)
    }

    cptcal <-
      rcarbon::calibrate(
        x = data_sub$Age,
        errors = data_sub$Error,
        ids = data_sub$LabID,
        calCurves = sel_calcurve,
        verbose = TRUE,
        ncores = 1
      )

    cptspd <-
      rcarbon::spd(
        x = cptcal,
        timeRange = c(max_age, 0),
        spdnormalised = normalise_to_one,
        runm = sel_smooth_size,
        verbose = TRUE
      )

    cptspd$grid %>%
      tibble::as_tibble() %>%
      return()
  }

  # Prepare calibration curves
  intcal20 <-
    rcarbon::mixCurves(
      calCurve1 = "intcal20",
      calCurve2 = "shcal20",
      p = 1,
      resOffsets = 0,
      resErrors = 0
    ) %>%
    tibble::as_tibble()

  shcal20 <-
    rcarbon::mixCurves(
      calCurve1 = "shcal20",
      calCurve2 = "intcal20",
      p = 1,
      resOffsets = 0,
      resErrors = 0
    ) %>%
    tibble::as_tibble()

  calmixed <-
    rcarbon::mixCurves(
      calCurve1 = "intcal20",
      calCurve2 = "shcal20",
      p = 0.5, # using the default p of 0.5
      resOffsets = 0,
      resErrors = 0
    ) %>%
    tibble::as_tibble()

  data_rc_calcurve <-
    data_source_c14 %>%
    dplyr::mutate(
      calcurve = purrr::map(
        .x = curve_name,
        .f = ~ switch(.x,
          "intcal20" = intcal20,
          "SHCal20" = shcal20,
          "mixed_curve20" = calmixed
        )
      )
    )

  # dummay table to bind all the results
  dummy_age_table <-
    tibble::tibble(
      age = seq(
        from = age_to, # [config]
        to = age_from,
        by = -1
      )
    )

  data_spd <-
    data_rc_calcurve %>%
    dplyr::mutate(
      spd = purrr::pmap(
        .l = list(
          rc, # ..1
          calcurve, # ..2
          dataset_id # ..3
        ),
        .f = ~ {
          message(..3)

          sel_data <- ..1

          sel_cal_curve <- ..2

          res <-
            dplyr::bind_cols(
              dummy_age_table,
              data_source_dist_vec %>%
                purrr::map_dfc(
                  .f = ~ get_spd_density(
                    data_source = sel_data,
                    sel_dist = .x,
                    sel_calcurve = sel_cal_curve,
                    min_n_dates = min_n_dates,
                    max_age = age_to,
                    sel_smooth_size = age_timestep
                  )
                )
            )

          return(res)
        }
      )
    )
}
