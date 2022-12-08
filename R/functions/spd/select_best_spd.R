#' @title Get the best SPD for each site
#' @description For each distance class of SPD of each pollen record, noe
#' RDA is estimated with event types as responses (binary) and SPD values as
#' predictors. Next, R2 is estimated using for each distance class.
#' Finally, the distance class with the highest R2 is selected as the
#' representation of human activity for that pollen record.
#' As this is only possile for sites with both events and SPD, some sequences
#' might end without SPD. In that case, the most common distance for each
#' region is used.
select_best_spd <- function(data_source_events,
                            data_source_spd,
                            data_source_meta,
                            data_source_dist_vec) {
  # helper functions
  get_distance_numeric <- function(dist_vec) {
    stringr::str_replace(dist_vec, "x", "") %>%
      as.numeric() %>%
      round() %>%
      return()
  }

  get_distance_character <- function(dist_vec) {
    round(dist_vec) %>%
      as.character() %>%
      paste0(
        "x",
        .
      ) %>%
      rlang::set_names() %>%
      return()
  }

  get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  select_spd <- function(data_source) {
    data_source %>%
      dplyr::mutate(
        spd_selected = purrr::map2(
          .x = spd,
          .y = best_dist,
          .f = ~ .x %>%
            janitor::clean_names() %>%
            dplyr::select(
              dplyr::all_of(
                c("age", get_distance_character(.y))
              )
            ) %>%
            rlang::set_names(
              nm = c("age", "spd")
            )
        )
      ) %>%
      return()
  }

  fit_ordination <- function(data_source, dist_vec) {
    sel_data <- data_source

    data_events <-
      sel_data %>%
      dplyr::select(
        !c(
          get_distance_character(dist_vec) %>%
            dplyr::any_of(),
          "age"
        )
      )

    get_distance_character(dist_vec) %>%
      purrr::map_dbl(
        .f = ~ {
          res_model <-
            vegan::rda(
              as.formula(
                paste(
                  "data_events ~ ", .x
                )
              ),
              data = sel_data
            )

          res <-
            vegan::RsquareAdj(res_model) %>%
            purrr::pluck("adj.r.squared")

          if (
            is.null(res)
          ) {
            return(0)
          } else {
            return(res)
          }
        }
      ) %>%
      return()
  }

  # emerge events and spd  -----
  data_merge <-
    dplyr::inner_join(
      data_source_spd %>%
        dplyr::rename(
          spd = data
        ),
      data_source_events,
      by = "dataset_id"
    ) %>%
    dplyr::filter(
      # has events
      purrr::map_lgl(
        .x = events,
        .f = tibble::is_tibble
      )
    ) %>%
    dplyr::filter(
      # has spd
      purrr::map_lgl(
        .x = spd,
        .f = tibble::is_tibble
      )
    )

  # merge data into single data.frame
  data_to_fit <-
    data_merge %>%
    dplyr::mutate(
      data_to_fit = purrr::map2(
        .x = spd,
        .y = events,
        .f = ~ {
          dplyr::inner_join(
            .x %>%
              janitor::clean_names(),
            .y,
            by = "age"
          ) %>%
            dplyr::arrange(age)
        }
      )
    )

  # run the ordinations  -----
  ordinations <-
    data_to_fit %>%
    dplyr::mutate(
      ord_res = purrr::map(
        .x = data_to_fit,
        .f = ~ fit_ordination(
          data_source = .x,
          dist_vec = data_source_dist_vec
        )
      )
    )

  ordinations_summary <-
    ordinations %>%
    dplyr::filter(
      # filter out sequences which has only zeros
      purrr::map_lgl(
        .x = ord_res,
        .f = ~ (abs(.x) > 0) %>%
          any()
      )
    ) %>%
    dplyr::mutate(
      ord_sum = purrr::map(
        .x = ord_res,
        .f = ~ as.data.frame(.x) %>%
          tibble::rownames_to_column("dist") %>%
          tibble::as_tibble() %>%
          dplyr::mutate(
            dist_num = get_distance_numeric(dist)
          ) %>%
          rlang::set_names(
            nm = c("dist", "r2", "dist_num")
          )
      )
    ) %>%
    dplyr::mutate(
      best_dist = purrr::map_dbl(
        .x = ord_sum,
        .f = ~ .x %>%
          dplyr::slice(which.max(abs(r2))) %>%
          purrr::pluck("dist_num")
      )
    )

  data_spd_subset <-
    ordinations_summary %>%
    # select the best spd
    select_spd() %>%
    dplyr::mutate(
      spd_from_events = TRUE
    ) %>%
    dplyr::select(
      dataset_id, best_dist, spd_from_events, spd_selected
    )

  data_best_dist <-
    data_spd_subset %>%
    dplyr::select(dataset_id, best_dist)

  region_best_dist <-
    dplyr::inner_join(
      data_source_meta,
      data_best_dist,
      by = "dataset_id"
    ) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      best_dist = get_mode(best_dist)
    )

  data_region_fill <-
    data_source_meta %>%
    dplyr::select(dataset_id, region) %>%
    dplyr::inner_join(
      data_source_spd,
      by = "dataset_id"
    ) %>%
    # filter out sites with known distance
    dplyr::filter(
      !dataset_id %in% data_best_dist$dataset_id
    ) %>%
    # add best distance per region
    dplyr::inner_join(
      region_best_dist,
      by = "region"
    ) %>%
    select_spd() %>%
    dplyr::mutate(
      spd_from_events = FALSE
    ) %>%
    dplyr::select(
      dataset_id, best_dist, spd_from_events, spd_selected
    )

  res_spd <-
    data_region_fill %>%
    dplyr::select(
      dplyr::any_of(
        names(data_spd_subset)
      )
    ) %>%
    dplyr::bind_rows(
      data_spd_subset
    ) %>%
    dplyr::rename(
      spd = spd_selected
    )

  return(res_spd)
}
