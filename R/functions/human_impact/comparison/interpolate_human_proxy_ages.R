#' @title Align a human proxy to a target age grid
#' @description
#' Linearly interpolate proxy values within each dataset while recording the
#' two source ages and interpolation weight. Extrapolation is never allowed.
#' @param data_source Data frame with `dataset_id`, `age_bp`, and `value`.
#' @param target_ages Numeric vector of target ages in years BP.
#' @return Aligned proxy table with interpolation provenance.
#' @examples
#' aligned <- interpolate_human_proxy_ages(
#'   data.frame(dataset_id = 1, age_bp = c(1000, 2000), value = c(1, 3)),
#'   1500
#' )
interpolate_human_proxy_ages <- function(data_source, target_ages) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(c("dataset_id", "age_bp", "value") %in% names(data_source)),
    is.numeric(target_ages),
    length(target_ages) > 0L,
    all(is.finite(target_ages)),
    msg = "Human-proxy age-alignment inputs are invalid."
  )

  grouping_columns <-
    intersect(
      c(
        "dataset_id",
        "proxy",
        "radius_km",
        "extraction",
        "aggregation"
      ),
      names(data_source)
    )

  res_aligned <-
    data_source |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_columns))) |>
    dplyr::group_modify(
      ~ {
        data_group <-
          .x |>
          dplyr::filter(
            is.finite(.data[["age_bp"]]),
            is.finite(.data[["value"]])
          ) |>
          dplyr::arrange(.data[["age_bp"]]) |>
          dplyr::distinct(.data[["age_bp"]], .keep_all = TRUE)

        target_ages |>
          purrr::map(
            ~ {
            target_age <- .x

            younger_age <-
              suppressWarnings(max(
                data_group[["age_bp"]][data_group[["age_bp"]] <= target_age],
                na.rm = TRUE
              ))

            older_age <-
              suppressWarnings(min(
                data_group[["age_bp"]][data_group[["age_bp"]] >= target_age],
                na.rm = TRUE
              ))

            if (
              !is.finite(younger_age) || !is.finite(older_age)
            ) {
              return(tibble::tibble(
                age_bp = target_age,
                value = NA_real_,
                source_age_younger = NA_real_,
                source_age_older = NA_real_,
                interpolation_weight = NA_real_
              ))
            }

            younger_value <-
              data_group[["value"]][
                match(younger_age, data_group[["age_bp"]])
              ]

            older_value <-
              data_group[["value"]][
                match(older_age, data_group[["age_bp"]])
              ]

            interpolation_weight <-
              if (
                older_age == younger_age
              ) {
                0
              } else {
                (target_age - younger_age) / (older_age - younger_age)
              }

            value <-
              younger_value +
                interpolation_weight * (older_value - younger_value)

            tibble::tibble(
              age_bp = target_age,
              value = value,
              source_age_younger = younger_age,
              source_age_older = older_age,
              interpolation_weight = interpolation_weight
            )
            }
          ) |>
          dplyr::bind_rows()
      }
    ) |>
    dplyr::ungroup()

  return(res_aligned)
}
