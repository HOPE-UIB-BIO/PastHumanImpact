#' @title Compute the pooled human-minus-climate allocation
#' @description
#' Pool unit-level human, climate, and space HVarPart contributions into one
#' human-minus-climate allocation per profile, as a ratio of sums across
#' units.
#' @param data_units
#' Data frame with one row per analytical unit and numeric columns `human`,
#' `climate`, and `space` (individual contributions).
#' @param profile
#' Character vector of profiles: `"zero_truncated"`, `"signed"`, and/or
#' `"exclude_negative"`.
#' @return
#' A tibble with one row per profile and columns `profile`,
#' `pooled_difference`, `mean_difference`, `n_units`, and `n_human_greater`.
#' @details
#' With `h`, `c`, and `s` the contributions of a unit:
#' `zero_truncated` sets negative contributions to zero, keeps units with a
#' positive total `h + c + s`, and returns `sum(h - c) / sum(h + c + s)`;
#' `signed` uses all units and returns `sum(h - c) / sum(h + c + s)` on the
#' untruncated values; `exclude_negative` keeps only units without a negative
#' contribution and then applies the zero-truncated rule. This is the pooling
#' of `summarise_hvarpart_importance()`, where hierarchical individual
#' contributions sum to the total adjusted R-squared. `mean_difference` is the
#' unweighted mean of the unit-level `(h - c) / (h + c + s)` over units with a
#' positive total, and `n_human_greater` counts retained units with `h > c`.
#' With no retained unit the pooled and mean values are `NA`.
#' @examples
#' compute_pooled_human_climate_allocation(
#'   tibble::tibble(human = 0.1, climate = 0.3, space = 0.1),
#'   profile = "zero_truncated"
#' )
compute_pooled_human_climate_allocation <- function(
  data_units,
  profile = c("zero_truncated", "signed", "exclude_negative")
) {
  vec_profiles_allowed <-
    c("zero_truncated", "signed", "exclude_negative")

  assertthat::assert_that(
    is.data.frame(data_units),
    all(c("human", "climate", "space") %in% names(data_units)),
    is.character(profile),
    length(profile) > 0L,
    all(profile %in% vec_profiles_allowed),
    msg = "Pooled allocation inputs do not satisfy the contract."
  )

  data_values <-
    data_units |>
    dplyr::transmute(
      human = as.numeric(.data[["human"]]),
      climate = as.numeric(.data[["climate"]]),
      space = as.numeric(.data[["space"]])
    )

  res <-
    profile |>
    purrr::map(
      .f = ~ {
        profile_current <- .x

        data_profile <-
          if (
            identical(profile_current, "signed")
          ) {
            data_values
          } else if (
            identical(profile_current, "exclude_negative")
          ) {
            data_values |>
              dplyr::filter(
                .data[["human"]] >= 0,
                .data[["climate"]] >= 0,
                .data[["space"]] >= 0
              )
          } else {
            data_values |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::all_of(c("human", "climate", "space")),
                  .fns = ~ pmax(.x, 0)
                )
              )
          }

        data_totals <-
          data_profile |>
          dplyr::mutate(
            total = .data[["human"]] + .data[["climate"]] + .data[["space"]],
            difference = .data[["human"]] - .data[["climate"]]
          )

        data_pooled <-
          if (
            identical(profile_current, "signed")
          ) {
            data_totals
          } else {
            data_totals |>
              dplyr::filter(.data[["total"]] > 0)
          }

        data_positive <-
          data_totals |>
          dplyr::filter(.data[["total"]] > 0)

        n_units <-
          nrow(data_pooled)

        tibble::tibble(
          profile = profile_current,
          pooled_difference = dplyr::if_else(
            n_units > 0L,
            sum(data_pooled[["difference"]]) / sum(data_pooled[["total"]]),
            NA_real_
          ),
          mean_difference = dplyr::if_else(
            nrow(data_positive) > 0L,
            mean(data_positive[["difference"]] / data_positive[["total"]]),
            NA_real_
          ),
          n_units = as.integer(n_units),
          n_human_greater = as.integer(
            sum(data_pooled[["human"]] > data_pooled[["climate"]])
          )
        )
      }
    ) |>
    dplyr::bind_rows()

  return(res)
}
