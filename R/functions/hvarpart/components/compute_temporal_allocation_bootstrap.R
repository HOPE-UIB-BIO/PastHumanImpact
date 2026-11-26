#' @title Bootstrap the pooled human-minus-climate allocation over time
#' @description
#' Compute percentile intervals for the pooled human-minus-climate allocation
#' of region-by-age units with a moving-block bootstrap of consecutive ages
#' within each region, keeping the dependence between neighbouring ages.
#' @param data_units
#' Data frame with one row per estimable unit and columns `region`, `age`,
#' `human`, `climate`, and `space`.
#' @param profile
#' Character vector of profiles passed to
#' `compute_pooled_human_climate_allocation()`.
#' @param block_length
#' Number of consecutive units (ordered by age) in each block.
#' @param n_bootstrap
#' Number of bootstrap replicates.
#' @param seed
#' Integer random seed; the global random state is left unchanged.
#' @return
#' A tibble with one row per profile and columns `profile`, `lwr_95` and
#' `upr_95` (2.5th and 97.5th percentiles of the replicate pooled
#' differences), `n_bootstrap`, and `n_failed_replicates` (replicates
#' without a finite pooled difference).
#' @details
#' Regions are strata. Within a region with `n` units ordered by age, block
#' starts are drawn uniformly from the `n - L + 1` possible starts, where `L`
#' is `block_length` (or `n` when smaller), and blocks are concatenated until
#' `n` units are drawn. Each replicate pools the resampled units of all
#' regions with `compute_pooled_human_climate_allocation()`, so profile
#' filters (for example excluding units with negative contributions) are
#' applied within each replicate.
#' @examples
#' \dontrun{
#' compute_temporal_allocation_bootstrap(units, "zero_truncated")
#' }
compute_temporal_allocation_bootstrap <- function(
  data_units,
  profile = c("zero_truncated", "signed", "exclude_negative"),
  block_length = 3L,
  n_bootstrap = 1999L,
  seed = 1234L
) {
  assertthat::assert_that(
    is.data.frame(data_units),
    all(c("region", "age", "human", "climate", "space") %in%
      names(data_units)),
    nrow(data_units) > 0L,
    is.character(profile),
    length(profile) > 0L,
    is.numeric(block_length),
    length(block_length) == 1L,
    block_length >= 1,
    is.numeric(n_bootstrap),
    length(n_bootstrap) == 1L,
    n_bootstrap >= 1,
    is.numeric(seed),
    length(seed) == 1L,
    msg = "Temporal bootstrap inputs do not satisfy the contract."
  )

  data_ordered <-
    data_units |>
    dplyr::arrange(.data[["region"]], .data[["age"]])

  list_region_rows <-
    split(seq_len(nrow(data_ordered)), data_ordered[["region"]])

  mat_replicates <-
    withr::with_seed(
      seed = seed,
      code = purrr::map(
        seq_len(n_bootstrap),
        .f = ~ {
          vec_rows <-
            list_region_rows |>
            purrr::map(
              .f = ~ {
                vec_region_rows <- .x

                n_units <-
                  length(vec_region_rows)

                block_size <-
                  min(as.integer(block_length), n_units)

                n_blocks <-
                  ceiling(n_units / block_size)

                vec_starts <-
                  sample.int(
                    n = n_units - block_size + 1L,
                    size = n_blocks,
                    replace = TRUE
                  )

                vec_positions <-
                  vec_starts |>
                  purrr::map(
                    .f = ~ seq.int(from = .x, length.out = block_size)
                  ) |>
                  unlist(use.names = FALSE)

                vec_region_rows[vec_positions[seq_len(n_units)]]
              }
            ) |>
            unlist(use.names = FALSE)

          compute_pooled_human_climate_allocation(
            data_units = data_ordered[vec_rows, ],
            profile = profile
          ) |>
            dplyr::pull("pooled_difference")
        }
      )
    ) |>
    unlist(use.names = FALSE) |>
    matrix(ncol = length(profile), byrow = TRUE)

  res <-
    seq_along(profile) |>
    purrr::map(
      .f = ~ {
        vec_values <-
          mat_replicates[, .x]

        vec_finite <-
          vec_values[is.finite(vec_values)]

        vec_interval <-
          if (
            length(vec_finite) == 0L
          ) {
            c(NA_real_, NA_real_)
          } else {
            stats::quantile(
              vec_finite,
              probs = c(0.025, 0.975),
              names = FALSE
            )
          }

        tibble::tibble(
          profile = profile[.x],
          lwr_95 = vec_interval[1],
          upr_95 = vec_interval[2],
          n_bootstrap = as.integer(n_bootstrap),
          n_failed_replicates = as.integer(sum(!is.finite(vec_values)))
        )
      }
    ) |>
    dplyr::bind_rows()

  return(res)
}
