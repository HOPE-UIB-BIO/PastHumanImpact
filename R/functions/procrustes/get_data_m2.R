#' @title Prepare m2 and PCoA summaries by region and climate zone
#' @description
#' Builds per-age PCA models, computes pairwise Procrustes m2 matrices, and
#' derives temporal and PCoA summaries.
#' @param data_source Data frame with `dataset_id` and nested `data_merge` tables.
#' @param data_meta Data frame with `dataset_id`, `region`, and `climatezone`.
#' @param min_samples Minimum number of samples required per nested age group.
#' @param select_vars Character vector of columns to keep from nested `data_merge`.
#' @return Data frame with grouped PCA analyses and derived m2 summaries.
get_data_m2 <- function(data_source = data_for_hvar,
                        data_meta = data_meta,
                        min_samples = 5,
                        select_vars = NULL) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_meta),
    msg = "`data_meta` must be a data frame."
  )
  assertthat::assert_that(
    is.numeric(min_samples) && length(min_samples) == 1 && min_samples >= 1,
    msg = "`min_samples` must be a single numeric value >= 1."
  )
  assertthat::assert_that(
    is.character(select_vars),
    msg = "`select_vars` must be a character vector."
  )

  assertthat::assert_that(
    all(c("dataset_id", "data_merge") %in% names(data_source)),
    msg = "`data_source` must contain `dataset_id` and `data_merge`."
  )
  assertthat::assert_that(
    all(c("dataset_id", "region", "climatezone") %in% names(data_meta)),
    msg = "`data_meta` must contain `dataset_id`, `region`, and `climatezone`."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_source$data_merge, is.data.frame)),
    msg = "`data_merge` entries must be data frames."
  )

  # prepare data
  data_for_pca <-
    data_source %>%
    dplyr::inner_join(
      data_meta %>%
        dplyr::select(dataset_id, region, climatezone),
      by = "dataset_id"
    ) %>%
    tidyr::unnest(data_merge) %>%
    dplyr::select(
      c(
        "region", "climatezone",
        dplyr::all_of(select_vars)
      )
    ) %>%
    tidyr::nest(
      data = -c("age", "climatezone", "region")
    ) %>%
    dplyr::mutate(
      n_samples = purrr::map_dbl(
        .x = data,
        .f = ~ nrow(.x)
      )
    ) %>%
    dplyr::filter(n_samples >= min_samples)

  # Run PCA analyses; get procrustes sum of square matrices; extract difference with time
  pap_procrustes <-
    data_for_pca %>%
    dplyr::mutate(
      pca_analysis = purrr::map(
        .x = data,
        .f = run_pca
      )
    ) %>%
    dplyr::mutate(
      pca_analysis = pca_analysis %>%
        rlang::set_names(nm = data_for_pca$age)
    ) %>%
    dplyr::group_by(region, climatezone) %>%
    dplyr::summarise(
      pca_analysis = list(pca_analysis)
    ) %>%
    dplyr::mutate(
      m2 = purrr::map(
        .x = pca_analysis,
        .f = get_procrustes_m2
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      m2_time = purrr::map(
        .x = m2,
        .f = extract_m2_time
      )
    ) %>%
    dplyr::mutate(
      PCoA = purrr::map(
        .x = m2, .f = run_pcoa
      )
    ) %>%
    dplyr::mutate(
      m2_time_df = purrr::map(
        .x = m2_time,
        .f = get_m2_time_df
      )
    )

  return(pap_procrustes)
}
