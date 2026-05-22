#' @title Build summary tables from hvarpart outputs
#' @description
#' Build raw and weighted-mean importance summary tables from nested `varhp`
#' outputs for spatial or temporal grouping.
#' @param data_source Data frame containing at least `varhp` and grouping columns.
#' @param data_type One of `spatial` or `temporal`.
#' @param group_var Character vector of grouping column names.
#' @return List with `summary_table` and `summary_table_weighted_mean`.
get_summary_tables <- function(
  data_source,
  data_type = c("spatial", "temporal"),
  group_var = c("region")
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    is.character(group_var) && length(group_var) > 0,
    msg = "`group_var` must be a non-empty character vector."
  )

  assertthat::assert_that(
    assertthat::has_name(data_source, c("varhp", "data_merge")),
    msg = "`data_source` must contain `varhp` and `data_merge`."
  )

  data_type <- match.arg(data_type)

  assertthat::assert_that(
    assertthat::has_name(data_source, group_var),
    msg = paste(
      "data_source must have a column/s named:",
      paste(group_var, collapse = ", ")
    )
  )

  summary_table <- NULL
  summary_table_weighted_mean <- NULL

  summary_table_work <-
    data_source %>%
    dplyr::mutate(
      summary_table = purrr::map(
        .x = varhp,
        .f = ~ purrr::pluck(.x, "summary_table")
      )
    ) %>%
    tidyr::unnest(summary_table) %>%
    dplyr::select(-c(data_merge, varhp)) %>%
    dplyr::mutate(
      # negative variances can be ignored
      dplyr::across(
        .cols = c(Unique, Individual),
        .fns = ~ replace(., .x < 0, 0.000001)
      )
    )

  if (
    data_type == "spatial"
  ) {
    assertthat::assert_that(
      assertthat::has_name(data_source, "dataset_id"),
      msg = "data_source must have a column/s named dataset_id"
    )

    summary_table_grouped <-
      summary_table_work %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(group_var))
      ) %>%
      dplyr::mutate(
        # number records in climatezones for regions
        n_records = length(unique(dataset_id))
      ) %>%
      dplyr::ungroup() %>%
      janitor::clean_names() %>%
      dplyr::group_by(dataset_id)
  }


  if (
    data_type == "temporal"
  ) {
    assertthat::assert_that(
      assertthat::has_name(data_source, "age"),
      msg = "data_source must have a column/s named age"
    )

    summary_table_grouped <-
      summary_table_work %>%
      dplyr::ungroup() %>%
      janitor::clean_names() %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            c(
              "age",
              group_var
            )
          )
        )
      )
  }

  summary_table <-
    summary_table_grouped %>%
    dplyr::mutate(
      sum_importance = sum(individual),
      ratio_unique = unique / sum_importance,
      ratio_ind = individual / sum_importance
    ) %>%
    dplyr::ungroup()


  # summarise ratios of importance using wmean ratios by model importance
  #   (i.e sum importance of individual predictors)
  if (
    data_type == "spatial"
  ) {
    summary_table_weighted_mean_grouped <-
      summary_table %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            c(
              group_var,
              "predictor"
            )
          ),
        )
      )
  }

  if (
    data_type == "temporal"
  ) {
    summary_table_weighted_mean_grouped <-
      summary_table %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            c(
              "age",
              group_var,
              "predictor"
            )
          )
        )
      )
  }

  summary_table_weighted_mean <-
    summary_table_weighted_mean_grouped %>%
    # summarise by model weight
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        dplyr::all_of(
          c(
            "ratio_unique",
            "ratio_ind"
          )
        ),
        list(
          wmean = ~ weighted.mean(
            x = .x,
            w = sum_importance,
            na.rm = TRUE
          )
        )
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("ratio"),
      names_to = "importance_type",
      values_to = "ratio"
    )


  # - returning tables ----
  results <-
    list(
      summary_table = summary_table,
      summary_table_weighted_mean = summary_table_weighted_mean
    )

  return(results)
}
