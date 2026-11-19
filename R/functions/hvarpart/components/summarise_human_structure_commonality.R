#' @title Summarise variation shared by human and a structural group
#' @description
#' Reduce commonality fractions to the variation that the human group shares
#' with one structural group (for example time), per fitted model and in
#' summary across models.
#' @param data_commonality
#' Output of `summarise_hvarpart_commonality_fractions()`.
#' @param id_columns
#' Character vector of columns identifying one fitted model.
#' @param summary_columns
#' Character vector of columns defining summary groups; must be a subset of
#' `id_columns`.
#' @param human_group
#' Name of the human predictor group.
#' @param structure_group
#' Name of the structural predictor group.
#' @return
#' A list with `dataset_values` (one row per fitted model: `unique_human`,
#' `human_structure_two_way`, `human_shared_with_structure`,
#' `human_marginal`, `shared_share_of_marginal`, `total_fraction`) and
#' `summary` (per summary group: `n_records` and the median, first, and third
#' quartile of each value).
#' @details
#' `human_structure_two_way` is the fraction common to exactly the human and
#' structural groups. `human_shared_with_structure` adds every higher-order
#' fraction containing both groups. `human_marginal` is the sum of all
#' fractions containing the human group, which equals the adjusted R-squared
#' of the human group alone. `shared_share_of_marginal` is
#' `human_shared_with_structure / human_marginal`, and is `NA` when
#' `human_marginal` is not positive. Fractions can be negative.
#' @examples
#' \dontrun{
#' summarise_human_structure_commonality(data_commonality = commonality)
#' }
summarise_human_structure_commonality <- function(
  data_commonality,
  id_columns = c("model_id", "dataset_id"),
  summary_columns = "model_id",
  human_group = "human",
  structure_group = "time"
) {
  assertthat::assert_that(
    is.data.frame(data_commonality),
    is.character(id_columns),
    length(id_columns) > 0L,
    all(c(id_columns, "groups", "fraction", "total_fraction") %in%
      names(data_commonality)),
    is.character(summary_columns),
    all(summary_columns %in% id_columns),
    assertthat::is.string(human_group),
    assertthat::is.string(structure_group),
    human_group != structure_group,
    msg = "Human-structure commonality inputs do not satisfy the contract."
  )

  list_group_sets <-
    stringr::str_split(data_commonality[["groups"]], "&")

  data_flags <-
    data_commonality |>
    dplyr::mutate(
      has_human = purrr::map_lgl(
        list_group_sets,
        .f = ~ human_group %in% .x
      ),
      has_structure = purrr::map_lgl(
        list_group_sets,
        .f = ~ structure_group %in% .x
      ),
      n_set = purrr::map_int(list_group_sets, length)
    )

  data_values <-
    data_flags |>
    dplyr::summarise(
      unique_human = sum(
        .data[["fraction"]][.data[["has_human"]] & .data[["n_set"]] == 1L]
      ),
      human_structure_two_way = sum(
        .data[["fraction"]][
          .data[["has_human"]] & .data[["has_structure"]] &
            .data[["n_set"]] == 2L
        ]
      ),
      human_shared_with_structure = sum(
        .data[["fraction"]][.data[["has_human"]] & .data[["has_structure"]]]
      ),
      human_marginal = sum(.data[["fraction"]][.data[["has_human"]]]),
      total_fraction = dplyr::first(.data[["total_fraction"]]),
      .by = dplyr::all_of(id_columns)
    ) |>
    dplyr::mutate(
      shared_share_of_marginal = dplyr::if_else(
        .data[["human_marginal"]] > 0,
        .data[["human_shared_with_structure"]] / .data[["human_marginal"]],
        NA_real_
      ),
      .before = "total_fraction"
    )

  vec_value_columns <-
    c(
      "unique_human", "human_structure_two_way",
      "human_shared_with_structure", "human_marginal",
      "shared_share_of_marginal", "total_fraction"
    )

  data_summary <-
    data_values |>
    dplyr::summarise(
      n_records = dplyr::n(),
      dplyr::across(
        dplyr::all_of(vec_value_columns),
        .fns = list(
          median = ~ stats::median(.x, na.rm = TRUE),
          q25 = ~ stats::quantile(
            .x, probs = 0.25, na.rm = TRUE, names = FALSE
          ),
          q75 = ~ stats::quantile(
            .x, probs = 0.75, na.rm = TRUE, names = FALSE
          )
        ),
        .names = "{.col}_{.fn}"
      ),
      .by = dplyr::all_of(summary_columns)
    )

  res <-
    list(
      dataset_values = data_values,
      summary = data_summary
    )

  return(res)
}
