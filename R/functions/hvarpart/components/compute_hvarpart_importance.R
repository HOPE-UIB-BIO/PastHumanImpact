#' @title Compute unmodified HVarPart importance components
#' @description
#' Extract predictor-level hierarchical-partitioning components directly from
#' `varhp_output$Hier.part`, retain direct commonality fractions from
#' `varhp_output$Var.part`, and attach model-level eligibility diagnostics.
#' Values returned by HVarPart are never truncated, averaged, or silently
#' discarded.
#' @param data_source Data frame containing a `varhp` list-column.
#' @param id_cols Character vector naming columns that uniquely identify one
#' HVarPart model.
#' @param expected_predictors Character vector of required predictor groups.
#' @return
#' A tibble with one row per model and expected predictor. It retains `unique`,
#' `average_share`, `individual`, `individual_percent`,
#' `varpart_unique`, `shared`, `varpart_total`, and
#' `total_adjusted_r_squared`, plus explicit availability, eligibility,
#' negative-contribution, and exclusion diagnostics. Hierarchical individual
#' importance remains separate from direct variation fractions.
#' @examples
#' \dontrun{
#' data_importance <-
#'   compute_hvarpart_importance(
#'     data_source = data_h1_results,
#'     id_cols = "dataset_id"
#'   )
#' }
compute_hvarpart_importance <- function(
  data_source,
  id_cols,
  expected_predictors = c("human", "climate")
) {
  reserved_columns <-
    c(
      "model_id",
      "predictor",
      "unique",
      "average_share",
      "individual",
      "individual_percent",
      "varpart_unique",
      "shared",
      "varpart_total",
      "total_adjusted_r_squared"
    )

  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    is.character(id_cols),
    length(id_cols) > 0L,
    !anyNA(id_cols),
    !anyDuplicated(id_cols),
    msg = "`id_cols` must be a non-empty vector of unique column names."
  )
  assertthat::assert_that(
    is.character(expected_predictors),
    length(expected_predictors) > 0L,
    !anyNA(expected_predictors),
    !anyDuplicated(expected_predictors),
    msg = "`expected_predictors` must contain unique predictor names."
  )
  assertthat::assert_that(
    all(c(id_cols, "varhp") %in% names(data_source)),
    is.list(data_source[["varhp"]]),
    msg = "`data_source` must contain identifier columns and nested results."
  )
  assertthat::assert_that(
    !any(id_cols %in% reserved_columns),
    msg = "`id_cols` cannot use reserved HVarPart output names."
  )

  data_ids <-
    data_source |>
    dplyr::select(dplyr::all_of(id_cols))

  assertthat::assert_that(
    nrow(dplyr::distinct(data_ids)) == nrow(data_ids),
    msg = "`id_cols` must uniquely identify every HVarPart model."
  )

  model_ids <-
    data_ids |>
    purrr::pmap_chr(
      .f = function(...) {
        list_values <- list(...)
        vec_values <-
          purrr::map_chr(
            .x = list_values,
            .f = ~ ifelse(
              is.na(.x),
              "<NA>",
              as.character(.x)
            )
          )

        res_id <-
          stringr::str_c(
            id_cols,
            "=",
            vec_values,
            collapse = "|"
          )

        return(res_id)
      }
    )

  extract_model_components <- function(data_varhp) {
    data_expected <-
      tibble::tibble(
        predictor = expected_predictors
      )

    empty_result <- function() {
      res_empty <-
        data_expected |>
        dplyr::mutate(
          unique = NA_real_,
          average_share = NA_real_,
          individual = NA_real_,
          individual_percent = NA_real_,
          varpart_unique = NA_real_,
          shared = NA_real_,
          varpart_total = NA_real_,
          total_adjusted_r_squared = NA_real_,
          model_result_available = FALSE,
          predictor_present = FALSE,
          varpart_available = FALSE
        )

      return(res_empty)
    }

    if (
      !is.list(data_varhp) ||
        !"varhp_output" %in% names(data_varhp) ||
        !is.list(data_varhp[["varhp_output"]])
    ) {
      res <-
        empty_result()

      return(res)
    }

    data_output <- data_varhp[["varhp_output"]]

    if (
      !"Hier.part" %in% names(data_output) ||
        is.null(data_output[["Hier.part"]])
    ) {
      res <-
        empty_result()

      return(res)
    }

    data_hier <-
      data_output[["Hier.part"]] |>
      as.data.frame() |>
      tibble::rownames_to_column("predictor")

    required_fields <-
      c(
        "Unique",
        "Average.share",
        "Individual",
        "I.perc(%)"
      )

    for (
      field_name in setdiff(required_fields, names(data_hier))
    ) {
      data_hier[[field_name]] <- NA_real_
    }

    total_adjusted_r_squared <-
      if (
        "Total_explained_variation" %in% names(data_output) &&
          length(data_output[["Total_explained_variation"]]) == 1L
      ) {
        as.numeric(data_output[["Total_explained_variation"]])
      } else {
        NA_real_
      }

    data_varpart <-
      if (
        "Var.part" %in% names(data_output) &&
          !is.null(data_output[["Var.part"]])
      ) {
        data_output[["Var.part"]] |>
          as.data.frame() |>
          tibble::rownames_to_column("component") |>
          dplyr::mutate(
            component = stringr::str_squish(.data[["component"]])
          )
      } else {
        NULL
      }

    varpart_available <-
      !is.null(data_varpart) &&
      "Fractions" %in% names(data_varpart)

    if (
      varpart_available
    ) {
      data_varpart <-
        data_varpart |>
        dplyr::transmute(
          component = .data[["component"]],
          fraction = as.numeric(.data[["Fractions"]])
        )

      data_varpart_unique <-
        data_varpart |>
        dplyr::filter(
          stringr::str_starts(.data[["component"]], "Unique to ")
        ) |>
        dplyr::transmute(
          predictor = stringr::str_remove(
            .data[["component"]],
            "^Unique to "
          ),
          varpart_unique = .data[["fraction"]]
        )
      shared_values <-
        data_varpart |>
        dplyr::filter(
          stringr::str_starts(.data[["component"]], "Common to ")
        ) |>
        dplyr::pull("fraction")
      shared <-
        if (
          length(shared_values) == 1L
        ) {
          shared_values
        } else {
          NA_real_
        }
      varpart_total_values <-
        data_varpart |>
        dplyr::filter(.data[["component"]] == "Total") |>
        dplyr::pull("fraction")
      varpart_total <-
        if (
          length(varpart_total_values) == 1L
        ) {
          varpart_total_values
        } else {
          NA_real_
        }
    } else {
      data_varpart_unique <-
        tibble::tibble(
          predictor = character(),
          varpart_unique = numeric()
        )
      shared <- NA_real_
      varpart_total <- NA_real_
    }

    data_raw <-
      data_hier |>
      dplyr::transmute(
        predictor = as.character(.data[["predictor"]]),
        unique = as.numeric(.data[["Unique"]]),
        average_share = as.numeric(.data[["Average.share"]]),
        individual = as.numeric(.data[["Individual"]]),
        individual_percent = as.numeric(.data[["I.perc(%)"]]),
        predictor_present = TRUE
      ) |>
      dplyr::filter(.data[["predictor"]] %in% expected_predictors)

    res_components <-
      data_expected |>
      dplyr::left_join(
        data_varpart_unique,
        by = "predictor"
      ) |>
      dplyr::left_join(
        data_raw,
        by = "predictor"
      ) |>
      dplyr::mutate(
        predictor_present = tidyr::replace_na(
          .data[["predictor_present"]],
          FALSE
        ),
        shared = shared,
        varpart_total = varpart_total,
        total_adjusted_r_squared = total_adjusted_r_squared,
        model_result_available = TRUE,
        varpart_available = varpart_available
      )

    return(res_components)
  }

  data_importance <-
    data_source |>
    dplyr::select(dplyr::all_of(c(id_cols, "varhp"))) |>
    dplyr::mutate(
      model_id = model_ids,
      components = purrr::map(
        .x = .data[["varhp"]],
        .f = extract_model_components
      )
    ) |>
    dplyr::select(
      dplyr::all_of(c(id_cols, "model_id", "components"))
    ) |>
    tidyr::unnest(cols = "components") |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(id_cols, "model_id")
        )
      )
    ) |>
    dplyr::mutate(
      has_required_predictors = all(.data[["predictor_present"]]),
      has_finite_total = all(
        is.finite(.data[["total_adjusted_r_squared"]])
      ),
      has_positive_total = dplyr::first(.data[["has_finite_total"]]) &&
        dplyr::first(.data[["total_adjusted_r_squared"]]) > 0,
      has_finite_individual = all(
        is.finite(.data[["individual"]])
      ),
      has_finite_varpart =
        all(.data[["varpart_available"]]) &&
        all(is.finite(.data[["varpart_unique"]])) &&
        all(is.finite(.data[["shared"]])) &&
        all(is.finite(.data[["varpart_total"]])),
      has_negative_unique = any(
        .data[["unique"]] < 0,
        na.rm = TRUE
      ),
      has_negative_individual = any(
        .data[["individual"]] < 0,
        na.rm = TRUE
      ),
      is_importance_eligible =
        all(.data[["model_result_available"]]) &&
        dplyr::first(.data[["has_required_predictors"]]) &&
        dplyr::first(.data[["has_finite_total"]]) &&
        dplyr::first(.data[["has_positive_total"]]) &&
        dplyr::first(.data[["has_finite_individual"]]),
      exclusion_reason = dplyr::case_when(
        !all(.data[["model_result_available"]]) ~ "missing_result",
        !dplyr::first(.data[["has_required_predictors"]]) ~
          "missing_predictor",
        !dplyr::first(.data[["has_finite_total"]]) ~ "non_finite_total",
        !dplyr::first(.data[["has_positive_total"]]) ~
          "non_positive_total",
        !dplyr::first(.data[["has_finite_individual"]]) ~
          "non_finite_individual",
        .default = NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::relocate(dplyr::all_of(c(id_cols, "model_id", "predictor")))

  return(data_importance)
}
