#' @title Build human-proxy HVarPart design-diagnostic specifications
#' @description
#' Define the single-proxy model variants used to diagnose how the climate
#' representation, the residual degree-of-freedom minimum, and trend sharing
#' between a human proxy and time affect time-controlled HVarPart results.
#' @return
#' A seven-row tibble with columns `model_id`, `model_label`,
#' `human_candidates` (list), `human_preference` (list), `exploratory`,
#' `proxy`, `proxy_variant`, `climate_representation`,
#' `within_dataset_profile_id`, and `time_slice_profile_id`.
#' @details
#' `proxy` is one of `"spd"`, `"kk10"`, or `"hyde"`, using the matched
#' columns `spd_sqrt`, `kk10_fraction`, and `hyde_sqrt`. `proxy_variant` is
#' `"original"` or `"age_detrended"`; detrended variants use the column
#' `<proxy column>_age_detrended`. `climate_representation` is `"full"` when
#' every valid climate candidate is kept and `"filtered"` when the shared
#' response-free selection is used. Only the two original SPD variants have a
#' region-age (`time_slice_profile_id`) counterpart; the others carry `NA`.
#' @examples
#' build_human_proxy_diagnostic_specifications()
build_human_proxy_diagnostic_specifications <- function() {
  vec_proxy_columns <-
    c(spd = "spd_sqrt", kk10 = "kk10_fraction", hyde = "hyde_sqrt")

  vec_proxy_labels <-
    c(
      spd = "Square-root SPD",
      kk10 = "KK10 land-use fraction",
      hyde = "Square-root HYDE population"
    )

  data_variants <-
    tibble::tribble(
      ~proxy, ~proxy_variant, ~climate_representation,
      "spd", "original", "full",
      "spd", "original", "filtered",
      "spd", "age_detrended", "filtered",
      "kk10", "original", "filtered",
      "kk10", "age_detrended", "filtered",
      "hyde", "original", "filtered",
      "hyde", "age_detrended", "filtered"
    )

  data_specs <-
    data_variants |>
    dplyr::mutate(
      proxy_stem = dplyr::if_else(
        .data[["proxy_variant"]] == "age_detrended",
        stringr::str_c(.data[["proxy"]], "_age_detrended"),
        .data[["proxy"]]
      ),
      human_column = dplyr::if_else(
        .data[["proxy_variant"]] == "age_detrended",
        stringr::str_c(
          unname(vec_proxy_columns[.data[["proxy"]]]),
          "_age_detrended"
        ),
        unname(vec_proxy_columns[.data[["proxy"]]])
      ),
      model_id = stringr::str_c(
        .data[["proxy_stem"]],
        "__",
        .data[["climate_representation"]],
        "_climate"
      ),
      model_label = stringr::str_c(
        unname(vec_proxy_labels[.data[["proxy"]]]),
        dplyr::if_else(
          .data[["proxy_variant"]] == "age_detrended",
          " detrended on age",
          ""
        ),
        dplyr::if_else(
          .data[["climate_representation"]] == "full",
          ", all four climate variables",
          ", filtered climate selection"
        )
      ),
      human_candidates = as.list(.data[["human_column"]]),
      human_preference = as.list(.data[["human_column"]]),
      exploratory = TRUE,
      within_dataset_profile_id = stringr::str_c(
        "within_dataset_",
        .data[["proxy_stem"]],
        "_",
        .data[["climate_representation"]],
        "_climate_design_diagnostic_time_control"
      ),
      time_slice_profile_id = dplyr::if_else(
        .data[["proxy_variant"]] == "original" &
          .data[["proxy"]] == "spd",
        stringr::str_c(
          "time_slice_spd_",
          .data[["climate_representation"]],
          "_climate_design_diagnostic_spatial_control"
        ),
        NA_character_
      )
    )

  res <-
    data_specs |>
    dplyr::select(
      dplyr::all_of(c(
        "model_id", "model_label", "human_candidates", "human_preference",
        "exploratory", "proxy", "proxy_variant", "climate_representation",
        "within_dataset_profile_id", "time_slice_profile_id"
      ))
    )

  assertthat::assert_that(
    nrow(res) == 7L,
    !anyDuplicated(res[["model_id"]]),
    !anyDuplicated(res[["within_dataset_profile_id"]]),
    msg = "Human-proxy diagnostic specifications are inconsistent."
  )

  return(res)
}
