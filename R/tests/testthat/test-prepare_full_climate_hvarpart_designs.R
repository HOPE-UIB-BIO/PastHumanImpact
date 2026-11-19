testthat::test_that("prepare_full_climate_hvarpart_designs() keeps all four", {
  set.seed(900723)

  data_unit <-
    tibble::tibble(
      spd_sqrt = stats::runif(12),
      temp_annual = stats::rnorm(12),
      temp_cold = stats::rnorm(12),
      prec_summer = stats::rnorm(12),
      prec_win = stats::rnorm(12)
    )

  data_designs <-
    tibble::tibble(
      dataset_id = c("a", "a"),
      model_id = c("full", "filtered"),
      selection_status = "eligible_for_design_check",
      data_merge = list(data_unit, data_unit),
      predictor_vars = list(
        list(human = "spd_sqrt", climate = "temp_annual"),
        list(human = "spd_sqrt", climate = "temp_annual")
      )
    )

  result <-
    prepare_full_climate_hvarpart_designs(
      data_designs = data_designs,
      model_ids = "full"
    )

  list_vars <-
    dplyr::pull(result, "predictor_vars")

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_identical(
    list_vars[[1]][["climate"]],
    c("temp_annual", "temp_cold", "prec_summer", "prec_win")
  )
  testthat::expect_identical(list_vars[[1]][["human"]], "spd_sqrt")
  testthat::expect_identical(list_vars[[2]][["climate"]], "temp_annual")
  testthat::expect_identical(
    dplyr::pull(result, "climate_representation"),
    c("full", "selected")
  )
  testthat::expect_identical(
    dplyr::pull(result, "data_merge"),
    dplyr::pull(data_designs, "data_merge")
  )
})

testthat::test_that("prepare_full_climate_hvarpart_designs() drops invalid", {
  data_unit <-
    tibble::tibble(
      spd_sqrt = c(1, 2, 3),
      temp_annual = c(1, 1, 1),
      temp_cold = c(1, NA, 2),
      prec_summer = c(3, 1, 2),
      prec_win = c(1, 2, 4)
    )

  data_designs <-
    tibble::tibble(
      dataset_id = c("a", "b", "c"),
      model_id = c("full", "full", "full"),
      selection_status = c(
        "missing_climate_predictor",
        "missing_human_predictor",
        "climate_selection_error"
      ),
      data_merge = list(
        data_unit,
        data_unit,
        dplyr::select(data_unit, -dplyr::all_of(c("prec_summer", "prec_win")))
      ),
      predictor_vars = list(
        list(human = "spd_sqrt", climate = character()),
        list(human = character(), climate = "prec_win"),
        list(human = "spd_sqrt", climate = character())
      )
    )

  result <-
    prepare_full_climate_hvarpart_designs(
      data_designs = data_designs,
      model_ids = "full"
    )

  list_climate <-
    dplyr::pull(result, "predictor_vars") |>
    purrr::map("climate")

  testthat::expect_identical(list_climate[[1]], c("prec_summer", "prec_win"))
  testthat::expect_identical(
    dplyr::pull(result, "selection_status"),
    c(
      "eligible_for_design_check",
      "missing_human_predictor",
      "missing_climate_predictor"
    )
  )
  testthat::expect_identical(list_climate[[3]], character())
})

testthat::test_that("prepare_full_climate_hvarpart_designs() validates", {
  data_designs <-
    tibble::tibble(
      model_id = "full",
      selection_status = "eligible_for_design_check",
      data_merge = list(tibble::tibble(x = 1)),
      predictor_vars = list(list(human = "x", climate = character()))
    )

  testthat::expect_error(
    prepare_full_climate_hvarpart_designs(
      data_designs = data_designs,
      model_ids = "absent"
    )
  )
  testthat::expect_error(
    prepare_full_climate_hvarpart_designs(
      data_designs = dplyr::select(data_designs, -"predictor_vars"),
      model_ids = "full"
    )
  )
})
