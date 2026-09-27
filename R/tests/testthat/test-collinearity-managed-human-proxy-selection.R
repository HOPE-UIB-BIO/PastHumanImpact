testthat::test_that("local selection is deterministic and response-independent", {
  set.seed(42)
  data <- tibble::tibble(
    preferred = seq_len(80),
    redundant = .data[["preferred"]] + stats::rnorm(80, sd = 0.01),
    independent = stats::rnorm(80),
    response_a = stats::rnorm(80),
    response_b = rev(.data[["response_a"]])
  )
  selected_a <- select_local_hvarpart_predictors(
    data, c("preferred", "redundant"), "independent",
    c("preferred", "redundant"), "independent", max_cor = 0.8, max_vif = 5
  )
  selected_b <- select_local_hvarpart_predictors(
    dplyr::mutate(data, response_a = .data[["response_b"]]),
    c("preferred", "redundant"), "independent",
    c("preferred", "redundant"), "independent", max_cor = 0.8, max_vif = 5
  )
  testthat::expect_identical(
    selected_a$predictor_vars, selected_b$predictor_vars
  )
  testthat::expect_identical(
    selected_a$predictor_vars$human, "preferred"
  )
  testthat::expect_equal(
    selected_a$thresholds, c(max_cor = 0.8, max_vif = 5)
  )
})

testthat::test_that("selection classifies invalid and empty groups", {
  data <- tibble::tibble(
    missing = c(1, 2, NA_real_), constant = 1, climate = c(1, 2, 3)
  )
  result <- select_local_hvarpart_predictors(
    data, c("missing", "constant"), "climate",
    c("missing", "constant"), "climate"
  )
  testthat::expect_identical(result$status, "missing_human_predictor")
  testthat::expect_setequal(
    result$audit$reason, c("non_finite", "constant", "retained")
  )
})

testthat::test_that("joint selection honours SPD then KK10 then HYDE preference", {
  set.seed(9)
  data <- tibble::tibble(
    spd_sqrt = seq_len(50),
    kk10_fraction = .data[["spd_sqrt"]] + stats::rnorm(50, sd = 0.001),
    hyde_sqrt = .data[["spd_sqrt"]] + stats::rnorm(50, sd = 0.001),
    climate = stats::rnorm(50)
  )
  result <- select_local_hvarpart_predictors(
    data,
    c("spd_sqrt", "kk10_fraction", "hyde_sqrt"), "climate",
    c("spd_sqrt", "kk10_fraction", "hyde_sqrt"), "climate"
  )
  testthat::expect_identical(result$predictor_vars$human, "spd_sqrt")
})

testthat::test_that("bridge and joint designs share the selected climate set", {
  set.seed(10)
  nested <- tibble::tibble(
    dataset_id = "one",
    data_merge = list(tibble::tibble(
      age = seq(2000, 8000, length.out = 30),
      spd_sqrt = sqrt(seq_len(30)),
      kk10_fraction = stats::runif(30), hyde_sqrt = seq_len(30),
      temp_annual = stats::rnorm(30), temp_cold = stats::rnorm(30),
      prec_summer = stats::rnorm(30), prec_win = stats::rnorm(30)
    ))
  )
  designs <- prepare_local_hvarpart_model_designs(
    nested, build_human_proxy_model_specifications(), "dataset_id"
  )
  testthat::expect_equal(nrow(designs), 2L)
  testthat::expect_identical(
    designs$model_id, c("joint_filtered", "spd_matched_bridge")
  )
  testthat::expect_true(
    length(designs$predictor_vars[[1]]$human) >= 1L
  )
  testthat::expect_true(
    length(designs$predictor_vars[[1]]$climate) >= 1L
  )
  testthat::expect_identical(
    designs$predictor_vars[[1]]$climate,
    designs$predictor_vars[[2]]$climate
  )
})

testthat::test_that("temporal figure requires exact unit stacks", {
  specs <- build_human_proxy_model_specifications() |>
    dplyr::filter(.data[["model_id"]] == "joint_filtered")
  data <- tidyr::crossing(
    analysis = "temporal_joint_human_proxies",
    model_id = "joint_filtered", region = "Europe",
    age = c(2000, 2500),
    predictor = c("human", "climate", "space")
  ) |>
    dplyr::mutate(allocation = 1 / 3)
  plot <- plot_human_proxy_temporal_comparison(data, specs)
  testthat::expect_s3_class(plot, "ggplot")
  bad <- dplyr::mutate(data, allocation = 0.2)
  testthat::expect_error(
    plot_human_proxy_temporal_comparison(bad, specs),
    "sum exactly to one"
  )
})
