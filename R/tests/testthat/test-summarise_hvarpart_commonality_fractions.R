testthat::test_that("summarise_hvarpart_commonality_fractions() sums to R2", {
  set.seed(900723)

  n_rows <- 25L

  data_fit <-
    tibble::tibble(
      age = seq_len(n_rows),
      human = seq_len(n_rows) / 5 + stats::rnorm(n_rows),
      clim_a = stats::rnorm(n_rows),
      clim_b = stats::rnorm(n_rows)
    ) |>
    dplyr::mutate(
      time = as.numeric(scale(.data[["age"]])),
      y_1 = .data[["human"]] + .data[["clim_a"]] + stats::rnorm(n_rows),
      y_2 = .data[["time"]] - .data[["clim_b"]] + stats::rnorm(n_rows),
      y_3 = stats::rnorm(n_rows)
    )

  fit_result <-
    fit_varhp(
      data_source = data_fit,
      response_vars = c("y_1", "y_2", "y_3"),
      predictor_vars = list(
        human = "human",
        climate = c("clim_a", "clim_b"),
        time = "time"
      ),
      get_significance = FALSE
    )

  data_fits <-
    tibble::tibble(
      model_id = c("m", "m"),
      dataset_id = c("fitted", "skipped"),
      result = list(
        list(temporal_hvarpart = fit_result),
        list(temporal_hvarpart = NULL)
      )
    )

  result <-
    summarise_hvarpart_commonality_fractions(data_fits = data_fits)

  testthat::expect_named(
    result,
    c(
      "model_id", "dataset_id", "fraction_label", "groups", "n_groups",
      "fraction", "percent_total", "total_fraction"
    )
  )
  testthat::expect_equal(nrow(result), 7L)
  testthat::expect_identical(
    unique(dplyr::pull(result, "dataset_id")),
    "fitted"
  )

  rda_full <-
    vegan::rda(
      data_fit[c("y_1", "y_2", "y_3")] ~ human + clim_a + clim_b + time,
      data = data_fit,
      scale = TRUE
    )

  adjusted_r2 <-
    vegan::RsquareAdj(rda_full)[["adj.r.squared"]]

  testthat::expect_equal(
    sum(dplyr::pull(result, "fraction")),
    adjusted_r2,
    tolerance = 1e-3
  )
  testthat::expect_equal(
    sum(dplyr::pull(result, "fraction")),
    unique(dplyr::pull(result, "total_fraction")),
    tolerance = 1e-3
  )
})

testthat::test_that("summarise_hvarpart_commonality_fractions() parses", {
  mat_var_part <-
    matrix(
      c(0.1, 0.2, 0.05, -0.01, 0.03, 0.02, 0.01, 0.4, rep(0, 8)),
      ncol = 2L,
      dimnames = list(
        c(
          "Unique to human                    ",
          "Unique to climate                  ",
          "Unique to time                     ",
          "Common to human, and climate       ",
          "Common to human, and time          ",
          "Common to climate, and time        ",
          "Common to human, climate, and time ",
          "Total                              "
        ),
        c("Fractions", "  % Total")
      )
    )

  data_fits <-
    tibble::tibble(
      model_id = "m",
      dataset_id = "a",
      result = list(list(
        temporal_hvarpart = list(varhp_output = list(Var.part = mat_var_part))
      ))
    )

  result <-
    summarise_hvarpart_commonality_fractions(data_fits = data_fits)

  testthat::expect_identical(
    dplyr::pull(result, "groups"),
    c(
      "human", "climate", "time", "human&climate", "human&time",
      "climate&time", "human&climate&time"
    )
  )
  testthat::expect_identical(
    dplyr::pull(result, "n_groups"),
    c(1L, 1L, 1L, 2L, 2L, 2L, 3L)
  )
  testthat::expect_identical(
    dplyr::pull(result, "fraction_label")[[5]],
    "Common to human, and time"
  )
  testthat::expect_equal(unique(dplyr::pull(result, "total_fraction")), 0.4)
})

testthat::test_that("summarise_hvarpart_commonality_fractions() edge cases", {
  data_empty <-
    tibble::tibble(
      model_id = "m",
      dataset_id = "a",
      result = list(list(temporal_hvarpart = NULL))
    )

  result <-
    summarise_hvarpart_commonality_fractions(data_fits = data_empty)

  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_true("fraction" %in% names(result))

  testthat::expect_error(
    summarise_hvarpart_commonality_fractions(
      data_fits = data_empty,
      id_columns = "absent"
    )
  )
  testthat::expect_error(
    summarise_hvarpart_commonality_fractions(data_fits = list())
  )
})
