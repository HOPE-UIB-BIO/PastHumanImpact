data_bracketing_lookup <-
  tibble::tibble(
    source_id = "hyde_3_5",
    layer_index = base::seq_len(6),
    age_bp = c(11950, 10950, 2950, 1950, 550, 450)
  )

testthat::test_that("select_human_proxy_bracketing_layers() brackets", {
  res <-
    select_human_proxy_bracketing_layers(
      data_lookup = data_bracketing_lookup,
      target_ages_bp = c(500, 11500, 2000)
    )

  testthat::expect_named(
    res,
    c("source_id", "source_layer_index", "layer_index", "age_bp")
  )
  testthat::expect_equal(
    dplyr::pull(res, "age_bp"),
    c(11950, 10950, 2950, 1950, 550, 450)
  )
  testthat::expect_equal(dplyr::pull(res, "layer_index"), 1:6)
  testthat::expect_equal(
    dplyr::pull(res, "source_layer_index"),
    1:6
  )
})

testthat::test_that("select_human_proxy_bracketing_layers() drops extras", {
  res <-
    select_human_proxy_bracketing_layers(
      data_lookup = data_bracketing_lookup,
      target_ages_bp = c(1950, 500)
    )

  testthat::expect_equal(dplyr::pull(res, "age_bp"), c(1950, 550, 450))
  testthat::expect_equal(dplyr::pull(res, "source_layer_index"), c(4L, 5L, 6L))
  testthat::expect_equal(dplyr::pull(res, "layer_index"), 1:3)
})

testthat::test_that("select_human_proxy_bracketing_layers() refuses gaps", {
  testthat::expect_error(
    select_human_proxy_bracketing_layers(
      data_lookup = data_bracketing_lookup,
      target_ages_bp = c(500, 12000)
    ),
    regexp = "extrapolat"
  )
  testthat::expect_error(
    select_human_proxy_bracketing_layers(
      data_lookup = data_bracketing_lookup,
      target_ages_bp = 400
    ),
    regexp = "extrapolat"
  )
  testthat::expect_error(
    select_human_proxy_bracketing_layers(
      data_lookup = dplyr::select(data_bracketing_lookup, -"layer_index"),
      target_ages_bp = 500
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    select_human_proxy_bracketing_layers(
      data_lookup = data_bracketing_lookup,
      target_ages_bp = c(500, NA)
    ),
    regexp = "contract"
  )
})
