testthat::test_that("human-proxy spatial balance wrapper fits each model", {
  n <- 12L
  records <- tibble::tibble(
    model_id = "joint", dataset_id = as.character(seq_len(n)),
    long = seq(0, 11), lat = seq(40, 51),
    region = rep(c("Europe", "Asia"), each = 6),
    climatezone = rep(c("Cold", "Arid"), 6),
    signed_difference = seq(-0.3, 0.3, length.out = n),
    signed_weight = 1, zero_balance = seq(-0.5, 0.5, length.out = n),
    zero_weight = 1
  )
  result <- fit_human_proxy_spatial_balance_models(
    records, permutations = 9L, min_unique_locations = 20L,
    min_residual_df = 2L, distance_km = 500
  )
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_true("estimates" %in% names(result[["result"]][[1]]))
  testthat::expect_error(
    fit_human_proxy_spatial_balance_models(data.frame(x = 1)),
    "contract"
  )
})
