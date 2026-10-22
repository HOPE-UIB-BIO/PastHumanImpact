testthat::test_that("joint human-proxy inputs match both H1 structures", {
  within <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      data_merge = list(
        tibble::tibble(age = c(2000, 2500, 8500), response = 1:3),
        tibble::tibble(age = c(2000, 2500), response = 4:5)
      )
    )
  slices <-
    tibble::tibble(
      region = c("Europe", "Asia"),
      age = c(2000, 2500),
      data_merge = list(
        tibble::tibble(dataset_id = "a", response = 1),
        tibble::tibble(dataset_id = "b", response = 2)
      ),
      n_samples = 1L
    )
  proxies <-
    tidyr::crossing(
      dataset_id = c("a", "b"),
      age_bp = c(2000, 2500, 8500)
    ) |>
    dplyr::mutate(
      region = dplyr::if_else(.data[["dataset_id"]] == "a", "Europe", "Asia"),
      spd_transformed = 1,
      kk10_transformed = 2,
      hyde_transformed = 3
    )
  metadata <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      region = c("Europe", "Asia")
    )

  result <-
    prepare_joint_human_proxy_hvarpart_inputs(
      data_within_dataset = within,
      data_time_slices = slices,
      data_proxy_matches = proxies,
      data_metadata = metadata
    )

  testthat::expect_identical(
    purrr::map_int(result[["within_dataset"]][["data_merge"]], nrow),
    c(2L, 2L)
  )
  testthat::expect_identical(result[["time_slices"]][["n_samples"]], c(1L, 1L))
  testthat::expect_false(any(result[["matched_values"]][["age"]] == 8500))
  testthat::expect_true(
    all(
      c("spd_transformed", "kk10_transformed", "hyde_transformed") %in%
        names(result[["within_dataset"]][["data_merge"]][[1]])
    )
  )
})

testthat::test_that("joint human-proxy inputs reject invalid keys and regions", {
  within <-
    tibble::tibble(
      dataset_id = "a",
      data_merge = list(tibble::tibble(age = 2000, response = 1))
    )
  slices <-
    tibble::tibble(
      region = "Europe",
      age = 2000,
      data_merge = list(tibble::tibble(dataset_id = "a", response = 1))
    )
  proxy <-
    tibble::tibble(
      dataset_id = "a",
      age_bp = 2000,
      region = "Europe",
      spd_transformed = 1,
      kk10_transformed = 2,
      hyde_transformed = 3
    )
  metadata <- tibble::tibble(dataset_id = "a", region = "Europe")

  testthat::expect_error(
    prepare_joint_human_proxy_hvarpart_inputs(
      within,
      slices,
      dplyr::bind_rows(proxy, proxy),
      metadata
    ),
    regexp = "keys must be unique"
  )
  testthat::expect_error(
    prepare_joint_human_proxy_hvarpart_inputs(
      within,
      slices,
      proxy,
      dplyr::mutate(metadata, region = "Asia")
    ),
    regexp = "agree with canonical"
  )
})

testthat::test_that("selected KK10 inputs do not require SPD or HYDE", {
  data_within <-
    tibble::tibble(
      dataset_id = "a",
      data_merge = list(tibble::tibble(age = c(2000, 2500), response = 1:2))
    )

  data_slices <-
    tibble::tibble(
      region = "Europe",
      age = 2000,
      data_merge = list(tibble::tibble(dataset_id = "a", response = 1))
    )

  data_proxy <-
    tibble::tibble(
      dataset_id = "a",
      age_bp = c(2000, 2500),
      region = "Europe",
      kk10_transformed = c(0, 0.3)
    )

  data_metadata <-
    tibble::tibble(dataset_id = "a", region = "Europe")

  res <-
    prepare_joint_human_proxy_hvarpart_inputs(
      data_within_dataset = data_within,
      data_time_slices = data_slices,
      data_proxy_matches = data_proxy,
      data_metadata = data_metadata,
      human_columns = "kk10_transformed"
    )

  testthat::expect_identical(
    res[["within_dataset"]][["data_merge"]][[1]][["kk10_transformed"]],
    c(0, 0.3)
  )

  testthat::expect_identical(res[["time_slices"]][["n_samples"]], 1L)

  testthat::expect_false(
    any(c("spd_transformed", "hyde_transformed") %in%
      names(res[["matched_values"]]))
  )

  testthat::expect_error(
    prepare_joint_human_proxy_hvarpart_inputs(
      data_within, data_slices, data_proxy, data_metadata,
      human_columns = character()
    ),
    regexp = "human_columns"
  )

  testthat::expect_error(
    prepare_joint_human_proxy_hvarpart_inputs(
      data_within, data_slices,
      dplyr::mutate(data_proxy, kk10_transformed = Inf), data_metadata,
      human_columns = "kk10_transformed"
    ),
    regexp = "finite"
  )

  testthat::expect_error(
    prepare_joint_human_proxy_hvarpart_inputs(
      data_within, data_slices, data_proxy, data_metadata,
      human_columns = c("kk10_transformed", "kk10_transformed")
    ),
    regexp = "human_columns"
  )
})
