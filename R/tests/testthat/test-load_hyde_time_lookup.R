testthat::test_that("load_hyde_time_lookup() converts HYDE time steps", {
  path_nc <-
    base::tempfile(fileext = ".nc")

  dim_lon <-
    ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = 0.5)

  dim_lat <-
    ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = 0.5)

  dim_time <-
    ncdf4::ncdim_def(
      name = "time",
      units = "days since 1-5-1 00:00:00",
      vals = c(-3650365, -365, 36135),
      calendar = "365_day"
    )

  var_value <-
    ncdf4::ncvar_def(
      name = "cropland",
      units = "km**2",
      dim = list(dim_lon, dim_lat, dim_time),
      missval = -9999
    )

  nc <-
    ncdf4::nc_create(path_nc, vars = list(var_value))

  ncdf4::ncvar_put(nc, varid = var_value, vals = c(1, 2, 3))

  ncdf4::nc_close(nc)

  res <-
    load_hyde_time_lookup(
      file_paths = c(path_nc, path_nc),
      source_id = "hyde_3_5"
    )

  testthat::expect_named(
    res,
    c("source_id", "layer_index", "calendar_year", "age_bp")
  )
  testthat::expect_equal(dplyr::pull(res, "calendar_year"), c(-10000, 0, 100))
  testthat::expect_equal(dplyr::pull(res, "age_bp"), c(11950, 1950, 1850))
  testthat::expect_equal(dplyr::pull(res, "layer_index"), 1:3)
  testthat::expect_true(base::all(dplyr::pull(res, "source_id") == "hyde_3_5"))
})

testthat::test_that("load_hyde_time_lookup() rejects other time axes", {
  path_a <-
    base::tempfile(fileext = ".nc")

  path_b <-
    base::tempfile(fileext = ".nc")

  path_c <-
    base::tempfile(fileext = ".nc")

  path_d <-
    base::tempfile(fileext = ".nc")

  dim_lon <-
    ncdf4::ncdim_def(name = "lon", units = "degrees_east", vals = 0.5)

  dim_lat <-
    ncdf4::ncdim_def(name = "lat", units = "degrees_north", vals = 0.5)

  list_time <-
    list(
      a = ncdf4::ncdim_def(
        name = "time",
        units = "days since 1-5-1 00:00:00",
        vals = c(-365, 36135),
        calendar = "365_day"
      ),
      b = ncdf4::ncdim_def(
        name = "time",
        units = "days since 1-5-1 00:00:00",
        vals = c(-365, 36600),
        calendar = "365_day"
      ),
      c = ncdf4::ncdim_def(
        name = "time",
        units = "hours since 1-5-1 00:00:00",
        vals = c(-365, 36135),
        calendar = "365_day"
      ),
      d = ncdf4::ncdim_def(
        name = "time",
        units = "days since 1-5-1 00:00:00",
        vals = c(-365, 36500),
        calendar = "365_day"
      )
    )

  vec_paths <-
    c(a = path_a, b = path_b, c = path_c, d = path_d)

  purrr::walk(
    .x = c("a", "b", "c", "d"),
    .f = ~ {
      var_value <-
        ncdf4::ncvar_def(
          name = "cropland",
          units = "km**2",
          dim = list(dim_lon, dim_lat, purrr::chuck(list_time, .x)),
          missval = -9999
        )

      nc <-
        ncdf4::nc_create(purrr::chuck(vec_paths, .x), vars = list(var_value))

      ncdf4::ncvar_put(nc, varid = var_value, vals = c(1, 2))

      ncdf4::nc_close(nc)
    }
  )

  testthat::expect_error(
    load_hyde_time_lookup(
      file_paths = c(path_a, path_d),
      source_id = "hyde_3_5"
    ),
    regexp = "differ"
  )
  testthat::expect_error(
    load_hyde_time_lookup(
      file_paths = path_b,
      source_id = "hyde_3_5"
    ),
    regexp = "whole"
  )
  testthat::expect_error(
    load_hyde_time_lookup(
      file_paths = path_c,
      source_id = "hyde_3_5"
    ),
    regexp = "days since"
  )
  testthat::expect_error(
    load_hyde_time_lookup(
      file_paths = base::tempfile(fileext = ".nc"),
      source_id = "hyde_3_5"
    ),
    regexp = "contract"
  )
})
