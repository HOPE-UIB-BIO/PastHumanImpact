#' @title Load the source-layer ages of HYDE NetCDF files
#' @description
#' Read the time axis of one or more HYDE NetCDF files and convert it to
#' calendar years and years before 1950.
#' @param file_paths
#' Character vector of existing HYDE NetCDF paths. All files must share one
#' time axis.
#' @param source_id
#' Character scalar source identifier written to the output.
#' @param time_dimension
#' Character scalar name of the time dimension. Default `time`.
#' @return
#' Tibble with `source_id`, `layer_index`, `calendar_year`, and `age_bp`,
#' one row per time step in file order.
#' @details
#' HYDE stores time as `days since <year>-<month>-<day>` on a `365_day`
#' calendar, one step per output year. The calendar year is the reference
#' year plus days / 365, so negative years are BCE and year 0 is the step
#' HYDE labels `0AD`. Ages are 1950 minus the calendar year. Any other
#' calendar or unit, a non-integer year, or time axes that differ between
#' files are an error.
#' @examples
#' \dontrun{
#' load_hyde_time_lookup(c("cropland.nc", "grazing_land.nc"), "hyde_3_5")
#' }
load_hyde_time_lookup <- function(
  file_paths,
  source_id,
  time_dimension = "time"
) {
  assertthat::assert_that(
    is.character(file_paths),
    length(file_paths) > 0L,
    all(file.exists(file_paths)),
    assertthat::is.string(source_id),
    assertthat::is.string(time_dimension),
    msg = "HYDE time-lookup inputs do not satisfy the contract."
  )

  list_calendar_years <-
    file_paths |>
    purrr::map(
      .f = ~ {
        nc <- ncdf4::nc_open(.x)

        on.exit(ncdf4::nc_close(nc), add = TRUE)

        dim_time <- nc[["dim"]][[time_dimension]]

        if (
          is.null(dim_time)
        ) {
          cli::cli_abort(
            "HYDE file {.path {.x}} has no {.val {time_dimension}} dimension."
          )
        }

        vec_units_match <-
          stringr::str_match(
            string = dim_time[["units"]],
            pattern = "^days since (-?[0-9]+)-[0-9]+-[0-9]+"
          )

        if (
          is.na(vec_units_match[1, 1])
        ) {
          cli::cli_abort(
            c(
              "HYDE time units must be 'days since <year>-<month>-<day>'.",
              "x" = "{.path {.x}} uses {.val {dim_time[['units']]}}."
            )
          )
        }

        list_calendar <-
          ncdf4::ncatt_get(nc, varid = time_dimension, attname = "calendar")

        if (
          !isTRUE(list_calendar[["hasatt"]]) ||
            !list_calendar[["value"]] %in% c("365_day", "noleap")
        ) {
          cli::cli_abort(
            "HYDE time in {.path {.x}} must use a 365-day calendar."
          )
        }

        vec_years <-
          as.numeric(vec_units_match[1, 2]) +
          as.numeric(dim_time[["vals"]]) / 365

        if (
          any(abs(vec_years - round(vec_years)) > 1e-9)
        ) {
          cli::cli_abort(
            "HYDE time steps in {.path {.x}} are not whole years."
          )
        }

        round(vec_years)
      }
    )

  flag_same_axis <-
    list_calendar_years |>
    purrr::map_lgl(
      .f = ~ identical(.x, list_calendar_years[[1]])
    ) |>
    all()

  if (
    !flag_same_axis
  ) {
    cli::cli_abort("HYDE time axes differ between files.")
  }

  vec_calendar_years <-
    list_calendar_years[[1]]

  res_lookup <-
    tibble::tibble(
      source_id = source_id,
      layer_index = seq_along(vec_calendar_years),
      calendar_year = vec_calendar_years,
      age_bp = 1950 - vec_calendar_years
    )

  return(res_lookup)
}
