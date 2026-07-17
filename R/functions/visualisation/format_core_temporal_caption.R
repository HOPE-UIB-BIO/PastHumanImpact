#' @title Format a core temporal figure caption
#' @description Format location, elevation, environment, and data-source notes.
#' @param data_metadata One-row core metadata data frame.
#' @return Character scalar figure caption.
#' @examples
#' caption <- format_core_temporal_caption(
#'   data.frame(
#'     long = 8.5,
#'     lat = 46.4,
#'     altitude = 1936,
#'     country = "Switzerland",
#'     depositionalenvironment = "Valley Mire"
#'   )
#' )
format_core_temporal_caption <- function(data_metadata) {
  assertthat::assert_that(
    is.data.frame(data_metadata),
    nrow(data_metadata) == 1L,
    msg = "`data_metadata` must be a one-row data frame."
  )

  required_columns <-
    c(
      "long",
      "lat",
      "altitude",
      "country",
      "depositionalenvironment"
    )

  assertthat::assert_that(
    all(required_columns %in% names(data_metadata)),
    msg = "Core metadata are missing caption columns."
  )
  assertthat::assert_that(
    is.numeric(data_metadata[["long"]]),
    is.numeric(data_metadata[["lat"]]),
    is.numeric(data_metadata[["altitude"]]),
    msg = "Core coordinates and altitude must be numeric."
  )

  longitude <- data_metadata[["long"]][1]
  latitude <- data_metadata[["lat"]][1]
  altitude <- data_metadata[["altitude"]][1]
  country <- data_metadata[["country"]][1]
  environment <- data_metadata[["depositionalenvironment"]][1]
  latitude_direction <-
    ifelse(latitude >= 0, "N", "S")
  longitude_direction <-
    ifelse(longitude >= 0, "E", "W")

  vec_metadata <-
    c(
      if (
        is.finite(latitude) && is.finite(longitude)
      ) {
        stringr::str_glue(
          "Coordinates: {abs(round(latitude, 3))} {latitude_direction}, ",
          "{abs(round(longitude, 3))} {longitude_direction}"
        )
      },
      if (
        is.finite(altitude)
      ) {
        stringr::str_glue("Elevation: {round(altitude)} m a.s.l.")
      },
      if (
        !is.na(country) && nzchar(country)
      ) {
        stringr::str_glue("Country: {country}")
      },
      if (
        !is.na(environment) && nzchar(environment)
      ) {
        stringr::str_glue("Environment: {environment}")
      }
    )

  if (
    "doi" %in% names(data_metadata) &&
      !is.na(data_metadata[["doi"]][1]) &&
      nzchar(data_metadata[["doi"]][1])
  ) {
    vec_metadata <-
      c(
        vec_metadata,
        stringr::str_glue("DOI: {data_metadata[[\"doi\"]][1]}")
      )
  }

  res_caption <-
    stringr::str_c(
      stringr::str_c(vec_metadata, collapse = " | "),
      "Raw values are shown where a pre-interpolation series exists; ",
      "density metrics begin on the common age grid.",
      sep = "\n"
    )

  return(res_caption)
}
