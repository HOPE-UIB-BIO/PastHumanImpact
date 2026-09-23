#' @title Prepare the HYDE 3.2 population raster from nested archives
#' @description
#' Extract `popc.tif` from the `raw-data.zip` reproducibility archive, which
#' contains nested `HYDE.zip` and `popc.tif.zip` archives.
#' @param archive_path Path to the downloaded `raw-data.zip` file.
#' @param destination Final path for `popc.tif`.
#' @param overwrite Logical. Replace an existing destination when `TRUE`.
#' @return Normalized path to the extracted population raster.
#' @examples
#' \dontrun{
#' prepare_hyde_population_raster("raw-data.zip", "popc.tif")
#' }
prepare_hyde_population_raster <- function(
  archive_path,
  destination,
  overwrite = FALSE
) {
  assertthat::assert_that(
    assertthat::is.string(archive_path),
    file.exists(archive_path),
    assertthat::is.string(destination),
    is.logical(overwrite),
    length(overwrite) == 1L,
    msg = "HYDE archive extraction inputs do not satisfy the contract."
  )

  if (
    file.exists(destination) && !isTRUE(overwrite)
  ) {
    return(normalizePath(destination, winslash = "/", mustWork = TRUE))
  }

  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  path_staging <-
    tempfile(
      pattern = "hyde-extraction-",
      tmpdir = dirname(destination)
    )

  dir.create(path_staging, recursive = TRUE)

  on.exit(
    unlink(path_staging, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  archive_contents <- utils::unzip(archive_path, list = TRUE)
  hyde_entry <-
    archive_contents[["Name"]][
      stringr::str_detect(
        archive_contents[["Name"]],
        stringr::regex("(^|/)HYDE[.]zip$", ignore_case = TRUE)
      )
    ]

  if (
    length(hyde_entry) != 1L
  ) {
    cli::cli_abort("Expected one nested `HYDE.zip` in {.path {archive_path}}.")
  }

  utils::unzip(
    zipfile = archive_path,
    files = hyde_entry,
    exdir = path_staging
  )

  path_hyde_archive <- file.path(path_staging, hyde_entry)
  hyde_contents <- utils::unzip(path_hyde_archive, list = TRUE)
  population_archive_entry <-
    hyde_contents[["Name"]][
      stringr::str_detect(
        hyde_contents[["Name"]],
        stringr::regex("(^|/)popc[.]tif[.]zip$", ignore_case = TRUE)
      )
    ]

  if (
    length(population_archive_entry) != 1L
  ) {
    cli::cli_abort("Expected one nested `popc.tif.zip` in `HYDE.zip`.")
  }

  utils::unzip(
    zipfile = path_hyde_archive,
    files = population_archive_entry,
    exdir = path_staging
  )

  path_population_archive <-
    file.path(path_staging, population_archive_entry)
  population_contents <- utils::unzip(path_population_archive, list = TRUE)
  population_entry <-
    population_contents[["Name"]][
      stringr::str_detect(
        population_contents[["Name"]],
        stringr::regex("(^|/)popc[.]tif$", ignore_case = TRUE)
      )
    ]

  if (
    length(population_entry) != 1L
  ) {
    cli::cli_abort("Expected one `popc.tif` in `popc.tif.zip`.")
  }

  utils::unzip(
    zipfile = path_population_archive,
    files = population_entry,
    exdir = path_staging
  )

  path_population_staged <- file.path(path_staging, population_entry)

  if (
    file.exists(destination) && !isTRUE(unlink(destination) == 0L)
  ) {
    cli::cli_abort("Could not replace existing file {.path {destination}}.")
  }

  if (
    !isTRUE(file.rename(path_population_staged, destination))
  ) {
    cli::cli_abort("Could not finalize HYDE raster {.path {destination}}.")
  }

  res_path <- normalizePath(destination, winslash = "/", mustWork = TRUE)

  return(res_path)
}
