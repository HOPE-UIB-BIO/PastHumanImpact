#' @title Load analysis profiles
#' @description
#' Load and validate the declarative project analysis-profile registry.
#' @param path Character scalar path to the profile CSV file.
#' @return Validated tibble containing the analysis profiles.
#' @examples
#' \dontrun{
#' profiles <- load_analysis_profiles("R/analyses/00_profiles/profiles.csv")
#' }
load_analysis_profiles <- function(path) {
  assertthat::assert_that(
    is.character(path),
    length(path) == 1L,
    file.exists(path),
    msg = "`path` must identify an existing profile CSV file."
  )

  res_profiles <-
    readr::read_csv(
      file = path,
      show_col_types = FALSE
    )

  validate_analysis_profiles(data_profiles = res_profiles)

  return(res_profiles)
}
