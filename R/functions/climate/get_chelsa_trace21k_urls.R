#' @title A function to select palaeoclimatic variables from CHELSA
#' @description This function create a tibble of palaeoclimatic variables, file names, and urls
#' @return A tibble to be used with the_get chelsa_download function
get_chelsa_trace21k_urls <- function(variables = c("bio", "tasmin", "tasmax", "pr"),
                                     name = "CHELSA_TraCE21k",
                                     bio_var = c(1:19),
                                     month_var = c(1:12),
                                     time_var = c(20:-220)) {
  assertthat::assert_that(
    is.character(variables) && length(variables) > 0,
    msg = "`variables` must be a non-empty character vector."
  )

  assertthat::assert_that(
    all(variables %in% c("bio", "tasmin", "tasmax", "pr")),
    msg = "`variables` must contain only: bio, tasmin, tasmax, pr."
  )

  assertthat::assert_that(
    is.character(name) && length(name) == 1,
    msg = "`name` must be a single character value."
  )

  assertthat::assert_that(
    is.numeric(bio_var),
    msg = "`bio_var` must be numeric."
  )

  assertthat::assert_that(
    is.numeric(month_var),
    msg = "`month_var` must be numeric."
  )

  assertthat::assert_that(
    is.numeric(time_var),
    msg = "`time_var` must be numeric."
  )

  base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/chelsa_trace/"

  res_data <-
    tidyr::expand_grid(
      model = name,
      time_id = time_var,
      variable = variables,
      bio = bio_var,
      month = month_var
    ) %>%
    dplyr::mutate(
      month = replace(month, variable == "bio", NA),
      bio = replace(bio, variable != "bio", NA),
      histdir = dplyr::case_when(
        variable == "bio" ~ "bio/",
        variable == "tasmin" ~ "tasmin/",
        variable == "tasmax" ~ "tasmax/",
        variable == "pr" ~ "pr/"
      ),
      file = dplyr::case_when(
        variable == "bio" ~
          paste0(
            histdir,
            model, "_",
            variable, stringr::str_pad(bio, 2, "left", "0"), "_", time_id, "_V1.0.tif"
          ),
        variable != "bio" ~
          paste0(
            histdir,
            model, "_",
            variable, "_",
            month, "_", time_id, "_V1.0.tif"
          )
      ),
      url = paste0(base_url, file)
    ) %>%
    # dplyr::select(file, url) %>%
    dplyr::distinct()

  return(res_data)
}
