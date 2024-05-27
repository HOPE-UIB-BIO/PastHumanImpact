#' @title Function to create a dataframe of m2 and time
#' @param data The vector of time m2

get_m2_time_df <- function(data) {
  df <- data %>%
    data.frame(delta_m2 = .) %>%
    tibble::rownames_to_column("time")
  return(df)
}
