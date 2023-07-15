# get summary tables


get_r2_summary <- function(data_table, sel_var, group_vars){
  
  data_table %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_vars)
      )
    ) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(sel_var),
        list(
          median = ~ median(.x, na.rm = TRUE),
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          upr = ~ stats::quantile(.x, 0.975, na.rm = TRUE),
          lwr = ~ stats::quantile(.x, 0.025, na.rm = TRUE)
        )
      )
    ) %>%
    return()
  
}
