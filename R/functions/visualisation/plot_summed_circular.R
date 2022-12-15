plot_summed_circular <- function(data_source,
                                 group_vars = c(
                                   "region",
                                   "ecozone_koppen_5"
                                 ),
                                 col_var = "predictor",
                                 sel_mode = c(
                                   "individual",
                                   "unique",
                                   "average_share",
                                   "i_perc_percent"
                                 ),
                                 add_error = c("95%", "sd", FALSE),
                                 add_polygon = c("mean", "95%", "sd", FALSE),
                                 full_scale = FALSE) {
  group_vars <- as.character(group_vars)
  col_var <- as.character(col_var)
  sel_mode <- as.character(sel_mode)
  sel_mode <- match.arg(sel_mode)
  add_error <- as.character(add_error)
  add_error <- match.arg(add_error)
  add_polygon <- as.character(add_polygon)
  add_polygon <- match.arg(add_polygon)
  full_scale <- as.character(full_scale)

  # add all grouping vars together
  merged_group_vars <-
    c(
      "predictor", # always has to be there
      group_vars,
      col_var
    ) %>%
    unique()

  # get summary for continents
  data_summed <-
    get_r2_summary_varpar(
      data_source = data_source,
      sel_var = sel_mode,
      group_vars = merged_group_vars
    )

  plot_res <-
    plot_circular(
      data_source = data_summed,
      y_var_name = paste0(sel_mode, "_mean"),
      col_var_name = col_var,
      facet_var_name = group_vars,
      add_error = add_error,
      add_polygon = add_polygon,
      full_scale = full_scale
    )

  return(plot_res)
}
