plot_hgam_result <- function(
    data_source_raw,
    data_source_pred,
    x_var = "age",
    y_var = "value",
    group_var = "dataset_id",
    x_var_name = "Age (cal yr BP)",
    y_var_name = "var",
    title = NULL) {
  data_source_raw %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = get(x_var),
        y = get(y_var),
      )
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        group = get(group_var)
      ),
      linewidth = 0.1
    ) +
    ggplot2::geom_ribbon(
      data = data_source_pred,
      mapping = ggplot2::aes(
        ymin = lwr,
        ymax = upr
      ),
      alpha = 0.2,
      col = "gray80"
    ) +
    ggplot2::geom_line(
      data = data_source_pred,
      linewidth = 1
    ) +
    ggplot2::scale_x_continuous(
      trans = "reverse"
    ) +
    ggplot2::labs(
      title = title,
      x = x_var_name,
      y = y_var_name
    )
}