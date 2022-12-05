
data_to_plot <-
  dplyr::inner_join(
    data_filtered %>% 
      dplyr::select(dataset_id, ecozone_koppen_5),
    data_climate_pred %>% 
      tidyr::unnest(clim_data_pred) %>% 
      dplyr::filter(age <= 12e3 & age >= 0))

plot_list <-
  c("temp_cold",
    "prec_summer",
    "prec_winter",
    "gdm") %>% 
  purrr::set_names() %>% 
  purrr::map(
    .x  = .,
    .f = ~ data_to_plot %>% 
      ggplot2::ggplot(
        ggplot2::aes(
          x = age,
          y = get(.x),
          col = ecozone_koppen_5))+
      ggplot2::geom_line(
        alpha = 0.1,
        ggplot2::aes(group = dataset_id))+
      ggplot2::geom_smooth(
        method = "gam",
        formula = y ~ s(x, bs = "tp"))+
      ggplot2::scale_x_continuous(
        trans = "reverse")+
      ggplot2::labs(
        x = "Age (cal yr BP)",
        y = eval(.x)))

ggpubr::ggarrange(
  plotlist = plot_list,
  common.legend = TRUE)