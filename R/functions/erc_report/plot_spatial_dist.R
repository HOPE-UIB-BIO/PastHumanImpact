plot_spatial_dist <-
    function(data_source,
             base_map,
             var_name,
             lab_name,
             error_family) {

      # Scatterplot
       base_map <-
        data_source %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = long,
            y = lat
            )
          ) +
        ggplot2::coord_fixed(
          ylim = c(
            min(data_source$lat), 
            max(data_source$lat)
            ),
          xlim = c(
            min(data_source$long), 
            max(data_source$long)
            )
          ) +
        ggplot2::labs(
          x = "Longitude",
          y = "Latitude",
          colour = "Climate zones"
          ) +
        ggplot2::theme_classic() +
        ggplot2::borders(
          colour = "black",
          size = 0.2
          ) +
        theme(
              legend.position = "none"
              )
       
       plot_main <-
            base_map +
            ggplot2::geom_point(
                data = data_source,
                ggplot2::aes(
                  size = get(var_name),
                  colour = climate_zone,
                  ),
                alpha = 0.5
              ) +
          ggplot2::scale_colour_manual(
              values = palette_ecozone # [config]
              ) +
            ggplot2::scale_size_continuous(
              range = c(0, 5)
              ) +
            ggplot2::labs(
              size = lab_name
              ) +
          ggplot2::ggtitle("(a)") + 
         ggplot2::guides(
            fill = ggplot2::guide_legend(
              nrow = 1,
              byrow = TRUE,
              title.position = "top"
              ),
            size = ggplot2::guide_legend(
              nrow = 1,
              byrow = TRUE,
              title.position = "left"
              )
            ) +
          ggplot2::theme(
           plot.title = element_text(
              colour = "black", 
              size = 16,
              face = "bold",
              hjust = 0),
          legend.position = "bottom",
          legend.box = "horizonal",
          legend.direction = "horizontal",
          legend.key.size = unit(0.6, "cm"),
          legend.title = ggplot2::element_text(
            size = 12
            ),
          legend.text = ggplot2::element_text(
            size = 11
            ),
          legend.spacing.y = unit(0.05, 'cm'),
            
          axis.title = ggplot2::element_text(
            color = "black",
            size = 16
            ),
          axis.text = ggplot2::element_text(
            colour = "black",
            size = 12
            )
        )
        
        plot_main_legend <-
          ggpubr::get_legend(plot_main) 
        
        plot_main_no_legend <-
          plot_main +
          ggpubr::rremove("legend") 

        # Latitudinal and longitudinal trends
        data_pred_long <-
            make_figure_spatial_trend(
                data_source = data_source,
                side = "long",
                var_name = var_name,
                error_family = error_family
                )

        data_pred_lat <-
            make_figure_spatial_trend(
                data_source = data_source,
                side = "lat",
                var_name = var_name,
                error_family = error_family
                )

        lat_plot <-
            data_source %>%
            ggplot2::ggplot(
                ggplot2::aes(
                    x = lat,
                    y = get(var_name)
                    )
                ) +
              ggplot2::geom_point(
                col = "#2CA388",
                size = 1.5,
                alpha = 0.3
                ) +
            ggplot2::geom_ribbon(
                data = data_pred_lat,
                ggplot2::aes(
                    ymax = upr,
                    ymin = lwr
                    ),
                fill = "#D55E00",
                colour = "NA",
                alpha = 0.3
                ) +
            ggplot2::geom_line(
                data = data_pred_lat,
                linewidth = 1,
                colour = "#0072B2"
                ) +
            ggplot2::theme_classic() +
            ggplot2::labs(x = "Latitude", 
                          y = "") +
            ggplot2::ggtitle("(b)") + 
            ggplot2::theme(
              plot.title = element_text(
                colour = "black", 
                size = 16,
                face = "bold",
                hjust = 0),
                axis.title = ggplot2::element_text(
                    color = "black",
                    size = 16
                    ),
                axis.text = ggplot2::element_text(
                    color = "black",
                    size = 12
                    )
                )

        long_plot <-
            data_source %>%
            ggplot2::ggplot(
              ggplot2::aes(
                x = long,
                y = get(var_name)
                )
              ) +
              ggplot2::geom_point(
                col = "#2CA388",
                size = 1.5,
                alpha = 0.3
                ) +
            ggplot2::geom_ribbon(
                data = data_pred_long,
                ggplot2::aes(
                    ymax = upr,
                    ymin = lwr
                    ),
                fill = "#D55E00",
                colour = "NA",
                alpha = 0.3
                ) +
            ggplot2::geom_line(
                data = data_pred_long,
                linewidth = 1,
                colour = "#0072B2"
                ) +
            ggplot2::theme_classic() +
            ggplot2::labs(x = "Longitude", y = "") +
          ggplot2::ggtitle("(c)") + 
            ggplot2::theme(
              plot.title = element_text(
                colour = "black", 
                size = 16,
                face = "bold",
                hjust = 0),
                axis.title = ggplot2::element_text(
                    color = "black",
                    size = 16
                    ),
                axis.text = ggplot2::element_text(
                    color = "black",
                    size = 12
                    )
                )

        trends_merged <-
            ggpubr::ggarrange(
                lat_plot,
                long_plot,
                ncol = 1,
                nrow = 2,
                align = "v"
                ) 
        trends_merged_1 <- 
          annotate_figure(trends_merged,
                          left = text_grob(
                            "DCCA gradient length",
                            color = "black", 
                            size = 16,
                            rot = 90,
                            x = 1.5,
                            y = 0.53)
                          ) +
          theme(
            plot.margin = ggplot2::unit(
            c(0, -0.2, 0, -0.5), # t, r, b, l
            "cm"
            )
          )

        final_plot <-
            ggpubr::ggarrange(
              plot_main_no_legend,
              trends_merged_1,
              nrow = 1,
              ncol = 2,
              widths = c(0.7, 0.3)
              )
        
        final_plot_legend <-
          ggpubr::ggarrange(
            final_plot,
            plot_main_legend,
            nrow = 2,
            ncol = 1,
            heights = c(0.8, 0.2)
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            panel.border = ggplot2::element_blank()
          )

        return(final_plot_legend)
    }
