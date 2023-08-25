#----------------------------#
# Example data use ----
#----------------------------#
library(here)
source("R/00_Config_file.R")

# load data for Europe
data_filtered <- 
  targets::tar_read(
    name = data_assembly_filtered,
    store = external_storage_targets
    ) %>% 
  dplyr::filter(region == "Europe")


# estimate total compositional turnover
data_dcca <-
  data_filtered %>%
  dplyr::mutate(
    percentages =
      purrr::map(
        .x = counts_harmonised,
        .f = function(.x) {
          counts <-
            .x %>%
            column_to_rownames("sample_id")
          percentages <-
            ((counts / rowSums(counts)) * 100) %>%
            round(., digits = 3) %>%
            mutate_all(~ replace(., is.nan(.),
                                 0)) %>%
            rownames_to_column("sample_id") %>%
            dplyr::select(sample_id,
                          everything())
          return(percentages)
        }
      ),
    dcca = purrr::map2(
      .x = percentages,
      .y = levels,
      .f = ~ REcopol::fit_ordination(
        data_source_community = .x,
        data_source_predictors = .y,
        sel_method = "constrained",
        var_name_pred = "age",
        sel_complexity = "poly_2",
        transform_to_percentage = FALSE,
        tranformation = "none"
      )
    ),
    dcca_grad_length = purrr::map_dbl(
      .x = dcca,
      .f = ~ .x %>%
        purrr::pluck("axis_1_grad_length")
      )
    ) %>%
  dplyr::select(dataset_id,
                long,
                lat,
                ecozone_koppen_5,
                dcca_grad_length)

# Base map with Köppen-Geiger 5 climate zones (in Beck et al. 2018)

base_map <-
  data_dcca %>%
  ggplot2::ggplot(ggplot2::aes(x = long,
                               y = lat)) +
  ggplot2::coord_fixed(ylim = c(32.00, 75.00),
                       xlim = c(2, 35.00)) +
  
  ggplot2::labs(x = "Longitude",
                y = "Latitude",
                colour = "Climate zones") +
  ggplot2::theme_classic() +
  ggplot2::borders(colour = "black",
                   size = 0.2) +
  theme(legend.position = "none")


plot_dcca <-
  plot_spatial_dist(
    data_source = data_dcca,
    base_map = base_map,
    var_name = "dcca_grad_length",
    lab_name = "DCCA gradient length",
    error_family = "mgcv::Tweedie(p = 1.1, link = 'log')"
    )

ggsave(
  plot_dcca,
  file = paste(
    "Data_summary_outputs/Figure/",
    "Europe_data_filtered_gradient_length_250823.tiff",
    sep = ""
    ),
  dpi = 400,
  compress = "lzw",
  width = 15,
  height = 15,
  unit = "cm"
  )
