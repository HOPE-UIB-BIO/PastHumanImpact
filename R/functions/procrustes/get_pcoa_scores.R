#' @title Function to get scores from the Principal Coordinate Analysis
#' @param pcoa Input object from Principal Coordinate Analysis
#' @param region Add region column to get regional information information in the output of the table of site scores
#' @param ecozone Add ecozone_koppen division to get ecozone information in the output of the table of site scores

get_pcoa_scores <- function(pcoa, region, ecozone) {
  scores_df <- data.frame(pcoa$points) %>%
    rownames_to_column("label") %>% 
    mutate(region = region) %>%
    mutate(ecozone_koppen_5 = ecozone)
  scores_df
}