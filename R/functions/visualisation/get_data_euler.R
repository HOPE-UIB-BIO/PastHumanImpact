#' @title Restructure data for Euler diagrams
#' @description Restructure data fractions and names to get Euler diagrams
#' @param data_source The table with Hier.part 
#' @return Data structure for input to plot Euler diagrams


get_data_euler <- function(data_source) {
  
  rep_string <- c("Unique to human" = "Human",
                  "Unique to climate" = "Climate",
                  "Unique to time" = "Time",
                  "Common to human, and climate" = "Human&Climate",
                  "Common to human, and time" = "Human&Time",
                  "Common to climate, and time" = "Climate&Time",
                  "Common to human, climate, and time" = "Human&Climate&Time")
  
  
  data_work <-
    as.data.frame(data_source) %>%
    tibble::rownames_to_column("labels") %>%
    tibble::as_tibble() %>%
    mutate(labels = stringr::str_replace_all(labels, rep_string),
           labels = stringr::str_replace_all(labels, " ", "")) %>%
    filter(!labels == "Total") %>%
    
    dplyr::pull(Fractions, labels) %>%
    return()
}