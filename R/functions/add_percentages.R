#' @title A function to calculate percentage data
#' @description Add percentage data of harmonised counts to the data assembly
#' @return The data assembly

add_percentages <- function(data) {
  
  data %>%
    mutate(percentages_harmonised = purrr::map(counts_harmonised,
           .f = function(.x) {
             
            table <- .x %>%
               column_to_rownames("sample_id")
            percent <- table/rowSums(table)*100 
            new <- percent %>%
              rownames_to_column("sample_id") %>%
              as_tibble()
            new 
           }))
}
