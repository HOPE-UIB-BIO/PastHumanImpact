#' @title Select data and variables from data assembly
#' @description Get a subset of the full data assembly derived from RFossilpol and potentially add percentages
#' @return Get a reduced tibble of selected variables from the full data assembly

select_data <- function(data_assembly, 
                        variables = NULL,
                        add_percentages = TRUE) {
  
  if(add_percentages == TRUE) {
   
    data_assembly %>%
      mutate(percentages_harmonised = 
               purrr::map(counts_harmonised,
                          .f = function(.x) {
                            table <- .x %>% column_to_rownames("sample_id")
                            percent <- table/rowSums(table)*100
                            
                            new <- percent %>% 
                              rownames_to_column("sample_id") %>%
                              as_tibble()
                            new })) %>%
      dplyr::select(all_of(variables))
    
  } else {
    
    data_assembly %>%
      dplyr::select(all_of(variables))
  }
  
}