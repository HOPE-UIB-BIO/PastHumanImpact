#' @title Select levels by the total pollen sum
#' @param data_source Data.frame with pollen data
#' @param min_n_grains A number of pollen grains
#' @return Vector with valid sample_ids
#' @description Calculates pollen sum and test if each level has more pollen
#' sum than `min_n_grains`.
get_sampleid_rowsums <-
  function(data_source,
           min_n_grains) {
    current_frame <- sys.nframe()
    
    current_env <- sys.frame(which = current_frame)
    
    check_class("data_source", "data.frame")
    
    check_col_names("data_source", "sample_id")
    
    check_class("min_n_grains", "numeric")
    
    data_strip <-
      data_source %>%
      dplyr::select(-tidyselect::contains("sample_id"))
    
    data_subset <-
      data_source %>%
      dplyr::mutate(rowsum = rowSums(data_strip)) %>%
      dplyr::filter(rowsum > min_n_grains)
    
    if (
      nrow(data_subset) > 0
    ) {
      data_res <- data_subset
      
      data_res$sample_id %>%
        unique() %>%
        return()
    } else {
      return(NA_character_)
    }
  }