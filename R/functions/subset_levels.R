#' @title Subset levels in data.frame by the vector of `sample_ids`
#' @param data_source Data.frame including `sample_id`
#' @param level_vector Vector of `sample_ids`
#' @return Data.frame only including the levels in `level_vector`
#' @export
subset_levels <-
  function(data_source,
           level_vector) {
    check_class("data_source", "data.frame")
    
    check_col_names("data_source", "sample_id")
    
    check_class("data_source", "data.frame")
    
    assertthat::assert_that(
      is.character(data_source$sample_id),
      msg = "'sample_id' in 'data_source' must be 'character'"
    )
    
    check_class("level_vector", c("character", "logical"))
    
    if (
      length(level_vector) > nrow(data_source)
    ) {
      message(
        "WARNING: 'data_source' have smaller number of samples than 'level_vector'"
      )
    }
    
    data_work <-
      data_source %>%
      dplyr::filter(sample_id %in% level_vector)
    
    data_res <-
      data_work[order(match(data_work$sample_id, level_vector)), ]
    
    return(data_res)
  }