#' @title Subset levels in data.frame by the vector of `sample_ids`
#' @param data_source Matrix including `sample_id` as columns
#' @param level_vector Vector of `sample_ids`
#' @return Matrix only including the columns in `level_vector`
subset_uncertainty_matrix <-
  function(data_source,
           level_vector) {
    current_frame <- sys.nframe()
    
    current_env <- sys.frame(which = current_frame)
    
    check_class("data_source", "matrix")
    
    check_class("level_vector", c("character", "logical"))
    
    assertthat::assert_that(
      length(level_vector) <= ncol(data_source) | all(is.na(level_vector)),
      msg = "'data_source' cannot have smaller number of samples than 'level_vector'"
    )
    
    data_work <-
      data_source[, colnames(data_source) %in% level_vector]
    
    # in case that there is only one valid level, need to back transform to matrix
    if (
      !any(class(data_work) %in% "matrix")
    ) {
      data_work <- matrix(data = data_work, nrow = nrow(data_source))
      colnames(data_work) <- level_vector
    }
    
    data_res <-
      data_work[, order(match(colnames(data_work), level_vector))]
    
    # in case that there is only one valid level, need to back transform to matrix
    if (
      !any(class(data_res) %in% "matrix")
    ) {
      data_res <- matrix(data = data_res, nrow = nrow(data_source))
      colnames(data_res) <- level_vector
    }
    
    return(data_res)
  }