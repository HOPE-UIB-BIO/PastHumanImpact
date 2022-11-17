#' @title Check the result of filtration process on table
#' @description Function will check the number of rows of the table and return
#' warning message `msg` if there are no sequences
#' @param data_source Data.frame of sequences
#' @param msg Message to be appended to the end of warning
check_data_table <-
  function(data_source, msg = "") {
    check_class("data_source", "data.frame")
    
    stop_if_not(
      nrow(data_source) > 0,
      false_msg = paste(
        "There is 0 sequences based on the selected Criteria.",
        "Please change the criteria.", "\n",
        msg
      ),
      true_msg = paste(
        "Number of sequences prepared:",
        nrow(data_source)
      )
    )
  }