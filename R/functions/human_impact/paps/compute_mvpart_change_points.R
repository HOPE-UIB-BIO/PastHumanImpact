#' @title Extract mvpart change points
#' @description
#' Extract split indices from an `mvpart` or legacy `rpart` summary without
#' retaining printed summary output.
#' @param model Fitted tree model.
#' @param summary_backend Function used to summarise the model.
#' @return A numeric vector of split indices, possibly empty.
#' @examples
#' \dontrun{
#' compute_mvpart_change_points(model = fitted_tree)
#' }
compute_mvpart_change_points <- function(
    model,
    summary_backend = summary
) {
  assertthat::assert_that(
    is.function(summary_backend),
    msg = "`summary_backend` must be a function."
  )

  invisible(
    utils::capture.output(
      res_summary <- summary_backend(model)
    )
  )

  data_splits <-
    res_summary[["splits"]]

  if (
    is.null(data_splits) || NROW(data_splits) == 0L
  ) {
    res <-
      numeric(0)

    return(res)
  }

  data_splits <-
    as.data.frame(data_splits)

  assertthat::assert_that(
    "index" %in% names(data_splits),
    msg = "The fitted tree summary does not contain split indices."
  )

  vec_change_points <-
    as.numeric(data_splits[["index"]])

  return(vec_change_points)
}
