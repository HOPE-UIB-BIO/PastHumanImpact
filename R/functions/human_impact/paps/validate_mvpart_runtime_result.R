#' @title Validate an isolated mvpart result
#' @description
#' Enforce the data and provenance contract returned by the old-R subprocess.
#' @param result Result list returned by the isolated runtime.
#' @param operation One of `mrt` or `change_points`.
#' @param seed Whole number that the isolated runtime was asked to use.
#' @return The validated result, unchanged.
#' @details
#' The provenance must record the requested seed, which shows that the runner
#' passed it to the tree-fitting functions.
#' @examples
#' \dontrun{
#' validate_mvpart_runtime_result(result, operation = "mrt", seed = 1234L)
#' }
validate_mvpart_runtime_result <- function(result, operation, seed) {
  assertthat::assert_that(
    is.list(result),
    all(c("data", "provenance") %in% names(result)),
    is.character(operation),
    length(operation) == 1L,
    operation %in% c("mrt", "change_points"),
    is.numeric(seed),
    length(seed) == 1L,
    is.finite(seed),
    seed == round(seed),
    msg = "The isolated mvpart result contract is invalid."
  )

  data_result <-
    result[["data"]]

  vec_required_columns <-
    if (
      identical(operation, "mrt")
    ) {
      c(
        "dataset_id",
        "mvrt_partitions",
        "mvrt_cp",
        "mvrt_groups_n"
      )
    } else {
      c(
        "dataset_id",
        "mvrt_cp",
        "diversity_cp",
        "roc_cp",
        "roc_pp",
        "dcca_cp"
      )
    }

  assertthat::assert_that(
    is.data.frame(data_result),
    all(vec_required_columns %in% names(data_result)),
    identical(result[["provenance"]][["operation"]], operation),
    msg = "The isolated mvpart output has an invalid operation schema."
  )

  assertthat::assert_that(
    is.numeric(result[["provenance"]][["seed"]]),
    identical(
      as.integer(result[["provenance"]][["seed"]]),
      as.integer(seed)
    ),
    msg = "The isolated mvpart result did not use the requested seed."
  )

  return(result)
}
