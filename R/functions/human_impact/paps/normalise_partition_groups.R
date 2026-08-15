#' @title Normalise partition groups
#' @description
#' Renumber arbitrary terminal-node identifiers as consecutive integer groups.
#' @param vec_groups Atomic vector of partition identifiers.
#' @return An integer vector with consecutive group identifiers from one.
#' @examples
#' normalise_partition_groups(c(2, 2, 7))
normalise_partition_groups <- function(vec_groups) {
  assertthat::assert_that(
    is.atomic(vec_groups),
    length(vec_groups) > 0L,
    !anyNA(vec_groups),
    msg = "`vec_groups` must be a non-missing atomic vector."
  )

  vec_normalised <-
    as.integer(
      as.factor(vec_groups)
    )

  return(vec_normalised)
}
