#' @title Function to get the pairwise comparisons of procrustes sum of squares
#' @param data_list Input list of the Principal canonical analysis
#' @return Matrix of pairwise Procrustes sum-of-squares with lower-triangular values.

get_procrustes_m2 <- function(data_list) {
  assertthat::assert_that(
    is.list(data_list),
    msg = "`data_list` must be a list."
  )
  assertthat::assert_that(
    length(data_list) > 0,
    msg = "`data_list` must not be empty."
  )
  assertthat::assert_that(
    !is.null(names(data_list)) && all(names(data_list) != ""),
    msg = "`data_list` must have non-empty element names."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_list, ~ !is.atomic(.x))),
    msg = "Each `data_list` entry must be a model-like object."
  )

  name_vec <- names(data_list)

  len <- length(data_list)

  procrustes_m2 <- NULL

  for (i in 1:len) {
    prot2 <- NULL
    for (j in 1:len) {
      prot1 <- ifelse(j > i, NA, vegan::procrustes(
        X = data_list[[i]],
        Y = data_list[[j]],
        scale = TRUE,
        symmetric = TRUE,
        scores = "species"
      )$ss)
      prot2 <- c(prot2, prot1)
    }
    procrustes_m2 <- rbind(procrustes_m2, prot2)
  }

  rownames(procrustes_m2) <- name_vec
  colnames(procrustes_m2) <- name_vec

  return(procrustes_m2)
}
