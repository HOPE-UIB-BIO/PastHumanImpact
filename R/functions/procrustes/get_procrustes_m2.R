#' @title Function to get the pairwise comparisons of procrustes sum of squares
#' @param data_list Input list of the Principal canonical analysis

get_procrustes_m2 <- function(data_list) {
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
