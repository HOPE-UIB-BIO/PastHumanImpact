#' @title Permutation of hierarchial variation partitioning
#' @description This function permute the hierarchial variation partitioning,
#' either random or a time series design with mirror reflection
#' @param dv response data frame
#' @param iv either a data frame of predictor variables or list of data frames
#' of predictor variables
#' @param scale logical; standardize to unit variance when RDA method is applied
#' @param add logical; if constant should be added using db-RDA method
#' @param sqrt.dist logical; if square-root of dissimilarity should be added
#' @param nperm integer; number of permutation for adj R squared using CCA
#' method
#' @param permutations integer; number of permutation for randomised datasets
#' @param verbose Logical; Should progress be output?
#' @return provide p-values of varpart

perm_hvarpart <- function(dv,
                          iv,
                          method = c("RDA", "dbRDA", "CCA"),
                          type = c("adjR2", "R2"),
                          scale = FALSE,
                          add = FALSE,
                          sqrt.dist = FALSE,
                          n.perm = 1000,
                          permutations = 1000,
                          series = TRUE,
                          verbose = FALSE) {
  obs <-
    rdacca.hp::rdacca.hp(
      dv = dv,
      iv = iv,
      method = method,
      type = type,
      scale = scale,
      add = add,
      sqrt.dist = sqrt.dist,
      n.perm = n.perm
    )

  # helper function
  get_r2q <- function(data_source) {
    data_source %>%
      purrr::pluck("Hier.part") %>%
      as.data.frame() %>%
      purrr::pluck("Individual") %>%
      return()
  }

  r2q <-
    get_r2q(obs)

  if (
    isTRUE(series) && is.data.frame(iv)
  ) {
    n <- nrow(iv)
    nvar <- ncol(iv)

    # permute design for time series
    ctrl <-
      permute::how(
        within = permute::Within(type = "series", mirror = TRUE)
      )
    perms <-
      permute::allPerms(seq_len(n), control = ctrl)

    permutations <- ncol(perms)

    cat("\nPlease wait: running", permutations, "permutations \n")

    for (i in 1:permutations) {
      if (
        isTRUE(verbose)
      ) {
        message(i)
      }

      newiv <- iv
      for (j in 1:nvar) {
        newiv[, j] <- iv[perms[i, ], j]
      }

      simu <-
        rdacca.hp::rdacca.hp(
          dv = dv,
          iv = newiv,
          method = method,
          type = type,
          scale = scale,
          add = add,
          sqrt.dist = sqrt.dist,
          n.perm = n.perm
        )

      r2q <- cbind(r2q, get_r2q(simu))
    }
  } else if (
    isFALSE(series) && is.data.frame(iv)
  ) {
    cat("\nPlease wait: running", permutations - 1, "permutations \n")

    for (i in 1:permutations - 1) {
      if (
        isTRUE(verbose)
      ) {
        message(i)
      }

      newiv <- iv
      for (j in 1:nvar) {
        perms <- sample(1:n, n)
        newiv[, j] <- iv[, j][perms]
      }

      row.names(newiv) <- 1:n

      simu <-
        rdacca.hp::rdacca.hp(
          dv = dv,
          iv = newiv,
          method = method,
          type = type,
          scale = scale,
          add = add,
          sqrt.dist = sqrt.dist,
          n.perm = n.perm
        )

      r2q <- cbind(r2q, get_r2q(simu))
    }
  } else if (
    isTRUE(series) && isFALSE(is.data.frame(iv))
  ) {
    n <- nrow(iv[[1]])
    nvar <- length(iv)

    ctrl <-
      permute::how(
        within = permute::Within(type = "series", mirror = TRUE)
      )

    perms <-
      permute::allPerms(seq_len(n), control = ctrl)

    permutations <- ncol(perms)

    cat("\nPlease wait: running", permutations, "permutations \n")

    for (i in 1:permutations) {
      if (
        isTRUE(verbose)
      ) {
        message(i)
      }

      newiv <- list()
      for (j in 1:nvar) {
        newiv[[j]] <- iv[[j]][perms[i, ], ]
      }

      names(newiv) <- names(iv)

      simu <-
        rdacca.hp::rdacca.hp(
          dv = dv,
          iv = newiv,
          method = method, type = type,
          scale = scale, add = add, sqrt.dist = sqrt.dist,
          n.perm = n.perm
        )

      r2q <- cbind(r2q, get_r2q(simu))
    }
  } else {
    cat("\nPlease wait: running", permutations - 1, "permutations \n")

    n <- nrow(iv[[1]])
    nvar <- length(iv)

    for (i in 1:permutations - 1) {
      if (
        isTRUE(verbose)
      ) {
        message(i)
      }

      perms <- sample(1:n, n)
      newiv <- list()
      for (j in 1:nvar) {
        newiv[[j]] <- data.frame(iv[[j]][perms, ])
        row.names(newiv[[j]]) <- 1:n
      }
      names(newiv) <- names(iv)
      simu <-
        rdacca.hp::rdacca.hp(
          dv = dv,
          iv = newiv,
          method = method,
          type = type,
          scale = scale,
          add = add,
          sqrt.dist = sqrt.dist,
          n.perm = n.perm
        )
      r2q <- cbind(r2q, get_r2q(simu))
    }
  }

  fc_signif <- function(x) {
    pval <- round(
      1 - ecdf(x)(x[1]) + 1 / (permutations + 1),
      nchar(permutations)
    )
    if (pval <= 0.001) {
      return(noquote(paste(pval, "***", sep = " ")))
    }
    if (pval <= 0.01) {
      return(noquote(paste(pval, " **", sep = " ")))
    }
    if (pval <= 0.05) {
      return(noquote(paste(pval, "  *", sep = " ")))
    } else {
      return(noquote(paste(pval, "   ", sep = " ")))
    }
  }

  p_r2 <- apply(r2q, 1, fc_signif)
  result <- data.frame(obs$Hier.part[, 3], Pr = p_r2)
  colnames(result) <- c("Individual", "Pr(>I)")
  return(result)
}
