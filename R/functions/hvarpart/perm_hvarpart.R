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
#' @param rdacca_fun Function used to fit hierarchical variation partitioning.
#'   Defaults to `rdacca.hp::rdacca.hp`.
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
                          verbose = FALSE,
                          rdacca_fun = rdacca.hp::rdacca.hp) {
  method <- match.arg(method)
  type <- match.arg(type)

  assertthat::assert_that(
    is.data.frame(dv) || is.matrix(dv) || inherits(dv, "dist"),
    msg = "`dv` must be a data.frame, matrix, or dist object."
  )
  assertthat::assert_that(
    is.data.frame(iv) || is.list(iv),
    msg = "`iv` must be a data.frame or list of data.frames."
  )
  assertthat::assert_that(
    is.numeric(n.perm),
    length(n.perm) == 1,
    n.perm > 0,
    msg = "`n.perm` must be a positive number."
  )
  assertthat::assert_that(
    is.numeric(permutations),
    length(permutations) == 1,
    permutations > 0,
    msg = "`permutations` must be a positive number."
  )
  assertthat::assert_that(
    is.logical(series),
    length(series) == 1,
    !is.na(series),
    msg = "`series` must be TRUE or FALSE."
  )
  assertthat::assert_that(
    is.logical(verbose),
    length(verbose) == 1,
    !is.na(verbose),
    msg = "`verbose` must be TRUE or FALSE."
  )
  assertthat::assert_that(
    is.function(rdacca_fun),
    msg = "`rdacca_fun` must be a function."
  )

  # helper function
  get_r2q <- function(data_source) {
    data_source %>%
      purrr::pluck("Hier.part") %>%
      as.data.frame() %>%
      purrr::pluck("Individual") %>%
      return()
  }

  # observed values
  obs <-
    rdacca_fun(
      dv = dv,
      iv = iv,
      method = method,
      type = type,
      scale = scale,
      add = add,
      sqrt.dist = sqrt.dist,
      n.perm = n.perm
    )

  # observed r2
  r2q <-
    get_r2q(obs)

  if (
    isTRUE(series)
  ) {
    if (
      is.data.frame(iv)
    ) {
      n <- nrow(iv)
      nvar <- ncol(iv)
    } else {
      n <- nrow(iv[[1]])
      nvar <- length(iv)
    }

    # permute design for time series
    ctrl <-
      permute::how(
        within = permute::Within(type = "series", mirror = TRUE)
      )
    perms <-
      permute::allPerms(seq_len(n), control = ctrl)

    permutations <- ncol(perms)
  } else {
    if (
      is.data.frame(iv)
    ) {
      n <- nrow(iv)
      nvar <- ncol(iv)
    } else {
      n <- nrow(iv[[1]])
      nvar <- length(iv)
    }

    permutations <- permutations - 1
  }

  if (isTRUE(verbose)) {
    message(paste("Running", permutations, "permutations"))
  }

  for (i in 1:permutations) {
    if (
      isTRUE(verbose)
    ) {
      message(
        paste(
          " -", i, "out of", permutations
        )
      )
    }

    if (
      isTRUE(series) && is.data.frame(iv)
    ) {
      newiv <- iv
      for (j in 1:nvar) {
        newiv[, j] <- iv[perms[i, ], j]
      }
    } else if (
      isFALSE(series) && is.data.frame(iv)
    ) {
      newiv <- iv
      for (j in 1:nvar) {
        perms <- sample(1:n, n)
        newiv[, j] <- iv[, j][perms]
      }

      row.names(newiv) <- 1:n
    } else if (
      isTRUE(series) && isFALSE(is.data.frame(iv))
    ) {
      newiv <- list()
      for (j in 1:nvar) {
        newiv[[j]] <- iv[[j]][perms[i, ], ]
      }

      names(newiv) <- names(iv)
    } else {
      perms <- sample(1:n, n)
      newiv <- list()
      for (j in 1:nvar) {
        newiv[[j]] <- data.frame(iv[[j]][perms, ])
        row.names(newiv[[j]]) <- 1:n
      }
      names(newiv) <- names(iv)
    }

    simu <-
      rdacca_fun(
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

  fc_signif <- function(x) {
    pval <- round(
      1 - stats::ecdf(x)(x[1]) + 1 / (permutations + 1),
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

  result <-
    data.frame(get_r2q(obs), Pr = p_r2) %>%
    rlang::set_names(
      nm = c("Individual", "Pr(>I)")
    )

  return(result)
}
