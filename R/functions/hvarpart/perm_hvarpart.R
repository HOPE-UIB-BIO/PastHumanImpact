#' @title Permutation of hierarchial variation partitioning
#' @description This function permute the hierarchial variation partitioning, either random or a time series design with mirror reflection
#' @param dv response data frame
#' @param iv either a data frame of predictor variables or list of data frames of predictor variables
#' @param scale logical; standardize to unit variance when RDA method is applied
#' @param add logical; if constant should be added using db-RDA method
#' @param sqrt.dist logical; if square-root of dissimilarity should be added
#' @param nperm integer; number of permutation for adj R squared using CCA method
#' @param permutations integer; number of permutation for randomised datasets
#' @return provide p-values of varpart



perm_hvarpart <- function(dv, 
                         iv, 
                         method = c("RDA", "dbRDA", "CCA"), 
                         type = c("adjR2", "R2"), 
                         scale = FALSE, 
                         add = FALSE, sqrt.dist = FALSE, 
                         n.perm = 1000, 
                         permutations = 1000,
                         series = TRUE) {
  
  
  obs <- rdacca.hp(dv, iv, method = method, type = type, scale = scale, 
                   add = add, sqrt.dist = sqrt.dist, n.perm = n.perm)
  
  r2q <- obs$Hier.part[, 3]
  
  if(series == TRUE & is.data.frame(iv)) {
    
    n <- dim(iv)[1]
    nvar <- dim(iv)[2]    
    
    # permute design for time series
    ctrl <- permute::how(within = Within(type = "series", mirror = TRUE))
    perms <- allPerms(seq_len(n), control = ctrl)
    permutations <- ncol(perms)
    
    cat("\nPlease wait: running", permutations, "permutations \n") 
    
    for (i in 1:permutations) {
      newiv <- iv
      for (j in 1:nvar) {
        newiv[, j] <- iv[perms[i,], j]
      }
      
      simu = rdacca.hp(dv, newiv, method = method, type = type, 
                       scale = scale, add = add, sqrt.dist = sqrt.dist, 
                       n.perm = n.perm)
      
      r2q = cbind(r2q, simu$Hier.part[, 3])
    }
  }
  else if(series == FALSE & is.data.frame(iv)) {
    
    cat("\nPlease wait: running", permutations - 1, "permutations \n")  
    
    for (i in 1:permutations - 1) {
      newiv <- iv
      for (j in 1:nvar) {
        perms <- sample(1:n, n)
        newiv[, j] <- iv[, j][perms]
      }
      
      row.names(newiv) <- 1:n
      simu = rdacca.hp(dv, newiv, method = method, type = type, 
                       scale = scale, add = add, sqrt.dist = sqrt.dist, 
                       n.perm = n.perm)
      r2q = cbind(r2q, simu$Hier.part[, 3])
    }
  }
  else if(series == TRUE & !is.data.frame(iv)){
    
    
    
    n <- dim(iv[[1]])[1]
    nvar <- length(iv)
    
    ctrl <- permute::how(within = Within(type = "series", mirror = TRUE))
    perms <- allPerms(seq_len(n), control = ctrl)
    permutations <- ncol(perms)
    
    cat("\nPlease wait: running", permutations, "permutations \n")
    
    for (i in 1:permutations) {
      newiv <- list()
      for (j in 1:nvar) {
        newiv[[j]] <- iv[perms[i,],]
      }
      
      names(newiv) <- names(iv)
      simu = rdacca.hp(dv, newiv, method = method, type = type, 
                       scale = scale, add = add, sqrt.dist = sqrt.dist, 
                       n.perm = n.perm)
      r2q = cbind(r2q, simu$Hier.part[, 3])
    }
  } else {
    
    cat("\nPlease wait: running", permutations-1, "permutations \n") 
    
    n <- dim(iv[[1]])[1]
    nvar <- length(iv)
    
    for (i in 1:permutations - 1) {
      perms <- sample(1:n, n)
      newiv <- list()
      for (j in 1:nvar) {
        newiv[[j]] <- data.frame(iv[[j]][perms, ])
        row.names(newiv[[j]]) <- 1:n
      }
      names(newiv) <- names(iv)
      simu = rdacca.hp(dv, newiv, method = method, type = type, 
                       scale = scale, add = add, sqrt.dist = sqrt.dist, 
                       n.perm = n.perm)
      r2q = cbind(r2q, simu$Hier.part[, 3])
    }
  }
  
  Signi = function(x) {
    pval = round(1 - ecdf(x)(x[1]) + 1/(permutations + 1), 
                 nchar(permutations))
    if (pval <= 0.001) {
      return(noquote(paste(pval, "***", sep = " ")))
    }
    if (pval <= 0.01) {
      return(noquote(paste(pval, " **", sep = " ")))
    }
    if (pval <= 0.05) {
      return(noquote(paste(pval, "  *", sep = " ")))
    }
    else {
      return(noquote(paste(pval, "   ", sep = " ")))
    }
  }
  
  p.R2 = apply(r2q, 1, Signi)
  result = data.frame(obs$Hier.part[, 3], Pr = p.R2)
  colnames(result) <- c("Individual", "Pr(>I)")
  return(result)
}
