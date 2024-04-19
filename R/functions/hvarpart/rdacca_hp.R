#' @title Multivariate hierarchial variation partitioning 
#' @description This function originate from rdacca.hp package, but with minor change in function to run distance-based RDA so it works for all datasets switching dbrda with capscale from vegan package
#' @param dv response data either as a data.frame or a dist object
#' @param iv data.frame or list of data.frames with predictor variables
#' @param method character; which multivariate analysis to run; RDA, dbRDA, or CCA
#' @param type charachter; type of output; adjusted R2 or R2
#' @param scale logical TRUE or FALSE, if response variables have different units and should be scaled
#' @param add logical TRUE or FALSE; if a constant should be added with method dbRDA
#' @param sqrt.dist logical; if using distance-based RDA, should the distance matrix be square-root transformed?
#' @param n.perm integer; number of permutations
#' @param var.part logical; if TRUE, the function will return the partitioning results
#' @return List of model outputs and partitioning results




rdacca_hp <- function (dv, 
                       iv, 
                       method = c("RDA", "dbRDA", "CCA"), 
                       type = c("adjR2", "R2"), 
                       scale = FALSE, 
                       add = FALSE, 
                       sqrt.dist = FALSE, 
                       n.perm = 1000, 
                       var.part = FALSE) {
  
  if (is.data.frame(iv) || is.matrix(iv)) {
    iv <- as.data.frame(iv)
    if (sum(is.na(dv)) >= 1 || sum(is.na(iv)) >= 1) {
      stop("NA/NaN/Inf is not allowed in this analysis")
    }
    if (nrow(iv) <= ncol(iv)) {
      stop("sample size (row) is less than the number of predictors")
    }
    else {
      method <- method[1]
      type <- type[1]
      if (inherits(dv, "dist")) {
        method <- "dbRDA"
      }
      if (method %in% c("dbRDA", "dbrda", "DBRDA") && !inherits(dv, 
                                                                "dist")) {
        stop("response variables should be a 'dist' matrix for dbRDA")
      }
      if (method %in% c("RDA", "rda")) 
        dv <- scale(dv, scale = scale)
      iv <- as.data.frame(iv)
      ivname <- colnames(iv)
      iv.name <- ivname
      nvar <- dim(iv)[2]
      if (nvar < 2) 
        stop("Analysis not conducted. Insufficient number of predictors.")
      totalN <- 2^nvar - 1
      binarymx <- matrix(0, nvar, totalN)
      for (i in 1:totalN) {
        binarymx <- creatbin(i, binarymx)
      }
      commonM <- matrix(nrow = totalN, ncol = 3)
      for (i in 1:totalN) {
        tmp.design.ct <- iv[as.logical(binarymx[, i])]
        if (method == "RDA" || method == "rda") {
          gfa <- vegan::RsquareAdj(vegan::rda(dv ~ ., 
                                              tmp.design.ct))
        }
        if (method == "CCA" || method == "cca") {
          gfa <- vegan::RsquareAdj(vegan::cca(dv ~ ., 
                                              tmp.design.ct, permutations = n.perm))
        }
        if (method == "dbRDA" || method == "dbrda" || 
            method == "DBRDA") {
          gfa <- vegan::RsquareAdj(vegan::capscale(dv ~ 
                                                     ., tmp.design.ct, add = add, sqrt.dist = sqrt.dist))
        }
        if (type == "R2") 
          commonM[i, 2] <- gfa$r.squared
        if (type == "adjR2") 
          commonM[i, 2] <- gfa$adj.r.squared
      }
      commonlist <- vector("list", totalN)
      seqID <- vector()
      for (i in 1:nvar) {
        seqID[i] = 2^(i - 1)
      }
      for (i in 1:totalN) {
        bit <- binarymx[1, i]
        if (bit == 1) 
          ivname <- c(0, -seqID[1])
        else ivname <- seqID[1]
        for (j in 2:nvar) {
          bit <- binarymx[j, i]
          if (bit == 1) {
            alist <- ivname
            blist <- genList(ivname, -seqID[j])
            ivname <- c(alist, blist)
          }
          else ivname <- genList(ivname, seqID[j])
        }
        ivname <- ivname * -1
        commonlist[[i]] <- ivname
      }
      for (i in 1:totalN) {
        r2list <- unlist(commonlist[i])
        numlist <- length(r2list)
        ccsum <- 0
        for (j in 1:numlist) {
          indexs <- r2list[[j]]
          indexu <- abs(indexs)
          if (indexu != 0) {
            ccvalue <- commonM[indexu, 2]
            if (indexs < 0) 
              ccvalue <- ccvalue * -1
            ccsum <- ccsum + ccvalue
          }
        }
        commonM[i, 3] <- ccsum
      }
      orderList <- vector("list", totalN)
      index <- 0
      for (i in 1:nvar) {
        for (j in 1:totalN) {
          nbits <- sum(binarymx[, j])
          if (nbits == i) {
            index <- index + 1
            commonM[index, 1] <- j
          }
        }
      }
      outputcommonM <- matrix(nrow = totalN + 1, ncol = 2)
      totalRSquare <- sum(commonM[, 3])
      for (i in 1:totalN) {
        outputcommonM[i, 1] <- round(commonM[commonM[i, 
                                                     1], 3], digits = 4)
        outputcommonM[i, 2] <- round((commonM[commonM[i, 
                                                      1], 3]/totalRSquare) * 100, digits = 2)
      }
      outputcommonM[totalN + 1, 1] <- round(totalRSquare, 
                                            digits = 4)
      outputcommonM[totalN + 1, 2] <- round(100, digits = 4)
      rowNames <- NULL
      for (i in 1:totalN) {
        ii <- commonM[i, 1]
        nbits <- sum(binarymx[, ii])
        cbits <- 0
        if (nbits == 1) 
          rowName <- "Unique to "
        else rowName <- "Common to "
        for (j in 1:nvar) {
          if (binarymx[j, ii] == 1) {
            if (nbits == 1) 
              rowName <- paste(rowName, iv.name[j], sep = "")
            else {
              cbits <- cbits + 1
              if (cbits == nbits) {
                rowName <- paste(rowName, "and ", sep = "")
                rowName <- paste(rowName, iv.name[j], 
                                 sep = "")
              }
              else {
                rowName <- paste(rowName, iv.name[j], 
                                 sep = "")
                rowName <- paste(rowName, ", ", sep = "")
              }
            }
          }
        }
        rowNames <- c(rowNames, rowName)
      }
      rowNames <- c(rowNames, "Total")
      rowNames <- format.default(rowNames, justify = "left")
      colNames <- format.default(c("Fractions", " % Total"), 
                                 justify = "right")
      dimnames(outputcommonM) <- list(rowNames, colNames)
      VariableImportance <- matrix(nrow = nvar, ncol = 4)
      for (i in 1:nvar) {
        VariableImportance[i, 3] <- round(sum(binarymx[i, 
        ] * (commonM[, 3]/apply(binarymx, 2, sum))), 
        digits = 4)
      }
      VariableImportance[, 1] <- outputcommonM[1:nvar, 
                                               1]
      VariableImportance[, 2] <- VariableImportance[, 3] - 
        VariableImportance[, 1]
      total = round(sum(VariableImportance[, 3]), digits = 3)
      VariableImportance[, 4] <- round(100 * VariableImportance[, 
                                                                3]/total, 2)
      dimnames(VariableImportance) <- list(iv.name, c("Unique", 
                                                      "Average.share", "Individual", "I.perc(%)"))
      if (var.part) {
        outputList <- list(Method_Type = c(method, type), 
                           Total_explained_variation = total, Var.part = outputcommonM, 
                           Hier.part = VariableImportance)
      }
      else {
        outputList <- list(Method_Type = c(method, type), 
                           Total_explained_variation = total, Hier.part = VariableImportance)
      }
      class(outputList) <- "rdaccahp"
      outputList
    }
  }
  else {
    nvar <- length(iv)
    if (sum(unlist(lapply(iv, is.data.frame))) < nvar) 
      stop("data.frame is required for each group explanatory table")
    if (sum(is.na(dv)) >= 1 | sum(is.na(unlist(iv))) >= 1) {
      stop("NA/NaN/Inf is not allowed in this analysis")
    }
    else {
      method <- method[1]
      type <- type[1]
      if (inherits(dv, "dist")) {
        method <- "dbRDA"
      }
      if (method == "dbRDA" || method == "dbrda" || method == 
          "DBRDA") {
        if (!inherits(dv, "dist")) 
          return("dv should be a 'dist' matrix for dbRDA")
      }
      if (method == "RDA" || method == "rda") {
        dv <- scale(dv, scale = scale)
      }
      ilist <- names(iv)
      if (is.null(ilist)) {
        names(iv) <- paste("X", 1:nvar, sep = "")
      }
      else {
        whichnoname <- which(ilist == "")
        names(iv)[whichnoname] <- paste("X", whichnoname, 
                                        sep = "")
      }
      ilist <- names(iv)
      ivlist <- ilist
      iv.name <- ilist
      if (nvar < 2) 
        stop("Analysis not conducted. Insufficient number of predictor groups.")
      ivID <- matrix(nrow = nvar, ncol = 1)
      for (i in 0:nvar - 1) {
        ivID[i + 1] <- 2^i
      }
      totalN <- 2^nvar - 1
      binarymx <- matrix(0, nvar, totalN)
      for (i in 1:totalN) {
        binarymx <- creatbin(i, binarymx)
      }
      commonM <- matrix(nrow = totalN, ncol = 3)
      for (i in 1:totalN) {
        ivls <- iv[as.logical(binarymx[, i])]
        N <- length(ivls)
        if (N == 1) {
          tmp.design.ct <- ivls[[1]]
          if (method == "RDA" || method == "rda") {
            gfa <- vegan::RsquareAdj(vegan::rda(dv ~ 
                                                  ., tmp.design.ct))
          }
          if (method == "CCA" || method == "cca") {
            gfa <- vegan::RsquareAdj(vegan::cca(dv ~ 
                                                  ., tmp.design.ct, permutations = n.perm))
          }
          if (method == "dbRDA" || method == "dbrda" || 
              method == "DBRDA") {
            gfa <- vegan::RsquareAdj(vegan::capscale(dv ~ 
                                                    ., tmp.design.ct, add = add, sqrt.dist = sqrt.dist))
          }
          if (type == "R2") 
            commonM[i, 2] <- gfa$r.squared
          if (type == "adjR2") 
            commonM[i, 2] <- gfa$adj.r.squared
        }
        if (N > 1) {
          tmp.design.ct <- ivls[[1]]
          for (k in 2:N) {
            tmp.design.ct <- cbind(tmp.design.ct, ivls[[k]])
          }
          if (method == "RDA" || method == "rda") {
            gfa <- vegan::RsquareAdj(vegan::rda(dv ~ 
                                                  ., tmp.design.ct))
          }
          if (method == "CCA" || method == "cca") {
            gfa <- vegan::RsquareAdj(vegan::cca(dv ~ 
                                                  ., tmp.design.ct, permutations = n.perm))
          }
          if (method == "dbRDA" || method == "dbrda" || 
              method == "DBRDA") {
            gfa <- vegan::RsquareAdj(vegan::capscale(dv ~ 
                                                       ., tmp.design.ct, add = add, sqrt.dist = sqrt.dist))
          }
          if (type == "R2") 
            commonM[i, 2] <- gfa$r.squared
          if (type == "adjR2") 
            commonM[i, 2] <- gfa$adj.r.squared
        }
      }
      commonalityList <- vector("list", totalN)
      for (i in 1:totalN) {
        bit <- binarymx[1, i]
        if (bit == 1) 
          ilist <- c(0, -ivID[1])
        else ilist <- ivID[1]
        for (j in 2:nvar) {
          bit <- binarymx[j, i]
          if (bit == 1) {
            alist <- ilist
            blist <- genList(ilist, -ivID[j])
            ilist <- c(alist, blist)
          }
          else ilist <- genList(ilist, ivID[j])
        }
        ilist <- ilist * -1
        commonalityList[[i]] <- ilist
      }
      for (i in 1:totalN) {
        r2list <- unlist(commonalityList[i])
        numlist <- length(r2list)
        ccsum = 0
        for (j in 1:numlist) {
          indexs <- r2list[[j]]
          indexu <- abs(indexs)
          if (indexu != 0) {
            ccvalue <- commonM[indexu, 2]
            if (indexs < 0) 
              ccvalue <- ccvalue * -1
            ccsum <- ccsum + ccvalue
          }
        }
        commonM[i, 3] <- ccsum
      }
      orderList <- vector("list", totalN)
      index <- 0
      for (i in 1:nvar) {
        for (j in 1:totalN) {
          nbits <- sum(binarymx[, j])
          if (nbits == i) {
            index <- index + 1
            commonM[index, 1] <- j
          }
        }
      }
      outputcommonM <- matrix(nrow = totalN + 1, ncol = 2)
      totalRSquare <- sum(commonM[, 3])
      for (i in 1:totalN) {
        outputcommonM[i, 1] <- round(commonM[commonM[i, 
                                                     1], 3], digits = 4)
        outputcommonM[i, 2] <- round((commonM[commonM[i, 
                                                      1], 3]/totalRSquare) * 100, digits = 2)
      }
      outputcommonM[totalN + 1, 1] <- round(totalRSquare, 
                                            digits = 4)
      outputcommonM[totalN + 1, 2] <- round(100, digits = 4)
      rowNames = NULL
      for (i in 1:totalN) {
        ii <- commonM[i, 1]
        nbits <- sum(binarymx[, ii])
        cbits <- 0
        if (nbits == 1) 
          rowName <- "Unique to "
        else rowName = "Common to "
        for (j in 1:nvar) {
          if (binarymx[j, ii] == 1) {
            if (nbits == 1) 
              rowName <- paste(rowName, ivlist[j], sep = "")
            else {
              cbits = cbits + 1
              if (cbits == nbits) {
                rowName <- paste(rowName, "and ", sep = "")
                rowName <- paste(rowName, ivlist[j], 
                                 sep = "")
              }
              else {
                rowName <- paste(rowName, ivlist[j], 
                                 sep = "")
                rowName <- paste(rowName, ", ", sep = "")
              }
            }
          }
        }
        rowNames <- c(rowNames, rowName)
      }
      rowNames <- c(rowNames, "Total")
      rowNames <- format.default(rowNames, justify = "left")
      colNames <- format.default(c("Fractions", " % Total"), 
                                 justify = "right")
      dimnames(outputcommonM) <- list(rowNames, colNames)
      VariableImportance <- matrix(nrow = nvar, ncol = 4)
      for (i in 1:nvar) {
        VariableImportance[i, 3] <- round(sum(binarymx[i, 
        ] * (commonM[, 3]/apply(binarymx, 2, sum))), 
        digits = 4)
      }
      VariableImportance[, 1] <- outputcommonM[1:nvar, 
                                               1]
      VariableImportance[, 2] <- VariableImportance[, 3] - 
        VariableImportance[, 1]
      total = round(sum(VariableImportance[, 3]), digits = 3)
      VariableImportance[, 4] <- round(100 * VariableImportance[, 
                                                                3]/total, 2)
      dimnames(VariableImportance) <- list(iv.name, c("Unique", 
                                                      "Average.share", "Individual", "I.perc(%)"))
      if (var.part) {
        outputList <- list(Method_Type = c(method, type), 
                           Total_explained_variation = total, Var.part = outputcommonM, 
                           Hier.part = VariableImportance)
      }
      else {
        outputList <- list(Method_Type = c(method, type), 
                           Total_explained_variation = total, Hier.part = VariableImportance)
      }
      class(outputList) <- "rdaccahp"
      outputList
    }
  }
}
