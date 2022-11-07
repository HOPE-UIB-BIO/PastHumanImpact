# function to extract adjusted r squared from the dbRDA model

extract_adjR2 <- function(model) {
  adjR2 <- RsquareAdj(model)$adj.r.squared 
  
  adjR2 <- ifelse(identical(adjR2, numeric(0)) , NA, adjR2)
   
  return(adjR2)
}
