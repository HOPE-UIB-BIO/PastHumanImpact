# function to extract p-value from the dbRDA model

extract_p_value <- function(model) {
  p <- model["Pr(>F)"][1,]
  return(p)
}