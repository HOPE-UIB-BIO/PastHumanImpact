get_scores_constrained_spd <- function(data) {
  
  data <- data.frame(data)
  
  if(is.null(data$spd)) {
    return(NA)
  } else { 
    
    properties <- data %>% dplyr::select(n0:density_diversity)
    
    formula <- properties ~ spd + Condition(age + temp_annual + prec_annual + prec_summer + prec_win)
    
    mod <- vegan::capscale(formula,
                           dist = "gower",
                           add = TRUE,
                           data = data)
    
    adjr2 <- RsquareAdj(mod)$adj.r.squared
    cap1 <- scores(mod, choice = 1, display = "sites")
    constant <- attributes(sc)$const
    
    # dataframe of cap1 = scores of partial dpRDA constrained by spd, age, and spd values
    df <- data.frame(cap1 + constant, age = data$age, spd = data$spd)
    
    output <- list(adjr2 = adjr2, scores = df)
    return(output)}
  
}