# run dbRDA with data m2 and predictors

run_dbrda <- function(data_m2, data_pred){
  
  
  if(!is.null(data_m2) & !is.null(data_pred))
    {
  
  # convert m2 distances to euclidean distances
  data_dist <- stats::as.dist(data_m2)
  preds <- data_pred %>% dplyr::select(-age)
  
  # run dbRDA
  dbRDA_res <- vegan::dbrda(
    formula = data_dist ~ .,
    scale = TRUE,
    data = preds
  )
  return(dbRDA_res)
}
else{
  return(NA)
}
}
