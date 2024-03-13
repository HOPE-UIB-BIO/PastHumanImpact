# run dbRDA with data m2 and predictors

run_dbrda <- funtion(data_m2, data_pred){
  
  res <- stats::as.dist(data_m2)
  preds <- data_pred %>% dplyr::select(-age)
  
  # run dbRDA
  dbRDA_res <- vegan::dbrda(
    formula = res ~ .,
    scale = TRUE,
    data = preds
  )
  return(dbRDA_res)
}