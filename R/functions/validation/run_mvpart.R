# function to run MVPART model 

run_mvpart <- function(data, 
                       preds = "age", 
                       trans = TRUE, 
                       method = "gower", 
                       ...) {
  
  if(isTRUE(trans)){
    trans <- vegan::vegdist(data %>% 
                              dplyr::select(n0:density_diversity),
                            method = method) 
    
    input <- data.matrix(trans)
    
    res <- mvpart::mvpart(form = formula(paste("input ~", preds)),
                          xv = "1se", 
                          xval = nrow(data),
                          xvmult = 100,
                          all.leaves = TRUE,
                          data = data.frame(data))
  } else{
    
    input <- data.matrix(data %>% dplyr::select(n0:density_diversity))
    res <- mvpart::mvpart(form = formula(paste("input ~", preds)),
                          xv = "1se", 
                          xval = nrow(data),
                          xvmult = 100,
                          all.leaves = TRUE,
                          data = data.frame(data))  
  }
  
  return(res)
  
}
