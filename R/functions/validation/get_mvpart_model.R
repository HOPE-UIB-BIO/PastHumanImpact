# function to run PCOA model 

get_mvpart_model <- function(data, trans = TRUE, method = "gower"){
 
   if(isTRUE(trans)){
    trans <- vegan::vegdist(data %>% 
                            dplyr::select(n0:roc),
                            method = method) 
    
    res <- mvpart::mvpart(data.matrix(trans) ~ age + spd,
                          xv = "1se", data = data.frame(data))
     } else{
       
       res <- mvpart::mvpart(data.matrix(data %>% dplyr::select(n0:roc)) ~ age + spd,
                             xv = "1se", data = data.frame(data))  
    }
    
  return(res)
 
 
}
