# get scores dbrda
get_scores_dbrda <- 
  function(dbrda_mod){
    
    if(!is.null(dbrda_mod)){
      
      dbrda_score <-
        vegan::scores(dbrda_mod, tidy = TRUE) %>%
        dplyr::as_tibble()
      
      return(dbrda_score)
      
    } else {
      return(NA)
    }
    
  }