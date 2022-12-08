#' @title Hierarchial variation partitioning
#' @description Function to select response and predictor variables and run hierarchical variation partitioning and permutation 
#' @param data full dataset with response and predictor variables
#' @param run_matrix logical; if predictor variables should be assessed individually or as list of data frames
#' @param permutations integers; numbers of permutations
#' @param resp_vars vector of names of response variables
#' @param pred_vars vector of names of predictor variables
#' @param ... see parameters of functions within
#' @return List of model outputs and a summary table of the results

get_varhp <- function(data,
                      run_matrix = FALSE, 
                      permutations = 99, 
                      resp_vars = NULL, 
                      pred_vars = NULL, 
                      ...) {
  
  resp <- data %>% 
    dplyr::select(all_of(resp_vars))
  
  preds <- data %>% 
    dplyr::select(all_of(pred_vars))  %>%
    dplyr::select(where(~ any(. != 0))) # note have to remove empty vars for individual sites or the model will fail, need to take this into account later (for discussion - empty vars = different things)
  
  # run model to get variation inflation factors of the predictors, and total unexplained and explained variation 
  mod <- vegan::capscale(resp ~ as.matrix(preds), 
                         dist = "gower",
                         add = TRUE,
                         data = resp)
  
  
  vif.preds <- vegan::vif.cca(mod)
  
  if(run_matrix == TRUE) {
    # hierarchial variation partitioning to get the variations explained by all individual terms
    varhp <- rdacca.hp::rdacca.hp(dv = vegdist(resp, method = "gower"),
                                  iv = preds, 
                                  add = TRUE, 
                                  method = "dbRDA", 
                                  type = "adjR2", 
                                  var.part = TRUE, 
                                  ...)
    
    # significant test of the variables and explained variations
    hp.test <- perm_varpart(dv = vegdist(resp, method = "gower"),
                            iv = as.data.frame(preds), 
                            method ="dbRDA", 
                            add = TRUE, 
                            permutations = permutations, 
                            series = TRUE,
                                   ...)
    
    
    
  } else {
    
    pred.list <- list(
      human = data.frame(spd = preds$spd),
      climate = data.frame(preds %>% 
                             dplyr::select(temp_cold, 
                                           prec_summer, 
                                           prec_winter, 
                                           gdm)),
      time = data.frame(age = preds$age)
    )
    
    
    varhp <-  rdacca.hp::rdacca.hp(dv = vegdist(resp, method = "gower"),
                                   iv = pred.list, 
                                   add = TRUE, 
                                   method = "dbRDA", 
                                   type = "adjR2", 
                                   var.part = TRUE, 
                                   ...)
    
    hp.test <- permu_varpart(dv = vegdist(resp, method = "gower"),
                             iv = pred.list, 
                             method ="dbRDA", 
                             add = TRUE,
                             type = "adjR2",
                             permutations = permutations, 
                             series = TRUE,
                                   ...)
  }
  
  
  # extract relevant summary output
  output_table <- as.data.frame(varhp$Hier.part) %>%
    rownames_to_column("Vars") %>%
    left_join(hp.test, by = "Individual")
  
  output_table$Vif <- vif.preds 
  output_table$Total_eig <- mod$tot.chi
  output_table$Constrained_eig <- sum(mod$CCA$eig)
  output_table$Unconstrained_eig = mod$tot.chi - sum(mod$CCA$eig)
  
  summary_table <- output_table %>%
    dplyr::select(Vars, Vif, Total_eig, Constrained_eig, Unconstrained_eig, everything())
  
  results <- list(mod = mod, varhp_output = varhp, summary_table = summary_table)
  
  return(results)
  
}
