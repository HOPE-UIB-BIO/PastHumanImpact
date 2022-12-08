#' @title Get hierarchial variation partitioning results
#' @description Take the output of the hierarchical variation partitioning and create a long table with summary results along with different grouping variables such as region/continent and ecozones
#' @param data tibble output from the run_hvarpart with metadata
#' @param select_vars variables to be selected and summary table
#' @return A long formate table of the summary results that can be used for plotting and visualisation

#create table with output for plotting
create_varhp_table <- function(data, 
                               select_vars = c(dataset_id, region, ecozone, summary_hp)) {
  
  data %>% 
  mutate(summary_hp = purrr::map(varhp, function(.x){
    .x$summary_table
  })) %>%
  dplyr::select(all_of(select_vars)) %>%
  unnest(cols = c(summary_hp)) %>%
  mutate(`Pr(>I)`= str_remove(`Pr(>I)`, "\\ .*"),
         `Pr(>I)`= as.numeric(`Pr(>I)`)) %>%
  group_by(dataset_id) %>%
  mutate(AdjR2_total = sum(Individual)) %>%
  ungroup() %>%
  rename(pval = `Pr(>I)`,
         I.perc = `I.perc(%)`) %>%
  dplyr::select(dataset_id:Vars, Total_eig:Unconstrained_eig, AdjR2_total,Vif, Unique:pval)
  }


