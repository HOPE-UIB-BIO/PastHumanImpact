get_data_pollendiagram <- function(levels, pollen) {
  data_table <- levels %>%
    dplyr::select(sample_id, depth, age) %>%
    left_join(pollen, by = "sample_id")  %>%
    pivot_longer(-c(sample_id, depth, age), names_to = "taxa", values_to = "count") %>%
    group_by(sample_id, depth, age) %>%
    mutate(percent = count/sum(count) * 100) %>%
    ungroup() %>%
    mutate(taxa = as.factor(taxa))
  
  return(data_table)
}
