# working script for validation of results

# use of temporarily dataset saved in workspace

results_hvar_temporal <- read_rds("results_hvar_temporal.rds")
results_hvar_spatial <- read_rds("results_hvar_spatial.rds")

# filter subset Europe
# filter subset Europe and relevant time period
subset_europe <- results_hvar_spatial %>%
  inner_join(tar_read(data_meta) %>% 
               dplyr::select(dataset_id, ecozone_koppen_5, region), 
             by = "dataset_id") %>%
  dplyr::filter(region == "Europe") %>%
  unnest(data_merge) %>%
  filter(age >= 2000 & age <= 8500) %>%
  nest(data_merge = c(age:spd))
  


# 1. Add new analyses (1) mrt model (constrained by age and spd), and (2) pcoa model
## 1.1 for individual records
subset_europe2 <- subset_europe %>%
  dplyr::mutate(mvpart = 
                  purrr::map(data_merge,  
                             .f = ~ get_mvpart_model(., plot.add = FALSE))) %>%
  dplyr::mutate(pcoa_mod = 
                  purrr::map(data_merge,
                             .f = get_pcoa_model))

# 1.2 data nested by ecozones
subset_europe3  <- subset_europe2 %>%
  dplyr::select(dataset_id, ecozone_koppen_5, data_merge) %>%
  unnest(data_merge) %>%
  nest(data_merge = c(dataset_id, age:spd)) %>%
  mutate(mvpart = purrr::map(data_merge, .f = get_mvpart_model)) %>%
  mutate(pcoa_mod = purrr::map(data_merge, .f = get_pcoa_model))

# 1.3 full subdataset
full_dataset_europe <- subset_europe %>% 
  dplyr::select(-c(varhp)) %>%
  unnest(data_merge) %>%
  nest(data = -region) %>%
  mutate(mvpart = purrr::map(data, .f = get_mvpart_model )) %>%
  mutate(pcoa_mod = purrr::map(data, .f = get_pcoa_model))
 
# 2. summarise & visualise the results
## 2.1 results by individual sites

sc <- vegan::scores(subset_europe2$pcoa_mod[[2]], display = "sites", choices = 1)
sc_const <- sc + attr(sc,"const")

get_pcoa_score <- function(mod) {
  sc <- vegan::scores(mod, choices = 1, display = "sites") 
  sc_const <- sc + attr(sc,"const")
  return(sc_const) 
}

subset_europe2  <- subset_europe2 %>%
  mutate(pcoa1 = purrr::map(pcoa_mod, .f = possibly(get_pcoa_score, 
                                                    otherwise = NA_real_)))


# add data with pcoa1 and age
get_data_for_exploration <- function(data, pcoa1) {
  new <- data.frame(age = data$age,
                    spd = data$spd,
                    pcoa1 = pcoa1)
  return(new)
}

subset_europe2  <- subset_europe2 %>%
  mutate(pcoa1 = purrr::map(pcoa_mod, .f = possibly(get_pcoa_score, 
                                                    otherwise = NA_real_))) %>% 
  mutate(data_explore = purrr::map2(data_merge, pcoa1, .f = get_data_for_exploration))



# predictdf.gam <- function(model, xseq, se, level) {
#   olddata <- model.frame(model)
#   if (is.null(olddata$randomid)) {
#     newdata= tibble(x=xseq)
#   } else {
#     newdata = tibble(x=xseq, randomid=olddata$randomid[1])
#   }
#   pred <- predict(model, exclude="s(randomid)", newdata = newdata,
#                   se.fit = se, level = level, interval = if (se)
#                     "confidence"
#                   else "none")
#   if (se) {
#     y = pred$fit
#     ci <- pred$se.fit * 1.96
#     ymin = y - ci
#     ymax = y + ci
#     tibble(x = xseq, y, ymin, ymax, se = pred$se.fit)
#   }
#   else {
#     tibble(x = xseq, y = as.vector(pred))
#   }
#   
# }
# environment(predictdf.gam) <- environment(ggplot2:::predictdf.glm)

get_sum_table_mrt <- function(mod, data){
 table <-  data.frame(
   tot_divisions = mod$where %>% unique %>% length(),
   age_var = any((mod$frame$var == "age") == TRUE),
   spd_var = any((mod$frame$var == "spd") == TRUE),
   spd_in = is.null(data$spd) == FALSE
 )
 return(table)
}




subset_europe2 <- subset_europe2 %>%
  mutate(mrt_summary = purrr::map2(mvpart, data_merge, .f = get_sum_table_mrt))

summary_mrt_table <- subset_europe2 %>%
  dplyr::select(dataset_id, ecozone_koppen_5, mrt_summary) %>%
  unnest(mrt_summary)

# individual records; how many divisions
summary_mrt_table %>% 
 # filter(spd_var == TRUE) %>%
  ggplot(aes(x = tot_divisions, fill = ecozone_koppen_5)) +
  geom_bar() +
  theme_bw()

# summarise number of records with division based on spd and age 
# numbers based on individual records for total European subset
summarise_by_all <- summary_mrt_table %>%
  summarise(tot_records = n(),
            spd_true =  sum(spd_var == TRUE),
            age_true = sum(age_var == TRUE),
            spd_unique = sum(spd_var == TRUE & age_var == FALSE),
            age_unique = sum(age_var == TRUE & spd_var == FALSE),
            spd_age_true = sum(spd_var == TRUE & age_var == TRUE)
  ) %>%
  pivot_longer(spd_true:spd_age_true, names_to = "variable", values_to = "counts") %>%
  mutate(percent = counts/tot_records*100)

# numbers based on individual records by ecozones
summarise_by_ecozone <- summary_mrt_table %>%
  group_by(ecozone_koppen_5) %>%
  summarise(tot_records = n(),
            spd_true =  sum(spd_var == TRUE),
            age_true = sum(age_var == TRUE),
            spd_unique = sum(spd_var == TRUE & age_var == FALSE),
            age_unique = sum(age_var == TRUE & spd_var == FALSE),
            spd_age_true = sum(spd_var == TRUE & age_var == TRUE)
            ) %>%
  pivot_longer(spd_true:spd_age_true, names_to = "variable", values_to = "counts") %>%
  mutate(percent = counts/tot_records*100)





mds_fig <- subset_europe2 %>%
  dplyr::select(dataset_id, ecozone_koppen_5, data_explore) %>%
  unnest(data_explore) %>%
  ggplot(aes(x = age, y = MDS1, group = dataset_id)) +
  scale_x_reverse() +
  coord_flip() +
  geom_line(aes(col = as.numeric(dataset_id)),alpha = 0.3, linewidth = 1) +
  scale_colour_gradient2(low = "yellow2", mid = "orange3", high = "darkgreen")+
  facet_wrap(~ecozone_koppen_5, nrow = 1) +
  theme_bw()+
  theme(
    legend.position = "none"
  ) +
  labs(title = "PCOA axes 1 for individual records",
       y = "Values",
       x = "Age ca. BP")

mds_fig 

spd_fig <- subset_europe2 %>%
  dplyr::select(dataset_id, ecozone_koppen_5, data_explore) %>%
  unnest(data_explore) %>%
  ggplot(aes(x = age, y = spd, group = dataset_id )) +
  geom_line(aes(col = as.numeric(dataset_id))) +
  scale_x_reverse() +
  coord_flip() +
  #scale_colour_gradient2()+
  facet_wrap(~ecozone_koppen_5, nrow = 1) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(title = "SPD curves for individual records",
       y = "Values",
       x = "Age ca. BP")

spd_fig


## 2.2 results nested by ecozones
plot_results <- function(mrt_mod, pcoa_mod, data_merge, ecozone) {
  
  splits <- mrt_mod$where
  
  splits <- data.frame(splits) %>% 
    rownames_to_column("label") 
  
  scors <- vegan::scores(pcoa_mod, tidy = TRUE)
  
  fit_exp <- vegan::envfit(pcoa_mod, data_merge %>% 
                             dplyr::select(age, spd), perm = 999)
  
  fit_exp_scors <- vegan::scores(fit_exp, "vectors") * vegan::ordiArrowMul(fit_exp) 
  
  fit_exp_scors <- fit_exp_scors %>% 
    as.data.frame() %>%
    rownames_to_column("names")
  
  site_scors <- scors %>%
    filter(score == "sites") %>%
    inner_join(splits, by = "label") %>%
    mutate(splits = as.factor(splits))
  
  pcoa_plot <- 
    ggplot(data = site_scors, 
           aes(x = MDS1, 
               y = MDS2, 
               col = splits, 
               fill = splits, 
               shape = splits)) +
    coord_fixed() +
    stat_ellipse(type = "norm", linetype = 2) + 
    stat_ellipse(geom = "polygon", type = "t", alpha= 0.3) +
    geom_point(size = 2) +
    ggplot2::geom_segment(data = fit_exp_scors, 
                          aes(x = 0, xend = MDS1, y = 0, yend = MDS2), 
                          inherit.aes = FALSE, arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
    ggplot2::geom_text(data = fit_exp_scors, 
                       aes(x = MDS1, y = MDS2, label = names), 
                       inherit.aes = FALSE, colour = "navy", fontface = "bold") +
    theme_bw() +
    labs(title = paste("Ecozone", ecozone))
  
  pcoa_plot 
 
}

get_envfit <- function(mod, data, ...) {
  fit_exp <- vegan::envfit(mod, data %>% 
                             dplyr::select(age, spd))
  fit_exp
  
}


subset_europe3 <- subset_europe3 %>%
  mutate(envfit = purrr::map2(pcoa_mod, data_merge, .f = get_envfit)) %>%
  mutate(pcoa_fig = purrr::pmap(list(mvpart, 
                                pcoa_mod,
                                data_merge,
                                ecozone_koppen_5),
                                .f = ~ plot_results(mrt_mod = ..1,
                                                  pcoa_mod = ..2, 
                                                  data_merge = ..3,
                                                  ecozone = ..4)
              ))

full_dataset_europe <- full_dataset_europe %>%
  mutate(envfit = purrr::map2(pcoa_mod, data, .f = get_envfit)) %>%
  mutate(pcoa_fig = purrr::pmap(list(mvpart, 
                                     pcoa_mod,
                                     data,
                                     region),
                                .f = ~ plot_results(mrt_mod = ..1,
                                                    pcoa_mod = ..2, 
                                                    data_merge = ..3,
                                                    ecozone = ..4)
  ))


full_dataset_europe$envfit[[1]]

full_dataset_europe$pcoa_fig[[1]]

# add ordisurf or envfit  
#alternative use ggordiplot

# Experimental analysis

# For each record:
# Do db-RDA with time 
# extract the residuals for each analysis for individual records

# combine the residual results 
# do the MRTs with spd as constraining variable on different scales ecozones and europe with residual data as response



