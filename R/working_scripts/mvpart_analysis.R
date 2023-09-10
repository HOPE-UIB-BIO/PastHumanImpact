###MVPARTwrap package
# tools for mvpart 

# url <- "http://cran.r-project.org/src/contrib/Archive/MVPARTwrap/MVPARTwrap_0.1-9.2.tar.gz"
# install.packages(url)
# 


library(MVPARTwrap)

#Data upload

output_h1_spatial <-  targets::tar_read(output_hvar_spatial,
                                        store = paste0(
                                          data_storage_path,
                                          "_targets_h1"
                                        ))

data_meta <- targets::tar_read(data_meta,
                               store = paste0(
                                 data_storage_path,
                                 "_targets_h1"
                               ))

# split ecozones
data_meta <- data_meta %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) 


# add constrained ecoystem scores with spds; NAs when empty
output_h1_spatial <- 
  output_h1_spatial %>%
  mutate(constrained_scores = purrr::map(data_merge,
                                         .f = possibly(get_scores_constrained_spd,
                                                       otherwise = NA_real_)))
  


# combine with data_meta
data_input <- 
  output_h1_spatial %>%
  inner_join(data_meta %>% 
               dplyr::select(dataset_id, 
                             sel_classification, 
                             region), 
             by = "dataset_id")



# 1.2 data nested by region + ecozones
regional_data  <- data_input %>%
  dplyr::select(dataset_id, 
                region, 
                sel_classification, 
                data_merge) %>%
  unnest(data_merge) %>%
  nest(data_merge = -c(region, sel_classification))



# 
# # mvpart validation analysis
# set.seed(1221)
# region_run1 <- regional_data %>%
#   mutate(mvpart_run1 = purrr::map(data_merge,
#                                   .f = ~run_mvpart(.,
#                                                    preds = "age")))
# 
# set.seed(1222)
# region_run2 <- regional_data %>%
#   mutate(mvpart_run2 = purrr::map(data_merge,
#                                   .f = ~run_mvpart(.,
#                                                    preds = "spd")))
# set.seed(1223)
# region_run3 <- regional_data %>%
#   mutate(mvpart_run3 = purrr::map(data_merge,
#                                   .f = ~run_mvpart(.,
#                                                    preds = "spd + age")))
# set.seed(1224)
# region_run4 <- regional_data %>%
#   mutate(mvpart_run4 = purrr::map(data_merge,
#                                   .f = ~run_mvpart(.,
#                                                    preds = "spd + age + temp_annual + temp_cold + prec_summer + prec_win")))



# mvpart on constrained scores

data_input2 <- data_input %>%
  dplyr::select(dataset_id, 
                region, 
                sel_classification, 
                constrained_scores) 

group_human_impact <- 
  data_input2 %>% 
  dplyr::mutate(scores = 
                  purrr::map(constrained_scores, 
                             pluck("scores"))) %>%
  unnest(scores) %>%
  dplyr::mutate(adjr2 = purrr::map(constrained_scores,
                                       .f = . %>% pluck("adjr2"))) %>%
  unnest(adjr2) %>%
  group_by(sel_classification) %>%
  mutate(human_impact = case_when(
    adjr2 < 0 ~ "no",
    adjr2 > 0 & adjr2 <= 0.1 ~ "low",
    adjr2 > 0.1 & adjr2 <= 0.3 ~ "moderate",
    adjr2 > 0.3 & adjr2 <= 0.5 ~ "high",
    adjr2 > 0.5 ~ "very_high"
  )) 




check_adjr2 <- 
  output_h1_spatial %>% 
  left_join(data_meta, 
            by = "dataset_id") %>%
  dplyr::select(dataset_id, 
                lat, 
                long, 
                region, 
                sel_classification, 
                data_merge, 
                varhp) %>%
  dplyr::mutate(summary_table = 
                  purrr::map(varhp, 
                             pluck("summary_table"))) %>%
  unnest(summary_table) %>%
  dplyr::mutate(total_variance =
                  purrr::map_dbl(varhp, 
                                 .f = . %>% pluck("varhp_output") %>% 
                                   pluck("Total_explained_variation"))) %>%
  dplyr::select(-c(data_merge, varhp)) %>%
  rename(p_value = `Pr(>I)`,
         Individual_percent = `I.perc(%)`) 
 # mutate(across(Unique:Individual_percent, ~replace(., .x < 0, 0))) %>% # negative variances can be ignored 
 # mutate(Individual_percent = Individual/total_variance *100) %>% #recalculate individual percent
  # group_by(region, sel_classification) %>%
  # mutate(n_records = length(unique(dataset_id))) %>%
  # ungroup() %>%
  # mutate(Unique_percent = Unique/total_variance *100,
  #        Average.share_percent = Average.share/total_variance *100)  

#check adjr2 between unique partition and output of the individual output
group_human_impact %>%
  dplyr::select(dataset_id, region,sel_classification, adjr2) %>%
  inner_join(check_adjr2 %>% 
               dplyr::filter(predictor == "human") %>%
               dplyr::select(dataset_id, Unique, Average.share), by = "dataset_id") %>%
  distinct(dataset_id, .keep_all = TRUE) %>%
  mutate(diff_adjr2 = adjr2-Unique) %>%
  filter(region == "Europe") %>% 
  View()
  


# summarise human impact in ecozones and region
summarise_human_impact_ecozone <- 
  group_human_impact %>%
  group_by(sel_classification, region, human_impact) %>%
  mutate(n_impacts = length(unique(dataset_id))) %>%
  ungroup() %>%
  group_by(sel_classification, region) %>%
  mutate(n_records = length(unique(dataset_id))) %>%
  ungroup() %>%
  mutate(proportions_records_impact = n_impacts/n_records)

summarise_human_impact_region<- 
  group_human_impact %>%
  group_by(region, human_impact) %>%
  mutate(n_impacts = length(unique(dataset_id))) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(n_records = length(unique(dataset_id))) %>%
  ungroup() %>%
  mutate(proportions_records_impact = n_impacts/n_records)


# plot the proportion of human impact in each ecozone
select_region <- "Latin America"

fig_prop_human_impact <-
  summarise_human_impact_region %>% 
  dplyr::select(region, 
                human_impact, 
                n_impacts, 
                proportions_records_impact) %>%
  mutate(human_impact = factor(human_impact,
                               levels = c("no",
                                          "low",
                                          "moderate",
                                          "high",
                                          "very_high"))) %>%
  distinct() %>%
 filter(region == select_region) %>%
  ggplot(aes(x = human_impact,
             y = proportions_records_impact,
             fill = human_impact)) +
  geom_bar(stat = "identity",
           position =  position_dodge(),
           width = 1,
           alpha = .5) +
  theme_classic() +
  scale_y_continuous(limits=c(0,1))+
  theme(
    legend.position = "none"
  ) +
 # facet_wrap(~region) +
  labs(title = paste(select_region),
       y = "proportion number of records")


# Save figure
ggsave(
  paste0("prop_records_human_impact", select_region, ".png"),
  fig_prop_human_impact,
  width = 3, height = 4, units = "cm",
  scaling = 0.5,
  bg = "transparent"
)



# records filtered for MRTs 
ecosystem_prop_scores <-  
  summarise_human_impact %>%
   filter(adjr2 > 0.1) %>%
   dplyr::filter(region == select_region) %>%
   dplyr::select(dataset_id:sel_classification, CAP1, age) %>%
  ggplot(aes(x = age, y = CAP1, group = dataset_id)) +
  geom_line(aes(col = as.numeric(dataset_id)),alpha = 0.3, linewidth = 1) +
  scale_colour_gradient2(low = "yellow2", mid = "orange3", high = "darkgreen")+
  theme_bw()+
  theme(
    legend.position = "none"
  ) +
  labs(
       y = "Scores of ecosystem properties\n (constrained by spd)",
       x = "Age ca. BP")

# Save figure
ggsave(
  paste0("ecosystem_prop_scores_age", select_region, ".png"),
  ecosystem_prop_scores,
  width = 8, height = 5, units = "cm",
  scaling = 0.7,
  bg = "transparent"
)


# create data set for human impact regions and ecozones and MRT
dataset_human_impact <-
  summarise_human_impact %>%
  filter(adjr2 > 0.1) %>%
  distinct(dataset_id, age, .keep_all = TRUE) %>%
  dplyr::select(dataset_id:region, CAP1, age) %>% 
  
  
  nest(data = -c(region)) %>%
  mutate(data_for_mvpart = purrr::map(data,
                                      .f = function(x) {
                                        x %>%
                                          pivot_wider(
                                            names_from = dataset_id, 
                                            values_from = CAP1) %>%
                                          return()
                                      } )) %>%
  mutate(mvpart_human_impact = purrr::map(data_for_mvpart,
                                          .f = function(x){
                                            
                                            input <- data.matrix(x %>% dplyr::select(-age))
                                            data <- data.frame(x)
                                            
                                            res <- mvpart::mvpart(input ~ age,
                                                                  xv = "1se", 
                                                                  xval = nrow(data),
                                                                  xvmult = 100,
                                                                  data = data
                                            )
                                            return(res)
                                            
                                          })
  )



