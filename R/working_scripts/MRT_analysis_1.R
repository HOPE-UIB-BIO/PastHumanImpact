# MRT analysis 1



# Aim is to get one plot per region that show changes in SPD is highest correlated with changes in N0, N1, N2, and N2-N1. 

#vegan::RsquareAdj()
#vegan::goodness
# create triplots
# Partial RDA triplots (with fitted site scores) 
# Scaling 1 = sites
# Scaline 2 = focus in ecosystem properties

# plot pcoa diagrams 
pcoa_ecozones %>% 
  dplyr::select(site_scores) %>% 
  unnest(cols = c(site_scores)) %>%
  ggplot(aes(x = X1, y = X2, label = label, col = ecozone_koppen_5)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey", size = 1) +
  geom_point() + 
  geom_text(hjust = 0, nudge_x = 0.005) +
  labs(x = "PCoA1", y = "PCoA2") +
  facet_wrap(~region)+
  theme_bw() 




# PRODUCE TABLE 

# EXTENDED ANALYSIS H2

output_hvar_h2

# Run PCA analyses for each time bin in regional ecozones; get procrustes sum of square, extract difference with time
pap_procrustes_ecozones <- data_for_h2 %>% 
  mutate(pca_analysis = purrr::map(data,
                                   .f = run_pca)) %>%
  mutate(pca_analysis = pca_analysis %>% 
           rlang::set_names(nm = data_for_h2$age))  %>% 
  group_by(region, ecozone_koppen_5) %>%
  summarise(pca_analysis = list(pca_analysis)) %>% 
  mutate(m2 = purrr::map(pca_analysis, get_procrustes_m2))%>%
  ungroup() %>%
  mutate(m2_time = purrr::map(m2, .f = get_m2_time))

test <- vegan::procrustes(X = pap_procrustes_ecozones$pca_analysis[[1]][[1]], 
                          Y = pap_procrustes_ecozones$pca_analysis[[1]][[2]], 
                          scale = TRUE, 
                          symmetric = TRUE, 
                          scores = "species")
plot(test, kind = 2)
summary(test)

test <- vegan::protest(X = pap_procrustes_ecozones$pca_analysis[[1]][[2]], 
                       Y = pap_procrustes_ecozones$pca_analysis[[1]][[3]], 
                       scale = TRUE, 
                       symmetric = TRUE, 
                       scores = "species",
                       permutations = 199)
test$signif

plot(residuals(test))


# Add principal coordinate analysis of similarities and differences with time
pcoa_ecozones <- 
  pap_procrustes_ecozones %>%
  filter(!c(region == "North America" & ecozone_koppen_5 == "Tropical")) %>%
  mutate(PCoA = purrr::map(m2, .f = function(x){
    procrust.pcoa <- stats::cmdscale(stats::as.dist(x), eig = TRUE, add = TRUE)
    procrust.pcoa
  })) %>%
  mutate(site_scores = purrr::pmap(list(PCoA,
                                        region,
                                        ecozone_koppen_5),
                                   .f = ~get_scores(pcoa = ..1,
                                                    region = ..2,
                                                    ecozone = ..3))) %>%
  mutate(m2_time_df = purrr::map(m2_time, 
                                 .f = get_m2_time_df))


# plot pcoa diagrams 
pcoa_ecozones %>% 
  dplyr::select(site_scores) %>% 
  unnest(cols = c(site_scores)) %>%
  ggplot(aes(x = X1, y = X2, label = label, col = ecozone_koppen_5)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey", size = 1) +
  geom_point() + 
  geom_text(hjust = 0, nudge_x = 0.005) +
  labs(x = "PCoA1", y = "PCoA2") +
  facet_wrap(~region)+
  theme_bw() 


# PLOT change in m2 in consecutive time steps
pcoa_ecozones %>%
  dplyr::select(m2_time_df, region, ecozone_koppen_5) %>%
  unnest(cols = c(m2_time_df)) %>%
  ggplot(aes(x = as.numeric(time), y = delta_m2, col = ecozone_koppen_5, fill = ecozone_koppen_5 )) +
  geom_point() +
  geom_smooth() +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(~region)







