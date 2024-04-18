
#load output h2
output_h2 <- targets::tar_read("output_h2")


##### why does the hvar fails; data looks ok;

dataset_trouble <- output_h2 %>% 
  filter(region == "Europe",
         climatezone == "Cold_Without_dry_season_Warm_Summer")

dataset_trouble2 <- output_h2 %>% 
  filter(region == "Oceania",
         climatezone == "Tropical")

dataset_fine <- output_h2 %>% 
  filter(region == "Europe",
         climatezone == "Temperate_Without_dry_season")



## check original function
# dataset that fails
dataresp1 <- as.dist(dataset_trouble$data_response_dist[[1]])

datapreds1 <- list(
  human = dataset_trouble$data_merge[[1]] %>% 
    dplyr::select(spd),
  climate = dataset_trouble$data_merge[[1]] %>% 
    dplyr::select(temp_annual, 
                  temp_cold, 
                  prec_summer, 
                  prec_win)
  )


# original function
rdacca.hp::rdacca.hp(
    dv = dataresp1,
    iv = datapreds1,
    type = "adjR2",
    var.part = TRUE
  )


#Error in dimnames(u) <- list(dnam[[1]], c(axnam, negnam)) : 
# length of 'dimnames' [2] not equal to array extent 

#dataset that works
dataresp2 <- as.dist(dataset_fine$data_response_dist[[1]])

datapreds2 <- list(
  human = dataset_fine$data_merge[[1]] %>% 
                          dplyr::select(spd),
  climate = dataset_fine$data_merge[[1]] %>% 
                            dplyr::select(temp_annual, 
                                          temp_cold, 
                                          prec_summer, 
                                          prec_win)
)

# original function
rdacca.hp::rdacca.hp(
    dv = dataresp2,
    iv = datapreds2,
    type = "adjR2",
    var.part = TRUE
  )


# dataset that fails; Oceania
dataresp3 <- as.dist(dataset_trouble2$data_response_dist[[1]])

datapreds3 <- list(
  human = dataset_trouble2$data_merge[[1]] %>% 
    dplyr::select(spd),
  climate = dataset_trouble2$data_merge[[1]] %>% 
    dplyr::select(temp_annual, 
                  temp_cold, 
                  prec_summer,
                  prec_win
                  )
)


# original function

  rdacca.hp::rdacca.hp(
  dv = dataresp3,
  iv = datapreds3,
  type = "adjR2",
  scale = TRUE,
  sqrt.dist = FALSE,
  var.part = TRUE,
  add = FALSE
  
)


# what is the difference in datasets that creates this error?

dataresp1
dataresp2
dataresp3

datapreds1
datapreds2
datapreds3

#check db-RDA
# check dbRDA  

# Europe
dbrda(dataresp1 ~ spd + temp_annual + temp_cold + prec_summer + prec_win, data = dataset_trouble$data_merge %>% pluck(1), scale = TRUE) %>% 
  plot()

#Oceania
O_trop <- dbrda(dataresp3 ~  temp_annual + temp_cold + prec_summer + prec_win + spd, data = dataset_trouble2$data_merge %>% pluck(1), scale = TRUE) 

plot(O_trop)

vegan::RsquareAdj(O_trop)


# kind of make sense it is not possible to partition variance 

GGally::ggpairs(dataset_trouble$data_merge[[1]] %>% 
                  dplyr::select(spd, temp_annual, temp_cold, prec_summer, prec_win))

GGally::ggpairs(dataset_trouble2$data_merge[[1]] %>% 
                  dplyr::select(spd, temp_annual, temp_cold, prec_summer, prec_win))


# check unconstrained

unconstrained_test <- dbrda(dataresp1 ~ 1, data = dataset_trouble$data_merge %>% pluck(1), scale = TRUE)

plot(unconstrained_test)
    