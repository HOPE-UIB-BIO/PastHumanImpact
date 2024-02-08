#check data_spd

# get path to the data assembly
file_spd_path <-
  paste0(
    data_storage_path,
    "Data/spd/"
  )
# - load data assembly from path
data_spd_temp <-
  get_latest_file(file_name = "data_spd",
                       dir = file_spd_path)

# data subset rc dates
data_combine_rc_spd <- 
  data_spd_temp %>%
  dplyr::left_join(data_c14_subset, by = c("dataset_id")) %>%
  mutate(number_rc_250 = map_dbl(rc, ~ .x %>% dplyr::filter(dist < 250) %>% nrow()) ) %>%
  mutate(has_spd_250 = map_lgl(spd, ~any(.x$`250` > 0))) %>%
  mutate(has_min_dates_250 = number_rc_250 > 50) 


data_combine_rc_spd %>%
  dplyr::select(dataset_id, number_rc_250, has_min_dates_250, has_spd_250) 


#check
data_combine_rc_spd %>%
  dplyr::select(dataset_id, number_rc_250, has_min_dates_250, has_spd_250) %>%
  filter(!has_min_dates_250 == has_spd_250) 


# plot
data_combine_rc_spd %>%
  filter(dataset_id == "PVT_Zigetang_Co")%>%
  pluck("spd") %>% 
  pluck(1) %>% 
  pluck("250") %>% 
  plot()

data_combine_rc_spd %>%
  filter(dataset_id == "PVT_Zigetang_Co")%>%
  pluck("rc") %>% 
  pluck(1) %>% 
  filter(dist < 250) 

data_combine_rc_spd %>%
  filter(dataset_id == "1001")%>%
  pluck("spd") %>% 
  pluck(1) %>% 
  pluck("250") %>% 
  plot()

data_combine_rc_spd %>%
  filter(dataset_id == "1001")%>%
  pluck("rc") %>% 
  pluck(1) %>% 
  filter(dist < 250) %>% 
  dplyr::select(Age) %>% 
  plot()

