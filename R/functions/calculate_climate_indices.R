data_merge <-
  dplyr::inner_join(
    data_prec_sum,
    data_prec_win,
    by = "dataset_id") %>% 
  dplyr::inner_join(
    data_tasmin,
    by = "dataset_id") %>% 
  dplyr::inner_join(
    data_temp,
    by = "dataset_id")


#----------------------------------------------------------#
# 3. Calculate indices -----
#----------------------------------------------------------#

# GDM = number of growing degree months per year
data_gdm <-
  data_merge %>% 
  dplyr::mutate(
    gdm = purrr::map(
      .x = tasmin_data,
      .f = ~ {
        .x %>% 
          dplyr::mutate(
            temp_c = value/10 - 273.15, # transform to Celsius
            above_0 = temp_c > 0, # test if temperature higher than 0 C degrees
            time_id_fix = stringr::str_replace(time_id, "_[:digit:].*", "") %>% 
              as.numeric()) %>% 
          dplyr::group_by(time_id_fix) %>% 
          dplyr::summarise(
            # count number of months which above_0 == TRUE
            value = sum(above_0)) %>% 
          transform.ages(., temp_ref) %>% 
          dplyr::mutate(
            time_id = as.character(time_id_fix)) %>% 
          dplyr::select(
            value, 
            time_id,
            age)
      }))

#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

data_climate <-
  data_gdm %>% 
  dplyr::mutate(
    clim_data = purrr::pmap(
      .l = list(temp_data, prec_sum_data, prec_win_data, gdm),
      .f = ~ dplyr::inner_join(
        ..1 %>% 
          dplyr::rename(temp_cold = value),
        ..2 %>% 
          dplyr::rename(prec_summer = value),
        by = c("time_id", "age")) %>% 
        dplyr::inner_join(
          ..3 %>% 
            dplyr::rename(prec_winter = value),
          by = c("time_id", "age")) %>% 
        dplyr::inner_join(
          ..4 %>% 
            dplyr::rename(gdm = value),
          by = c("time_id", "age")) %>% 
        dplyr::select(
          age, time_id, temp_cold, prec_summer, prec_winter, gdm))) %>% 
  dplyr::select(dataset_id, clim_data)
