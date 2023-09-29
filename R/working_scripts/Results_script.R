####### IMPORT RESULTS & RESHAPE TABLES FOR PLOTTING


################################################################
# 1. Load output/results from targets
################################################################

# H1

output_h1_temporal <-
  targets::tar_read(
    name = "output_hvar_temporal",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )

output_h1_spatial <-
  targets::tar_read(
    name = "output_hvar_spatial",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )

# H2

output_hvar_h2 <-
  targets::tar_read(
    name = "output_hvar_h2",
    store = paste0(
      data_storage_path,
      "_targets_h2"
    )
  )

# meta data information
data_meta <-
  targets::tar_read(
    name = "data_meta",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )


# Redefine major ecozones
data_meta <-
  data_meta %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  )


# Define variables for selection
group_vars_spatial <- c("predictor", "sel_classification", "region")

group_vars_temporal <- c("predictor", "region", "age")

sel_var <-
  c(
    "total_variance",
    "individual",
    "unique",
    "average_share",
    "individual_percent",
    "unique_percent",
    "average_share_percent"
  )


###############################################################################
# 3. Prepare tables for results H1
###############################################################################

# 3.1 spatial analysis

data_spatial_vis <-
  output_h1_spatial %>%
  dplyr::left_join(
    data_meta,
    by = "dataset_id"
  ) %>%
  dplyr::select(
    dataset_id,
    lat,
    long,
    region,
    sel_classification,
    data_merge,
    varhp
  ) %>%
  dplyr::mutate(
    summary_table = purrr::map(
      .x = varhp,
      .f = ~ purrr::pluck(.x, "summary_table")
    )
  ) %>%
  tidyr::unnest(summary_table) %>%
  dplyr::mutate(
    total_variance = purrr::map_dbl(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("varhp_output", "Total_explained_variation")
    )
  ) %>%
  dplyr::select(-c(data_merge, varhp)) %>%
  dplyr::rename(
    p_value = `Pr(>I)`,
    Individual_percent = `I.perc(%)`
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = Unique:Individual_percent,
      .fns = ~ replace(., .x < 0, 0)
    )
  ) %>% # negative variances can be ignored
  dplyr::mutate(
    Individual_percent = Individual / total_variance * 100
  ) %>% # recalculate individual percent
  group_by(
    region, sel_classification
  ) %>%
  dplyr::mutate(
    n_records = length(unique(dataset_id))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Unique_percent = Unique / total_variance * 100,
    Average.share_percent = Average.share / total_variance * 100
  ) %>%
  janitor::clean_names()

# 3.2 temporal analysis
data_temporal_vis <-
  output_h1_temporal %>%
  dplyr::mutate(
    summary_table = purrr::map(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("summary_table")
    )
  ) %>%
  tidyr::unnest(summary_table) %>%
  dplyr::mutate(
    total_variance = purrr::map_dbl(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("varhp_output") %>%
        purrr::pluck("Total_explained_variation")
    )
  ) %>%
  dplyr::rename(
    Individual_percent = `I.perc(%)`
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = Unique:Individual_percent,
      .fns = ~ replace(., .x < 0, 0)
    )
  ) %>%
  dplyr::mutate(
    Unique_percent = Unique / total_variance * 100,
    Average.share_percent = Average.share / total_variance * 100
  ) %>%
  dplyr::select(-c(data_merge, varhp)) %>%
  dplyr::mutate(
    p_value = readr::parse_number(`Pr(>I)`)
  ) %>%
  dplyr::ungroup() %>%
  janitor::clean_names()


################################################################################
# 2. Filter out models with negative or close to zero adjr2
###############################################################################

# High percentage of individual predictors comes from negative adjusted R2 of the full model;
# 13 datasets
poor_models_spatial <-
  data_spatial_vis %>%
  dplyr::filter(total_variance < 0)

# poor_models_spatial %>% View()


poor_models_spatial_ids <-
  data_spatial_vis %>%
  dplyr::filter(total_variance < 0) %>%
  purrr::pluck("dataset_id") %>%
  unique()

# remove poor models with negative variance
data_spatial_vis <-
  data_spatial_vis %>%
  dplyr::filter(!dataset_id %in% poor_models_spatial_ids)


# check range of variation
# data_spatial_vis$total_variance %>% boxplot()
# data_spatial_vis$total_variance %>% hist()
# data_spatial_vis$total_variance %>% range()

# remove models with lower than 5 % quantile from all
lower_5_percent <-
  data_spatial_vis$total_variance %>%
  quantile(., probs = 0.050, na.rm = TRUE)


# datasets left if threshold of total adjusted r2 is set
dataset_in <-
  data_spatial_vis %>%
  dplyr::filter(total_variance > lower_5_percent) %>%
  purrr::pluck("dataset_id") %>%
  unique() %>%
  length()

datasets_total <-
  data_spatial_vis %>%
  purrr::pluck("dataset_id") %>%
  unique() %>%
  length()

# how many will be removed
dataset_out <- datasets_total - dataset_in
# dataset_out

# check range of p_values
# data_spatial_vis$p_value %>% plot()
# data_spatial_vis$p_value %>% summary(., na.rm = TRUE)

# filter out datasets with adjr2 lower than 5 percent threshold
dataset_spatial_vis <-
  data_spatial_vis %>%
  dplyr::filter(total_variance > lower_5_percent)

# check datasets which variation does not add up
dataset_id_check <-
  data_spatial_vis %>%
  dplyr::filter(
    individual_percent > 100 |
      unique_percent > 100 |
      average_share_percent > 100
  ) %>%
  purrr::pluck("dataset_id") %>%
  unique()

check_datasets <-
  data_spatial_vis %>%
  filter(dataset_id %in% dataset_id_check)


# NB: filter out datasets for now with Individual and Unique percentages above 100 = 41 datasets
# sometimes the total_variance is extremely low whereas Individual are high
# comment; there is no satisfcatory solution to readjust the values, and I do not wish to make mistakes by manipulating the results
data_spatial_vis <-
  data_spatial_vis %>%
  dplyr::filter(!dataset_id %in% dataset_id_check)

## Check temporal variances
# data_temporal_vis$total_variance %>% plot()
# data_temporal_vis$total_variance %>% summary(., na.rm = TRUE)


#############################################################################
# 3. Get summary tables
#############################################################################


# Get adjusted R2 summary tables H1 spatial
r2_summary_spatial <-
  data_spatial_vis %>%
  get_r2_summary(., group_vars = group_vars_spatial, sel_var = sel_var)

# Get median values H1 spatial
summary_spatial_median <-
  r2_summary_spatial %>%
  dplyr::select(
    predictor,
    sel_classification,
    region,
    dplyr::ends_with("median")
  ) %>%
  tidyr::pivot_longer(
    dplyr::ends_with("median"),
    names_to = "variance_partition",
    values_to = "percentage_median"
  ) %>%
  dplyr::left_join(
    data_spatial_vis %>%
      dplyr::select(region, sel_classification, n_records) %>%
      dplyr::distinct(),
    by = c("sel_classification", "region")
  ) %>%
  dplyr::ungroup()

# Get adjusted R2 summary tables H1 temporal
r2_summary_temporal <-
  data_temporal_vis %>%
  get_r2_summary(., group_vars = group_vars_temporal, sel_var = sel_var)

# Get median values H1 spatial
summary_temporal_median <-
  r2_summary_temporal %>%
  dplyr::select(
    predictor,
    age,
    region,
    dplyr::ends_with("median")
  ) %>%
  tidyr::pivot_longer(
    dplyr::ends_with("median"),
    names_to = "variance_partition",
    values_to = "percentage_median"
  ) %>%
  dplyr::left_join(
    data_temporal_vis %>%
      dplyr::select(region, age, n_samples) %>%
      distinct(),
    by = c("age", "region")
  ) %>%
  dplyr::ungroup()


# Get adjusted R2 summary tables H2
data_h2_summary <-
  output_hvar_h2 %>%
  dplyr::mutate(
    summary_table = purrr::map(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("summary_table")
    )
  ) %>%
  tidyr::unnest(summary_table) %>%
  dplyr::mutate(
    total_variance = purrr::map_dbl(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("varhp_output") %>%
        purrr::pluck("Total_explained_variation")
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = Unique:`I.perc(%)`,
      .fns = ~ replace(., .x < 0, 0)
    )
  ) %>% # negative variances can be ignored
  dplyr::mutate(
    Unique_percent = Unique / total_variance * 100,
    Average.share_percent = Average.share / total_variance * 100
  ) %>%
  dplyr::select(-c(data_merge, varhp, responce_dist)) %>%
  dplyr::ungroup() %>%
  janitor::clean_names()

# Reshape H2 table for plotting
data_h2_vis <-
  data_h2_summary %>%
  tidyr::pivot_longer(
    c(unique_percent, average_share_percent),
    names_to = "variance_partition",
    values_to = "percentage"
  )
