# Load packages:
library(targets)
# library(tarchetypes) 

# Set target options:
tar_option_set(
  packages = c( "assertthat",
                "devtools",
                "REcopol",
                "RFossilpol",
                "ggpubr",
                "mgcv",
                "here",      
                "renv",       
                "roxygen2",
                "readr",
                "tidyverse",  
                "usethis",
                "vegan",
                "GGally",
                "gittargets"
               ),
  format = "rds", # default storage format
 
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# source R functions:
source("R/functions.R") 
#source("R/util.functions.R") # assembly all utility functions

# the targets list:
list(
  tar_target(data_assembly, "Data/assembly/data_assembly-2022-05-23.rds", format = "file"),
  tar_target(data_pollen, get_data_pollen(data_assembly)),
  tar_target(data_sites, get_data_site(data_assembly)),
  tar_target(data_filtered, filter_age_levels(data_pollen)),
  tar_target(data_diversity, get_diversity(data_filtered)),
  tar_target(data_mrt, get_mrt(data_filtered))
  #tar_target(data_dcca, get_dcca(data_filtered)),
  #tar_target(data_roc, get_roc(data_filtered))
  
)

# COMING BUT NEEDS MODIFICATION CODINGWISE:

  # tar_target(data_combined_paps, combine_paps(data_sites, data_diversity, data_mrt, data_roc, data_dcca))
  # tar_target(data_change_points_pap, get_change_points_pap(data_combined_pap))
  # tar_target(data_density, get_density_pap(data_change_points_pap))

 # make a separate run gam function on  response data first or at the end when all variables are in or incorporate in get_data_h1

  #tar_target(data_climate, get_climate())
  #tar_target(data_spd, get_spd())
  #tar_target(data_h1, get_data_h1(data_combined_pap, data_density, data_meta, data_events, data_climate, data_spd))
 
  #tar_target(model_h1, run_model_h1(data_h1))
  

