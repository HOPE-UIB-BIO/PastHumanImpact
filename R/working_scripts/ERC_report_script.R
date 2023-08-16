
# load data 

data_filtered <- targets::tar_read(
 name = data_assembly_filtered,
 store = path_to_folder_targets_h1_
)

data_meta <- targets::tar_read(
  name = data_meta,
  store = "h1-folder"
)

