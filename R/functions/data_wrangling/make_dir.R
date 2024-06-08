make_dir <- function(dir_path) {
  suppressWarnings(
    try(
      dir.create(
        dir_path,
        recursive = TRUE
      ),
      silent = TRUE
    )
  )
}