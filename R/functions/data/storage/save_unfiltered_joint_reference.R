#' @title Save unfiltered joint-model evidence as a labelled reference
#' @description Copy existing unfiltered joint HVarPart outputs into a named
#' reference directory without removing or modifying their original files.
#' @param source_directories Existing figure and table directories.
#' @param destination_directory Destination `Unfiltered_joint_reference` root.
#' @return Character vector of copied file paths; empty when no source evidence
#'   exists yet.
#' @examples
#' \dontrun{save_unfiltered_joint_reference(c("figures", "tables"), "reference")}
save_unfiltered_joint_reference <- function(
  source_directories,
  destination_directory
) {
  assertthat::assert_that(
    is.character(source_directories), is.character(destination_directory),
    length(destination_directory) == 1L,
    msg = "Unfiltered joint reference paths do not satisfy the contract."
  )
  source_files <- purrr::map(source_directories, .f = ~ {
    if (!dir.exists(.x)) return(character())
    list.files(.x, recursive = TRUE, full.names = TRUE)
  }) |>
    unlist(use.names = FALSE)
  source_files <- source_files[file.exists(source_files)]
  if (length(source_files) == 0L) return(character())
  normalized_roots <- normalizePath(source_directories)
  copied <- purrr::map_chr(source_files, .f = ~ {
    source <- .x
    source_root_index <- which(
      startsWith(normalizePath(source), normalized_roots)
    )[[1]]
    source_root <- source_directories[[source_root_index]]
    category <- basename(source_root)
    relative <- substring(normalizePath(source), nchar(normalizePath(source_root)) + 2L)
    destination <- file.path(destination_directory, category, relative)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(source, destination, overwrite = TRUE, copy.date = TRUE)
    if (!ok) cli::cli_abort("Failed to copy unfiltered reference file {.file {source}}.")
    destination
  })

  return(copied)
}
