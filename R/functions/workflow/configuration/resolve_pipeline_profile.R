#' @title Resolve the declared profile of a shared target pipeline
#' @description
#' Choose the `script_profile` declared in the pipeline contract registry for
#' the contract row that owns both the given target script and the active
#' target store. A target script shared by several profiles calls this while
#' it is sourced, so the profile follows from the store that the runner
#' passes to `targets::tar_make()` rather than from a global variable.
#' @param data_contracts
#' Pipeline contract registry with columns `script`, `store_relative_path`,
#' and `script_profile`.
#' @param script
#' Character scalar repository-relative path of the target script, as written
#' in the `script` column.
#' @param store
#' Character scalar path of the active target store, usually
#' `targets::tar_path_store()`.
#' @param data_storage_path
#' Character scalar project data root used to resolve registered stores.
#' @param default_profile
#' Non-empty character scalar returned when no contract row owns both the
#' script and the store, or when the matching row declares no profile.
#' @return
#' Character scalar profile identifier.
#' @details
#' Store paths are compared after converting backslashes to forward slashes,
#' collapsing repeated separators, and removing trailing separators. The
#' fallback lets `targets::tar_manifest()` and other calls without a
#' registered store inspect the default profile of the graph.
#' @examples
#' \dontrun{
#' resolve_pipeline_profile(
#'   data_contracts = pipeline_contracts,
#'   script = "R/analyses/example/pipeline.R",
#'   store = targets::tar_path_store(),
#'   data_storage_path = data_storage_path,
#'   default_profile = "joint"
#' )
#' }
resolve_pipeline_profile <- function(
  data_contracts,
  script,
  store,
  data_storage_path,
  default_profile
) {
  assertthat::assert_that(
    is.data.frame(data_contracts),
    all(
      c("script", "store_relative_path", "script_profile") %in%
        names(data_contracts)
    ),
    is.character(script),
    length(script) == 1L,
    !is.na(script),
    is.character(store),
    length(store) == 1L,
    !is.na(store),
    is.character(data_storage_path),
    length(data_storage_path) == 1L,
    !is.na(data_storage_path),
    is.character(default_profile),
    length(default_profile) == 1L,
    !is.na(default_profile),
    nzchar(default_profile),
    msg = "Pipeline profile inputs do not satisfy the contract."
  )

  # Compare stores with one separator style and no trailing separator.
  store_normalised <-
    store |>
    stringr::str_replace_all(
      pattern = "[\\\\/]+",
      replacement = "/"
    ) |>
    stringr::str_remove(
      pattern = "/$"
    )

  data_matching <-
    data_contracts |>
    dplyr::filter(.data[["script"]] == .env[["script"]]) |>
    dplyr::mutate(
      store_resolved = purrr::map_chr(
        .x = .data[["store_relative_path"]],
        .f = ~ resolve_pipeline_store_path(
          data_storage_path = data_storage_path,
          store_relative_path = .x
        )
      ) |>
        stringr::str_replace_all(
          pattern = "[\\\\/]+",
          replacement = "/"
        ) |>
        stringr::str_remove(
          pattern = "/$"
        )
    ) |>
    dplyr::filter(.data[["store_resolved"]] == store_normalised)

  vec_profile <-
    data_matching |>
    dplyr::pull("script_profile") |>
    as.character()

  res_profile <-
    if (
      length(vec_profile) == 1L &&
        !is.na(vec_profile) &&
        nzchar(vec_profile)
    ) {
      vec_profile
    } else {
      default_profile
    }

  return(res_profile)
}
