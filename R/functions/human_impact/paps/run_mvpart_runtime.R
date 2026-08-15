#' @title Run one isolated mvpart operation
#' @description
#' Serialize validated modern-R input, invoke the R 3.5 runtime through `rig`,
#' and load the validated plain-data result.
#' @param operation One of `mrt` or `change_points`.
#' @param data_input Data frame supplied to the isolated operation.
#' @param path_runner Path to the isolated runtime script.
#' @param path_installer Path to the pinned runtime installer.
#' @param path_function_files Paths to functions sourced by the old-R runner.
#' @param path_library Path to the isolated R 3.5 package library.
#' @param r_version Character scalar R version managed by `rig`.
#' @param rig_executable Path to the `rig` executable.
#' @param process_runner Function used to launch the subprocess.
#' @return A list containing validated `data` and runtime `provenance`.
#' @examples
#' \dontrun{
#' run_mvpart_runtime(
#'   operation = "mrt",
#'   data_input = data_pollen,
#'   path_runner = "run_mvpart_runtime.R",
#'   path_installer = "install_mvpart_runtime.R",
#'   path_function_files = character(),
#'   path_library = "~/R/legacy-library/mvpart-r35"
#' )
#' }
run_mvpart_runtime <- function(
    operation,
    data_input,
    path_runner,
    path_installer,
    path_function_files,
    path_library = path.expand("~/R/legacy-library/mvpart-r35"),
    r_version = "3.5.0",
    rig_executable = Sys.which("rig"),
    process_runner = processx::run
) {
  assertthat::assert_that(
    is.character(operation),
    length(operation) == 1L,
    operation %in% c("mrt", "change_points"),
    is.data.frame(data_input),
    is.character(path_runner),
    length(path_runner) == 1L,
    file.exists(path_runner),
    is.character(path_installer),
    length(path_installer) == 1L,
    file.exists(path_installer),
    is.character(path_function_files),
    length(path_function_files) > 0L,
    all(file.exists(path_function_files)),
    is.character(path_library),
    length(path_library) == 1L,
    dir.exists(path_library),
    is.character(r_version),
    length(r_version) == 1L,
    is.character(rig_executable),
    length(rig_executable) == 1L,
    nzchar(rig_executable),
    is.function(process_runner),
    msg = "Isolated mvpart runtime arguments are invalid."
  )

  path_working_directory <-
    tempfile(pattern = "mvpart-runtime-")

  dir.create(path_working_directory)

  on.exit(
    unlink(
      path_working_directory,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  path_request <-
    file.path(
      path_working_directory,
      "request.rds"
    )

  path_result <-
    file.path(
      path_working_directory,
      "result.rds"
    )

  saveRDS(
    object = list(
      operation = operation,
      data = data_input,
      function_files = normalizePath(
        path_function_files,
        winslash = "/",
        mustWork = TRUE
      )
    ),
    file = path_request,
    version = 2
  )

  res_process <-
    process_runner(
      command = rig_executable,
      args = c(
        "run",
        "--r-version",
        r_version,
        "--script",
        normalizePath(path_runner, winslash = "/", mustWork = TRUE),
        "--",
        normalizePath(path_request, winslash = "/", mustWork = TRUE),
        normalizePath(path_result, winslash = "/", mustWork = FALSE),
        normalizePath(path_library, winslash = "/", mustWork = TRUE)
      ),
      wd = tempdir(),
      echo = FALSE,
      error_on_status = FALSE
    )

  if (
    !identical(res_process[["status"]], 0L)
  ) {
    cli::cli_abort(
      c(
        "The isolated mvpart process failed.",
        "i" = res_process[["stderr"]],
        "i" = res_process[["stdout"]]
      )
    )
  }

  if (
    isFALSE(file.exists(path_result))
  ) {
    cli::cli_abort("The isolated mvpart process did not create a result file.")
  }

  res_runtime <-
    readRDS(path_result)

  res_runtime[["provenance"]][["contract_hashes"]] <-
    as.list(
      tools::md5sum(
        c(
          path_runner,
          path_installer,
          path_function_files
        )
      )
    )

  res_validated <-
    validate_mvpart_runtime_result(
      result = res_runtime,
      operation = operation
    )

  return(res_validated)
}
