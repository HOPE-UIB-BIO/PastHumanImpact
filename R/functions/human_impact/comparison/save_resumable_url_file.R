#' @title Save a URL to a restartable local file
#' @description
#' Download a large file with command-line curl, retaining a `.part` file when
#' a transfer is interrupted and resuming it on the next call.
#' @param url Character scalar source URL.
#' @param destination Character scalar final file path.
#' @param overwrite Logical. Restart the transfer and replace an existing final
#'   file when `TRUE`.
#' @param curl_command Path to the command-line curl executable.
#' @return Normalized path to the completed file.
#' @examples
#' \dontrun{
#' save_resumable_url_file("https://example.org/data.zip", "data.zip")
#' }
save_resumable_url_file <- function(
  url,
  destination,
  overwrite = FALSE,
  curl_command = unname(Sys.which("curl"))
) {
  assertthat::assert_that(
    assertthat::is.string(url),
    stringr::str_detect(url, "^https://"),
    assertthat::is.string(destination),
    is.logical(overwrite),
    length(overwrite) == 1L,
    assertthat::is.string(curl_command),
    nzchar(curl_command),
    msg = "Restartable download inputs do not satisfy the contract."
  )

  if (
    file.exists(destination) && !isTRUE(overwrite)
  ) {
    return(normalizePath(destination, winslash = "/", mustWork = TRUE))
  }

  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  path_partial <- paste0(destination, ".part")

  if (
    isTRUE(overwrite) && file.exists(path_partial)
  ) {
    unlink(path_partial)
  }

  cli::cli_inform(
    c(
      "Downloading {.url {url}}",
      "i" = "Partial transfer: {.path {path_partial}}"
    )
  )

  download_status <-
    system2(
      command = curl_command,
      args = c(
        "--location",
        "--fail",
        "--retry",
        "5",
        "--retry-all-errors",
        "--continue-at",
        "-",
        "--progress-bar",
        "--output",
        shQuote(path_partial),
        shQuote(url)
      ),
      stdout = "",
      stderr = "",
      wait = TRUE
    )

  if (
    !identical(download_status, 0L) ||
      !file.exists(path_partial) ||
      file.info(path_partial)[["size"]] <= 0
  ) {
    cli::cli_abort(
      c(
        "External proxy download did not complete.",
        "x" = "curl exit status: {download_status}",
        "i" = "Run the acquisition script again to resume the `.part` file."
      )
    )
  }

  if (
    file.exists(destination) && !isTRUE(unlink(destination) == 0L)
  ) {
    cli::cli_abort("Could not replace existing file {.path {destination}}.")
  }

  if (
    !isTRUE(file.rename(path_partial, destination))
  ) {
    cli::cli_abort(
      "Could not finalize downloaded file {.path {destination}}."
    )
  }

  res_path <- normalizePath(destination, winslash = "/", mustWork = TRUE)

  return(res_path)
}
