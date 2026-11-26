#' @title Compute sizes and MD5 checksums of human-proxy source files
#' @description
#' Read every external source file once and return its size and MD5
#' checksum for the provenance table.
#' @param data_sources
#' Data frame with `source_id` and `file_path`; every file must exist.
#' @return
#' Tibble with the columns `source_id`, `file_size_bytes_observed`
#' (numeric), and `file_md5_observed` (character), one row per source in
#' input order.
#' @examples
#' \dontrun{
#' compute_human_proxy_file_checksums(table_sources)
#' }
compute_human_proxy_file_checksums <- function(data_sources) {
  assertthat::assert_that(
    is.data.frame(data_sources),
    all(c("source_id", "file_path") %in% names(data_sources)),
    all(file.exists(data_sources[["file_path"]])),
    msg = "Human-proxy checksum inputs do not satisfy the contract."
  )

  vec_paths <-
    as.character(data_sources[["file_path"]])

  res_checksums <-
    tibble::tibble(
      source_id = as.character(data_sources[["source_id"]]),
      file_size_bytes_observed = as.numeric(file.size(vec_paths)),
      file_md5_observed = unname(tools::md5sum(vec_paths))
    )

  return(res_checksums)
}
