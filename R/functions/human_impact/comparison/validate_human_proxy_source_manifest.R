#' @title Validate the external human-proxy source manifest
#' @description
#' Validate source identity, scientific metadata, expected raster contracts,
#' and optionally the presence of external source files.
#' @param data_sources Source manifest data frame.
#' @param data_storage_path Character scalar project data root.
#' @param require_files Logical. If `TRUE`, source files must exist.
#' @return Manifest with a resolved `file_path` column.
#' @examples
#' \dontrun{
#' validate_human_proxy_source_manifest(sources, "Data")
#' }
validate_human_proxy_source_manifest <- function(
  data_sources,
  data_storage_path,
  require_files = TRUE
) {
  required_columns <-
    c(
      "source_id",
      "product",
      "version",
      "variable",
      "units",
      "file_relative_path",
      "source_url",
      "download_url",
      "doi",
      "license",
      "expected_layers",
      "aggregation"
    )

  assertthat::assert_that(
    is.data.frame(data_sources),
    all(required_columns %in% names(data_sources)),
    nrow(data_sources) == 2L,
    setequal(data_sources[["source_id"]], c("kk10", "hyde_3_2")),
    !anyDuplicated(data_sources[["source_id"]]),
    assertthat::is.string(data_storage_path),
    is.logical(require_files),
    length(require_files) == 1L,
    msg = "Human-proxy source manifest does not satisfy its contract."
  )

  assertthat::assert_that(
    all(stats::complete.cases(data_sources[required_columns])),
    all(data_sources[["expected_layers"]] > 0L),
    setequal(
      data_sources[["aggregation"]],
      c("area_weighted_mean", "cell_center_sum")
    ),
    msg = "Human-proxy source metadata are incomplete or unsupported."
  )

  res_sources <-
    data_sources |>
    dplyr::mutate(
      file_path = file.path(
        data_storage_path,
        .data[["file_relative_path"]]
      )
    )

  if (
    isTRUE(require_files) && any(!file.exists(res_sources[["file_path"]]))
  ) {
    vec_missing <-
      res_sources |>
      dplyr::filter(!file.exists(.data[["file_path"]])) |>
      dplyr::pull("file_relative_path")

    cli::cli_abort(
      c(
        "External human-proxy source files are missing.",
        "x" = paste(vec_missing, collapse = ", "),
        "i" = paste(
          "Run: Rscript",
          paste0(
            "R/analyses/91_sensitivity_analyses/",
            "human_proxy_convergence/download_sources.R"
          )
        ),
        "i" = paste(
          "The downloader resumes interrupted transfers; see",
          "human_proxy_convergence/README.md for details."
        )
      )
    )
  }

  return(res_sources)
}
