#' @title Validate the external human-proxy source manifest
#' @description
#' Validate source identity, scientific metadata, expected raster contracts,
#' and optionally the presence of external source files.
#' @param data_sources Source manifest data frame.
#' @param data_storage_path Character scalar project data root.
#' @param require_files Logical. If `TRUE`, source files must exist.
#' @param expected_source_ids
#' Character vector. The manifest must contain exactly these source
#' identifiers, once each. The default is the KK10 and HYDE 3.2 manifest of
#' the convergence sensitivity.
#' @param acquisition_hint
#' Character vector of hint lines shown when source files are missing.
#' @return Manifest with a resolved `file_path` column.
#' @details
#' Every `aggregation` value must be `area_weighted_mean` or
#' `cell_center_sum`. When the manifest has a `file_size_bytes` column and
#' `require_files` is `TRUE`, every file must have exactly that size on disk.
#' @examples
#' \dontrun{
#' validate_human_proxy_source_manifest(sources, "Data")
#' }
validate_human_proxy_source_manifest <- function(
  data_sources,
  data_storage_path,
  require_files = TRUE,
  expected_source_ids = c("kk10", "hyde_3_2"),
  acquisition_hint = c(
    stringr::str_c(
      "Run: Rscript R/analyses/91_sensitivity_analyses/",
      "human_proxy_convergence/download_sources.R"
    ),
    stringr::str_c(
      "The downloader resumes interrupted transfers; see ",
      "human_proxy_convergence/README.md for details."
    )
  )
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
    is.character(expected_source_ids),
    length(expected_source_ids) > 0L,
    !anyDuplicated(expected_source_ids),
    nrow(data_sources) == length(expected_source_ids),
    setequal(data_sources[["source_id"]], expected_source_ids),
    !anyDuplicated(data_sources[["source_id"]]),
    assertthat::is.string(data_storage_path),
    is.logical(require_files),
    length(require_files) == 1L,
    is.character(acquisition_hint),
    msg = "Human-proxy source manifest does not satisfy its contract."
  )

  assertthat::assert_that(
    all(stats::complete.cases(data_sources[required_columns])),
    all(data_sources[["expected_layers"]] > 0L),
    all(
      data_sources[["aggregation"]] %in%
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
        "x" = stringr::str_c(vec_missing, collapse = ", "),
        rlang::set_names(
          acquisition_hint,
          rep("i", length(acquisition_hint))
        )
      )
    )
  }

  if (
    isTRUE(require_files) && "file_size_bytes" %in% names(res_sources)
  ) {
    vec_size_mismatch <-
      res_sources |>
      dplyr::filter(
        file.size(.data[["file_path"]]) !=
          as.numeric(.data[["file_size_bytes"]])
      ) |>
      dplyr::pull("file_relative_path")

    if (
      length(vec_size_mismatch) > 0L
    ) {
      cli::cli_abort(
        c(
          "External human-proxy files do not have the recorded size.",
          "x" = stringr::str_c(vec_size_mismatch, collapse = ", ")
        )
      )
    }
  }

  return(res_sources)
}
