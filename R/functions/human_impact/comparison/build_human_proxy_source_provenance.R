#' @title Build the human-proxy source provenance table
#' @description
#' Combine the validated source manifest with observed file checksums and
#' the source time layers used, one row per source.
#' @param data_sources
#' Validated source manifest with at least `source_id`, `product`,
#' `version`, `variable`, `units`, `file_relative_path`, `doi`, and
#' `license`. Optional `file_size_bytes`, `file_md5`, and `notes` columns are
#' kept.
#' @param data_checksums
#' Data frame from `compute_human_proxy_file_checksums()`.
#' @param data_layers
#' Data frame with `source_id` and `age_bp`: the source layers read from
#' each source. Sources without layers (for example a static cell-area grid)
#' may be absent.
#' @return
#' Tibble with one row per source: the manifest columns, the observed size
#' and checksum, `md5_matches` (logical; `NA` when the manifest records no
#' checksum), `n_layers_used`, `source_ages_bp` (ages used, oldest first,
#' separated by `;`), and `source_ages_before_1_ce_bp` (the subset older
#' than 1949 BP, that is before 1 CE). Age columns are `NA` and
#' `n_layers_used` is zero for sources without layers.
#' @examples
#' \dontrun{
#' build_human_proxy_source_provenance(sources, checksums, layers)
#' }
build_human_proxy_source_provenance <- function(
  data_sources,
  data_checksums,
  data_layers
) {
  assertthat::assert_that(
    is.data.frame(data_sources),
    all(
      c(
        "source_id",
        "product",
        "version",
        "variable",
        "units",
        "file_relative_path",
        "doi",
        "license"
      ) %in% names(data_sources)
    ),
    !anyDuplicated(data_sources[["source_id"]]),
    is.data.frame(data_checksums),
    all(
      c("source_id", "file_size_bytes_observed", "file_md5_observed") %in%
        names(data_checksums)
    ),
    setequal(data_checksums[["source_id"]], data_sources[["source_id"]]),
    !anyDuplicated(data_checksums[["source_id"]]),
    is.data.frame(data_layers),
    all(c("source_id", "age_bp") %in% names(data_layers)),
    all(data_layers[["source_id"]] %in% data_sources[["source_id"]]),
    msg = "Human-proxy provenance inputs do not satisfy the contract."
  )

  data_layer_summary <-
    data_layers |>
    dplyr::distinct(.data[["source_id"]], .data[["age_bp"]]) |>
    dplyr::arrange(.data[["source_id"]], dplyr::desc(.data[["age_bp"]])) |>
    dplyr::group_by(.data[["source_id"]]) |>
    dplyr::summarise(
      n_layers_used = dplyr::n(),
      source_ages_bp = stringr::str_c(.data[["age_bp"]], collapse = ";"),
      # 1 CE is 1949 BP; older layers precede the Common Era.
      source_ages_before_1_ce_bp = dplyr::if_else(
        any(.data[["age_bp"]] > 1949),
        stringr::str_c(
          .data[["age_bp"]][.data[["age_bp"]] > 1949],
          collapse = ";"
        ),
        NA_character_
      ),
      .groups = "drop"
    )

  vec_expected_md5 <-
    if (
      "file_md5" %in% names(data_sources)
    ) {
      as.character(data_sources[["file_md5"]])
    } else {
      rep(NA_character_, nrow(data_sources))
    }

  res_provenance <-
    data_sources |>
    dplyr::mutate(
      file_md5_expected = vec_expected_md5
    ) |>
    dplyr::left_join(
      data_checksums,
      by = dplyr::join_by("source_id"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      data_layer_summary,
      by = dplyr::join_by("source_id"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      md5_matches = dplyr::if_else(
        is.na(.data[["file_md5_expected"]]),
        NA,
        .data[["file_md5_expected"]] == .data[["file_md5_observed"]]
      ),
      n_layers_used = dplyr::coalesce(.data[["n_layers_used"]], 0L)
    ) |>
    dplyr::select(-dplyr::all_of("file_md5_expected"))

  return(res_provenance)
}
