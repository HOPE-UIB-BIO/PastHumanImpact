#' @title Thin all strata for one replicate
#' @description
#' Apply greedy spatial thinning independently to every stratum and format the
#' retained identifiers for one distance and repetition.
#' @param data_coordinates Validated spatial records.
#' @param distance_matrix Pairwise distance matrix in kilometres.
#' @param strata_factor Factor defining thinning strata.
#' @param distance_km Minimum retained distance in kilometres.
#' @param repetition Repetition identifier.
#' @param id_col Identifier column name.
#' @param strata Character vector of stratum column names.
#' @return A retained-record ledger for one replicate.
#' @examples
#' \dontrun{
#' thin_spatial_replicate(
#'   data_coordinates = metadata,
#'   distance_matrix = distances,
#'   strata_factor = interaction(metadata[["region"]]),
#'   distance_km = 250,
#'   repetition = 1L,
#'   id_col = "dataset_id",
#'   strata = "region"
#' )
#' }
thin_spatial_replicate <- function(
  data_coordinates,
  distance_matrix,
  strata_factor,
  distance_km,
  repetition,
  id_col,
  strata
) {
  assertthat::assert_that(
    is.data.frame(data_coordinates),
    is.matrix(distance_matrix),
    nrow(distance_matrix) == nrow(data_coordinates),
    length(strata_factor) == nrow(data_coordinates),
    assertthat::is.string(id_col),
    is.character(strata),
    all(c(id_col, strata) %in% names(data_coordinates)),
    msg = "Spatial replicate thinning inputs do not satisfy the contract."
  )

  list_rows <-
    split(
      seq_len(nrow(data_coordinates)),
      strata_factor
    )
  vec_retained <-
    list_rows |>
    purrr::map(
      .f = ~ thin_spatial_stratum(
        row_indices = .x,
        distance_matrix = distance_matrix,
        distance_km = distance_km
      )
    ) |>
    unlist(use.names = FALSE)

  res_replicate <-
    data_coordinates[vec_retained, , drop = FALSE] |>
    dplyr::select(dplyr::all_of(c(id_col, strata))) |>
    dplyr::mutate(
      distance_km = distance_km,
      repetition = as.integer(repetition),
      .before = 1L
    )

  return(res_replicate)
}
