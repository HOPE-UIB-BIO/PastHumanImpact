#' @title Greedily thin one spatial stratum
#' @description
#' Visit records in random order and retain a record only when it is at least
#' the requested distance from all previously retained records.
#' @param row_indices Integer row indices for one stratum.
#' @param distance_matrix Pairwise distance matrix in kilometres.
#' @param distance_km Minimum retained distance in kilometres.
#' @return Integer indices of retained records.
#' @examples
#' \dontrun{
#' select_spatial_thinning_stratum(
#'   row_indices = 1:3,
#'   distance_matrix = distances,
#'   distance_km = 250
#' )
#' }
select_spatial_thinning_stratum <- function(
  row_indices,
  distance_matrix,
  distance_km
) {
  assertthat::assert_that(
    is.numeric(row_indices),
    length(row_indices) > 0L,
    all(is.finite(row_indices)),
    is.matrix(distance_matrix),
    all(row_indices %in% seq_len(nrow(distance_matrix))),
    is.numeric(distance_km),
    length(distance_km) == 1L,
    is.finite(distance_km),
    distance_km > 0,
    msg = "Spatial stratum thinning inputs do not satisfy the contract."
  )

  vec_order <-
    if (
      length(row_indices) == 1L
    ) {
      row_indices
    } else {
      sample(
        row_indices,
        size = length(row_indices),
        replace = FALSE
      )
    }
  res_retained <-
    vec_order |>
    purrr::reduce(
      .init = integer(),
      .f = ~ {
        keep_row <-
          length(.x) == 0L ||
          all(distance_matrix[.y, .x] >= distance_km)

        if (
          keep_row
        ) {
          c(.x, .y)
        } else {
          .x
        }
      }
    )

  return(res_retained)
}
