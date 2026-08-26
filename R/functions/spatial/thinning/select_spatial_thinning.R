#' @title Repeatedly thin spatial records within strata
#' @description
#' Apply a random greedy minimum-distance algorithm independently within each
#' stratum and return retained identifiers for every distance and repetition.
#' @param data_source Data frame containing identifiers and coordinates.
#' @param strata Character vector naming thinning strata.
#' @param distance_km Positive minimum distances in kilometres.
#' @param repetitions Number of repetitions per distance.
#' @param id_col Name of the unique identifier column.
#' @param long_col Name of the longitude column.
#' @param lat_col Name of the latitude column.
#' @param seed Integer random seed.
#' @return A tibble of retained identifiers, strata, distances and repetitions.
#' @examples
#' \dontrun{
#' select_spatial_thinning(
#'   data_source = metadata,
#'   strata = c("region", "climatezone")
#' )
#' }
select_spatial_thinning <- function(
  data_source,
  strata = c("region", "climatezone"),
  distance_km = c(250, 500),
  repetitions = 100L,
  id_col = "dataset_id",
  long_col = "long",
  lat_col = "lat",
  seed = 1234L
) {
  required_columns <- c(id_col, long_col, lat_col, strata)
  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(strata),
    length(strata) > 0L,
    all(required_columns %in% names(data_source)),
    is.numeric(distance_km),
    length(distance_km) > 0L,
    all(is.finite(distance_km)),
    all(distance_km > 0),
    is.numeric(repetitions),
    length(repetitions) == 1L,
    repetitions >= 1L,
    msg = "Spatial thinning inputs do not satisfy the required contract."
  )

  data_coordinates <-
    validate_spatial_coordinates(
      data_source = data_source,
      id_col = id_col,
      long_col = long_col,
      lat_col = lat_col
    )
  assertthat::assert_that(
    !anyNA(data_coordinates[strata]),
    msg = "Thinning strata cannot contain missing values."
  )
  mat_distance <-
    compute_spatial_distance_matrix(
      data_source = data_coordinates,
      id_col = id_col,
      long_col = long_col,
      lat_col = lat_col
    )
  vec_stratum <-
    interaction(
      data_coordinates[strata],
      drop = TRUE,
      lex.order = TRUE
    )

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv)
  if (
    old_seed_exists
  ) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv)
  }
  on.exit(
    {
      if (
        old_seed_exists
      ) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (
        exists(".Random.seed", envir = .GlobalEnv)
      ) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  set.seed(seed)

  data_replicates <-
    tidyr::expand_grid(
      distance_km = distance_km,
      repetition = seq_len(repetitions)
    )
  res_thinning <-
    data_replicates |>
    purrr::pmap(
      .f = ~ select_spatial_thinning_replicate(
        data_coordinates = data_coordinates,
        distance_matrix = mat_distance,
        strata_factor = vec_stratum,
        distance_km = ..1,
        repetition = ..2,
        id_col = id_col,
        strata = strata
      )
    ) |>
    dplyr::bind_rows()

  return(res_thinning)
}
