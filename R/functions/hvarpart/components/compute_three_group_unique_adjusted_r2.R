#' @title Compute pure fractions for three predictor groups
#' @description
#' Calculate pure human, climate, and structural adjusted R-squared fractions
#' and retain the combined shared and unexplained fractions.
#' @param response Numeric response matrix.
#' @param human Numeric human-predictor matrix.
#' @param climate Numeric climate-predictor matrix.
#' @param structure Numeric time or space predictor matrix.
#' @param structure_name Label for the structural group.
#' @return A six-row adjusted-R-squared fraction table.
#' @examples
#' \dontrun{
#' compute_three_group_unique_adjusted_r2(y, human, climate, time)
#' }
compute_three_group_unique_adjusted_r2 <- function(
  response,
  human,
  climate,
  structure,
  structure_name = "time"
) {
  assertthat::assert_that(
    is.matrix(response),
    is.matrix(human),
    is.matrix(climate),
    is.matrix(structure),
    nrow(response) == nrow(human),
    nrow(response) == nrow(climate),
    nrow(response) == nrow(structure),
    assertthat::is.string(structure_name),
    msg = "Three-group partial-fraction inputs are invalid."
  )

  pure_human <-
    compute_partial_rda_adjusted_r_squared(
      response = response,
      explanatory = human,
      conditioning = cbind(climate, structure)
    )
  pure_climate <-
    compute_partial_rda_adjusted_r_squared(
      response = response,
      explanatory = climate,
      conditioning = cbind(human, structure)
    )
  pure_structure <-
    compute_partial_rda_adjusted_r_squared(
      response = response,
      explanatory = structure,
      conditioning = cbind(human, climate)
    )
  total_explained <-
    compute_partial_rda_adjusted_r_squared(
      response = response,
      explanatory = cbind(human, climate, structure)
    )
  res_fractions <-
    tibble::tibble(
      fraction = c(
        "pure_human",
        "pure_climate",
        stringr::str_c("pure_", structure_name),
        "shared",
        "total_explained",
        "unexplained"
      ),
      adjusted_r_squared = c(
        pure_human,
        pure_climate,
        pure_structure,
        total_explained - pure_human - pure_climate - pure_structure,
        total_explained,
        1 - total_explained
      )
    )

  return(res_fractions)
}
