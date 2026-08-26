#' @title Build blocked response permutations
#' @description
#' Generate a matrix of random permutations independently within each
#' exchangeability block, preserving the original record order in every
#' permutation column.
#' @param values Numeric vector to permute.
#' @param blocks Vector defining exchangeability blocks.
#' @param permutations Number of permutation columns.
#' @return A numeric matrix with records in rows and permutations in columns.
#' @examples
#' \dontrun{
#' build_block_permutation_matrix(
#'   values = 1:6,
#'   blocks = rep(c("a", "b"), each = 3),
#'   permutations = 99L
#' )
#' }
build_block_permutation_matrix <- function(
  values,
  blocks,
  permutations
) {
  assertthat::assert_that(
    is.numeric(values),
    length(values) == length(blocks),
    length(values) > 0L,
    !anyNA(blocks),
    is.numeric(permutations),
    length(permutations) == 1L,
    permutations >= 1L,
    msg = "Blocked permutation inputs do not satisfy the contract."
  )

  list_rows <-
    split(
      seq_along(values),
      factor(
        as.character(blocks),
        levels = unique(as.character(blocks))
      )
    )
  list_permutation_rows <- rep(list(list_rows), permutations)
  list_permutations <-
    list_permutation_rows |>
    purrr::map(
      .f = ~ {
        list_current_rows <- .x
        list_permuted_values <-
          list_current_rows |>
          purrr::map(
            .f = ~ {
              vec_rows <- .x

              if (
                length(vec_rows) == 1L
              ) {
                res <-
                  values[vec_rows]

                return(res)
              }

              res_values <-
                sample(
                  values[vec_rows],
                  size = length(vec_rows),
                  replace = FALSE
                )

              return(res_values)
            }
          )
        vec_permuted <-
          seq_along(list_current_rows) |>
          purrr::reduce(
            .init = numeric(length(values)),
            .f = ~ {
              vec_rows <- list_current_rows[[.y]]
              .x[vec_rows] <- list_permuted_values[[.y]]

              return(.x)
            }
          )

        return(vec_permuted)
      }
    )
  res_permutations <-
    purrr::reduce(
      .x = list_permutations,
      .f = cbind
    )

  return(res_permutations)
}
