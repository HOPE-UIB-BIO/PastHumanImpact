#' @title Permute values independently within blocks
#' @description
#' Randomly reorder a numeric vector within each supplied permutation block
#' while preserving the original record order.
#' @param values Numeric vector to permute.
#' @param blocks Vector defining exchangeability blocks.
#' @return A numeric vector with values permuted within blocks.
#' @examples
#' \dontrun{
#' permute_values_within_blocks(
#'   values = 1:6,
#'   blocks = rep(c("a", "b"), each = 3)
#' )
#' }
permute_values_within_blocks <- function(values, blocks) {
  assertthat::assert_that(
    is.numeric(values),
    length(values) == length(blocks),
    length(values) > 0L,
    !anyNA(blocks),
    msg = "Permutation values and blocks do not satisfy the contract."
  )

  res_values <-
    tibble::tibble(
      row_index = seq_along(values),
      block = as.character(blocks),
      value = values
    ) |>
    dplyr::group_by(.data[["block"]]) |>
    dplyr::mutate(
      permuted_value = sample(
        .data[["value"]],
        size = dplyr::n(),
        replace = FALSE
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data[["row_index"]]) |>
    dplyr::pull("permuted_value")

  return(res_values)
}
