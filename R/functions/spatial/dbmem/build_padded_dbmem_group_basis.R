#' @title Build one grouped dbMEM basis to all records
#' @description
#' Place a group-level record basis into the rows of a global zero matrix and
#' assign globally unique sequential dbMEM names.
#' @param group_basis Numeric group-level dbMEM matrix.
#' @param row_indices Integer positions of group records in the global data.
#' @param n_records Total number of global records.
#' @param first_mem_index First global dbMEM index for this group.
#' @return A zero-padded numeric matrix with globally named dbMEM columns.
#' @examples
#' \dontrun{
#' build_padded_dbmem_group_basis(
#'   group_basis = matrix(1:4, nrow = 2),
#'   row_indices = c(1L, 3L),
#'   n_records = 3L,
#'   first_mem_index = 1L
#' )
#' }
build_padded_dbmem_group_basis <- function(
  group_basis,
  row_indices,
  n_records,
  first_mem_index
) {
  assertthat::assert_that(
    is.matrix(group_basis),
    is.numeric(row_indices),
    nrow(group_basis) == length(row_indices),
    is.numeric(n_records),
    length(n_records) == 1L,
    n_records >= length(row_indices),
    all(row_indices %in% seq_len(n_records)),
    is.numeric(first_mem_index),
    length(first_mem_index) == 1L,
    first_mem_index >= 1L,
    msg = "Grouped dbMEM padding inputs do not satisfy the contract."
  )

  n_mem <- ncol(group_basis)
  res_basis <- matrix(0, nrow = n_records, ncol = n_mem)

  if (
    n_mem > 0L
  ) {
    res_basis[row_indices, ] <- group_basis
    colnames(res_basis) <-
      sprintf(
        "dbmem_%03d",
        seq.int(first_mem_index, length.out = n_mem)
      )
  }

  return(res_basis)
}
