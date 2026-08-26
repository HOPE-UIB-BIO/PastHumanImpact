#' @title Filter nested tables by sample identifier
#' @description
#' Filter one nested table column using the identifiers stored in another
#' list-column.
#' @param data_source Data frame containing both list-columns.
#' @param table_name Character scalar naming the nested table column.
#' @param id_name Character scalar naming the identifier-vector column.
#' @return The input data frame with the selected nested tables filtered.
#' @examples
#' \dontrun{
#' filter_nested_tables_by_sample_id(data_source, "levels", "valid_ids")
#' }
filter_nested_tables_by_sample_id <- function(
    data_source,
    table_name,
    id_name
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(table_name),
    length(table_name) == 1L,
    is.character(id_name),
    length(id_name) == 1L,
    all(c(table_name, id_name) %in% names(data_source)),
    msg = "Nested-table filtering arguments are invalid."
  )

  data_filtered <-
    data_source |>
    dplyr::mutate(
      "{table_name}" := purrr::map2(
        .x = .data[[table_name]],
        .y = .data[[id_name]],
        .f = ~ dplyr::filter(
          .x,
          .data[["sample_id"]] %in% .y
        )
      )
    )

  return(data_filtered)
}
