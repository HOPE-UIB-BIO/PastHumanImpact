#' @title Derive dbMEM-zero adjusted importance estimates
#' @description
#' Set all candidate dbMEM columns to zero, predict one aggregation model, and
#' summarise adjusted balances at all Issue 325 aggregation levels.
#' @param model_object Fitted spatial importance model.
#' @param data_model Model data frame.
#' @param profile_name Profile label.
#' @param weight_col Weight column name.
#' @param mem_names Candidate dbMEM column names.
#' @return Adjusted overall, regional, and stratum estimates.
#' @examples
#' \dontrun{
#' get_adjusted_spatial_importance_estimates(
#'   model_object = model,
#'   data_model = records,
#'   profile_name = "signed",
#'   weight_col = "signed_weight",
#'   mem_names = "dbmem_001"
#' )
#' }
get_adjusted_spatial_importance_estimates <- function(
  model_object,
  data_model,
  profile_name,
  weight_col,
  mem_names = character()
) {
  assertthat::assert_that(
    inherits(model_object, "lm"),
    is.data.frame(data_model),
    assertthat::is.string(profile_name),
    assertthat::is.string(weight_col),
    is.character(mem_names),
    all(c(weight_col, mem_names, "region", "climatezone") %in%
      names(data_model)),
    msg = "Adjusted spatial estimate inputs do not satisfy the contract."
  )

  data_adjusted <-
    data_model |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(mem_names),
        .fns = ~ 0
      ),
      adjusted_balance = as.numeric(
        stats::predict(model_object, newdata = dplyr::pick(dplyr::everything()))
      )
    )
  data_levels <-
    tibble::tibble(
      level_name = c("overall", "region", "region_climatezone"),
      group_vars = list(
        character(),
        "region",
        c("region", "climatezone")
      )
    )
  res_estimates <-
    data_levels |>
    purrr::pmap_dfr(
      .f = ~ summarise_adjusted_spatial_importance_level(
        data_adjusted = data_adjusted,
        weight_col = weight_col,
        group_vars = ..2,
        level_name = ..1,
        profile_name = profile_name
      )
    ) |>
    dplyr::mutate(
      region = dplyr::coalesce(as.character(.data[["region"]]), "All"),
      climatezone = dplyr::coalesce(
        as.character(.data[["climatezone"]]),
        "All"
      ),
      ranking = dplyr::case_when(
        .data[["adjusted_balance"]] > 0 ~ "human",
        .data[["adjusted_balance"]] < 0 ~ "climate",
        .default = "tie"
      )
    )

  return(res_estimates)
}
