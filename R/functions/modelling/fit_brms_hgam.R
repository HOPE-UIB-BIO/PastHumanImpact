fit_brms_hgam <- function(
    data_source,
    x_var = "age",
    y_var = "var",
    group_var = "dataset_id",
    error_family = "gaussian",
    smooth_basis = c("tp", "cr"),
    sel_k = 10,
    sel_m = NULL,
    common_trend = TRUE,
    verbose = TRUE,
    chains = 4,
    ...) {
  RUtilpol::check_class("y_var", "character")

  RUtilpol::check_class("x_var", "character")

  RUtilpol::check_class("group_var", "character")

  RUtilpol::check_class("error_family", "character")

  smooth_basis <- match.arg(smooth_basis)

  RUtilpol::check_class("smooth_basis", "character")

  RUtilpol::check_vector_values("smooth_basis", c("tp", "cr"))

  RUtilpol::check_class("data_source", "data.frame")

  RUtilpol::check_col_names(
    "data_source",
    c(
      eval(group_var),
      eval(y_var),
      eval(x_var)
    )
  )

  RUtilpol::check_class("sel_k", "numeric")


  assertthat::assert_that(
    assertthat::is.count(sel_k),
    msg = "'sel_k' must be an integer"
  )

  RUtilpol::check_class("sel_m", c("NULL", "numeric"))

  if (
    isFALSE(is.null(sel_m))
  ) {
    assertthat::assert_that(
      assertthat::is.count(sel_m),
      msg = "'sel_m' must be an integer"
    )
  }
  RUtilpol::check_class("common_trend", "logical")

  RUtilpol::check_class("verbose", "logical")

  if (
    is.null(sel_m)
  ) {
    sel_m <-
      ifelse(common_trend, 1, 2)
  }

  assertthat::assert_that(
    assertthat::is.count(chains),
    msg = "'chains' must be an integer"
  )

  number_of_cores <- parallelly::availableCores()

  if (
    number_of_cores > chains
  ) {
    number_of_cores <- chains
  }

  if (
    isTRUE(verbose)
  ) {
    RUtilpol::output_comment(
      paste(
        "Using parallel estimation using N cores = ",
        number_of_cores
      )
    )
  }

  current_env <- environment()

  data_source <-
    data_source %>%
    dplyr::mutate(!!group_var := as.factor(get(group_var)))

  n_groups <-
    data_source %>%
    dplyr::distinct(get(group_var)) %>%
    purrr::pluck(1) %>%
    length()

  if (
    isTRUE(verbose)
  ) {
    RUtilpol::output_comment(
      paste("N datasets:", n_groups)
    )
  }

  formula_hgam_fin <-
    get_hgam_formula(
      y_var = y_var,
      x_var = x_var,
      group_var = group_var,
      sel_k = sel_k,
      sel_m = sel_m, ,
      n_groups = n_groups,
      common_trend = common_trend
    )

  try(
    fin_mod <-
      brms::brm(
        formula = brms::bf(stats::as.formula(formula_hgam_fin)),
        data = data_source,
        family = eval(parse(text = error_family)),
        silent = ifelse(isTRUE(verbose), 1, 0),
        chains = chains,
        cores = number_of_cores,
        ...
      )
  )

  if (
    !exists("fin_mod", envir = current_env)
  ) {
    fin_mod <- NA_real_
  }

  return(fin_mod)
}
