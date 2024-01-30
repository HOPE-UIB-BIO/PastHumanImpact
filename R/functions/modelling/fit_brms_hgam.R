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

  formula_gam <-
    paste0(
      y_var,
      " ~ s(",
      x_var,
      ", k = ", sel_k,
      ", bs = '", smooth_basis, "'",
      ")"
    )

  formula_hgam <-
    paste(
      paste0(
        "s(", x_var,
        ", by = ", group_var,
        ", bs = '", smooth_basis, "'",
        ", k = ", sel_k,
        ", m = ", sel_m,
        ")"
      ),
      paste0(
        "s(", group_var,
        ", bs = 're'",
        ", k = ", n_groups,
        ")"
      ),
      sep = " + "
    )

  if (
    isTRUE(common_trend)
  ) {
    formula_hgam_fin <-
      paste(
        formula_gam,
        formula_hgam,
        sep = " + "
      )
  } else {
    formula_hgam_fin <-
      paste0(y_var, " ~ ", formula_hgam)
  }

  try(
    fin_mod <-
      brms::brm(
        formula = stats::as.formula(formula_hgam_fin),
        data = data_source,
        family = eval(parse(text = error_family)),
        silent = ifelse(isTRUE(verbose), 1, 0),
        chains = chains,
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
