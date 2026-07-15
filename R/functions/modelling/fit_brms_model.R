#' @title Fit a temporal brms model
#' @description
#' Fit one temporal `brms` model either from explicit arguments or from a
#' one-row model configuration table. Errors during fitting return `NA_real_`
#' so lifecycle scripts can flag reruns.
#' @param data_source Data frame with response, predictor, and grouping columns.
#' @param model_config_row Optional one-row model configuration data frame.
#' @param x_var Name of the predictor column.
#' @param x_model_var Name of the standardised predictor column.
#' @param x_mean Optional predictor mean. Calculated from `data_source` when
#' omitted and no config row is supplied.
#' @param x_sd Optional predictor standard deviation. Calculated from
#' `data_source` when omitted and no config row is supplied.
#' @param y_var Name of the response column.
#' @param group_var Name of the repeated-record grouping column.
#' @param family_key Stable family key used by `get_model_family()`.
#' @param smooth_basis Smooth basis type (`"cr"` or `"tp"`).
#' @param sel_k Basis dimension for the common smooth.
#' @param sel_m Optional smooth penalty order.
#' @param common_trend Logical. If `TRUE`, include a shared smooth.
#' @param model_profile Formula profile used by `get_hgam_formula()`.
#' @param stratum_var Name of region-climatezone stratum column.
#' @param stratum_k Basis dimension for stratum smooths.
#' @param group_k Basis dimension for dataset-level factor smooths.
#' @param total_iterations Integer total MCMC iterations. Used when
#' `model_config_row` is not supplied.
#' @param min_iterations_per_chain Integer minimum iterations per chain.
#' @param max_chains Integer maximum number of chains.
#' @param adapt_delta Numeric target average proposal acceptance probability.
#' @param max_treedepth Integer maximum NUTS tree depth.
#' @param sampling_seed Positive integer sampling seed passed to `brms`.
#' @param control Optional list passed to the `control` argument of
#' `brms::brm()`. Explicit values override `adapt_delta` and `max_treedepth`.
#' @param verbose Logical. If `TRUE`, progress messages are printed.
#' @param ... Additional arguments passed to `brms::brm()`.
#' @return A fitted `brmsfit` object, or `NA_real_` when fitting fails.
#' @examples
#' \dontrun{
#' mod <- fit_brms_model(data_source = data_model, model_config_row = config[1, ])
#' }
fit_brms_model <- function(
  data_source,
  model_config_row = NULL,
  x_var = "age_ka",
  x_model_var = stringr::str_c(x_var, "scaled", sep = "_"),
  x_mean = NULL,
  x_sd = NULL,
  y_var = "value",
  group_var = "dataset_id",
  family_key = "student_identity",
  smooth_basis = c("cr", "tp"),
  sel_k = 8,
  sel_m = NULL,
  common_trend = TRUE,
  model_profile = c(
    "within_stratum_dataset_fs",
    "within_stratum_dataset_slope",
    "within_stratum_dataset_intercept",
    "dataset_smooth",
    "stratum_fs",
    "stratum_by",
    "stratum_fs_dataset_slope",
    "stratum_fs_dataset_fs"
  ),
  stratum_var = "stratum",
  stratum_k = 5,
  group_k = 3,
  total_iterations = 3200,
  min_iterations_per_chain = 100,
  max_chains = 4,
  adapt_delta = 0.9,
  max_treedepth = 10,
  sampling_seed = 1234L,
  control = NULL,
  verbose = TRUE,
  ...
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  if (
    isFALSE(is.null(model_config_row))
  ) {
    assertthat::assert_that(
      is.data.frame(model_config_row),
      nrow(model_config_row) == 1,
      msg = "`model_config_row` must be a one-row data frame."
    )

    required_config_cols <-
      c(
        "variable",
        "family_key",
        "model_profile",
        "x_var",
        "x_model_var",
        "x_mean",
        "x_sd",
        "y_var",
        "group_var",
        "stratum_var",
        "smooth_basis",
        "common_k",
        "group_k",
        "is_model_eligible",
        "total_iterations",
        "min_iterations_per_chain",
        "max_chains",
        "adapt_delta",
        "max_treedepth",
        "sampling_seed"
      )

    assertthat::assert_that(
      all(required_config_cols %in% names(model_config_row)),
      msg = "`model_config_row` is missing required config columns."
    )

    sel_variable <- model_config_row[["variable"]][1]
    x_var <- model_config_row[["x_var"]][1]
    x_model_var <- model_config_row[["x_model_var"]][1]
    x_mean <- model_config_row[["x_mean"]][1]
    x_sd <- model_config_row[["x_sd"]][1]
    y_var <- model_config_row[["y_var"]][1]
    group_var <- model_config_row[["group_var"]][1]
    family_key <- model_config_row[["family_key"]][1]
    model_profile <- model_config_row[["model_profile"]][1]
    stratum_var <- model_config_row[["stratum_var"]][1]
    smooth_basis <- model_config_row[["smooth_basis"]][1]
    sel_k <- model_config_row[["common_k"]][1]
    group_k <- model_config_row[["group_k"]][1]
    total_iterations <- model_config_row[["total_iterations"]][1]
    min_iterations_per_chain <-
      model_config_row[["min_iterations_per_chain"]][1]
    max_chains <- model_config_row[["max_chains"]][1]
    adapt_delta <- model_config_row[["adapt_delta"]][1]
    max_treedepth <- model_config_row[["max_treedepth"]][1]
    sampling_seed <- model_config_row[["sampling_seed"]][1]
    assertthat::assert_that(
      isTRUE(model_config_row[["is_model_eligible"]][1]),
      msg = "The selected model is not eligible for fitting."
    )

    assertthat::assert_that(
      all(c("variable", "region", "climatezone", x_var, y_var, group_var) %in%
        names(data_source)),
      msg = "`data_source` is missing required model columns."
    )

    data_source <-
      data_source %>%
      dplyr::filter(variable == sel_variable)

    if (
      all(c("region", "climatezone") %in% names(model_config_row))
    ) {
      data_source <-
        data_source %>%
        dplyr::filter(
          region == model_config_row[["region"]][1],
          climatezone == model_config_row[["climatezone"]][1]
        )
    }

    assertthat::assert_that(
      nrow(data_source) > 0,
      msg = "`data_source` has no rows for the selected model variable."
    )
  }

  model_profile <- match.arg(model_profile)
  smooth_basis <- match.arg(smooth_basis)

  assertthat::assert_that(
    is.character(y_var),
    length(y_var) == 1,
    is.character(x_var),
    length(x_var) == 1,
    is.character(x_model_var),
    length(x_model_var) == 1,
    x_var != x_model_var,
    is.character(group_var),
    length(group_var) == 1,
    is.character(stratum_var),
    length(stratum_var) == 1,
    msg = "Model variable names must be character scalars."
  )
  assertthat::assert_that(
    is.character(family_key),
    length(family_key) == 1,
    msg = "`family_key` must be a character scalar."
  )
  assertthat::assert_that(
    assertthat::is.count(sel_k),
    assertthat::is.count(stratum_k),
    assertthat::is.count(group_k),
    assertthat::is.count(total_iterations),
    assertthat::is.count(min_iterations_per_chain),
    assertthat::is.count(max_chains),
    assertthat::is.count(max_treedepth),
    assertthat::is.count(sampling_seed),
    msg = "Smooth and iteration settings must be positive integers."
  )
  assertthat::assert_that(
    is.numeric(adapt_delta),
    length(adapt_delta) == 1,
    adapt_delta > 0,
    adapt_delta < 1,
    msg = "`adapt_delta` must be a numeric scalar between 0 and 1."
  )
  assertthat::assert_that(
    is.null(control) || is.list(control),
    msg = "`control` must be NULL or a list."
  )
  assertthat::assert_that(
    is.logical(common_trend),
    length(common_trend) == 1,
    !is.na(common_trend),
    msg = "`common_trend` must be TRUE or FALSE."
  )
  assertthat::assert_that(
    is.logical(verbose),
    length(verbose) == 1,
    !is.na(verbose),
    msg = "`verbose` must be TRUE or FALSE."
  )

  required_data_cols <-
    if (
      model_profile == "dataset_smooth"
    ) {
      c(group_var, y_var, x_var)
    } else {
      c(group_var, stratum_var, y_var, x_var)
    }

  assertthat::assert_that(
    all(required_data_cols %in% names(data_source)),
    msg = "`data_source` is missing required model columns."
  )

  if (
    is.null(x_mean)
  ) {
    x_mean <- mean(data_source[[x_var]])
  }

  if (
    is.null(x_sd)
  ) {
    x_sd <- stats::sd(data_source[[x_var]])
  }

  data_source <-
    standardise_model_predictor(
      data_source = data_source,
      x_var = x_var,
      x_model_var = x_model_var,
      x_mean = x_mean,
      x_sd = x_sd
    )

  data_source <-
    data_source %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(group_var),
        as.factor
      )
    )

  if (
    model_profile != "dataset_smooth"
  ) {
    data_source <-
      data_source %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(stratum_var),
          as.factor
        )
      )
  }

  n_groups <-
    data_source %>%
    dplyr::distinct(.data[[group_var]]) %>%
    nrow()

  n_chains <-
    min(max_chains, parallelly::availableCores())

  if (
    (total_iterations / n_chains) < min_iterations_per_chain
  ) {
    n_chains <-
      max(1, floor(total_iterations / min_iterations_per_chain))
  }

  iter_per_chain <-
    ceiling(total_iterations / n_chains)

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform(
      paste(
        "Fitting",
        ifelse(
          is.null(model_config_row),
          "brms model",
          model_config_row[["model_id"]][1]
        ),
        "with",
        n_chains,
        "chain(s)."
      )
    )
  }

  formula_model <-
    get_hgam_formula(
      y_var = y_var,
      x_var = x_model_var,
      group_var = group_var,
      smooth_basis = smooth_basis,
      sel_k = sel_k,
      sel_m = sel_m,
      n_groups = n_groups,
      common_trend = common_trend,
      model_profile = model_profile,
      stratum_var = stratum_var,
      stratum_k = stratum_k,
      group_k = group_k
    )

  if (
    isFALSE(is.null(model_config_row)) &&
      "formula_text" %in% names(model_config_row)
  ) {
    assertthat::assert_that(
      identical(formula_model, model_config_row[["formula_text"]][1]),
      msg = "The generated formula does not match config `formula_text`."
    )
  }

  brms_control <-
    utils::modifyList(
      x = list(
        adapt_delta = adapt_delta,
        max_treedepth = max_treedepth
      ),
      val = if (
        is.null(control)
      ) {
        list()
      } else {
        control
      }
    )

  res_model <-
    tryCatch(
      {
        brms::brm(
          formula = brms::bf(stats::as.formula(formula_model)),
          data = data_source,
          family = get_model_family(family_key = family_key),
          silent = ifelse(isTRUE(verbose), 1, 2),
          chains = n_chains,
          cores = n_chains,
          iter = iter_per_chain,
          seed = as.integer(sampling_seed),
          control = brms_control,
          ...
        )
      },
      error = function(err) {
        if (
          isTRUE(verbose)
        ) {
          cli::cli_warn(
            paste("Model fitting failed:", conditionMessage(err))
          )
        }

        res_failure <- NA_real_
        attr(res_failure, "fit_error") <- conditionMessage(err)

        res_failure
      }
    )

  return(res_model)
}
