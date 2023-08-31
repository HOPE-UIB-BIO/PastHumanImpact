#!/bin/tcsh
#SBATCH -J spd_Europe_Arid_Steppe # (job name)
#SBATCH -o job.%j.out # (name of the job output file, %j expands to the job name)
#SBATCH -N 1 # (Number of requested nodes)
#SBATCH --ntasks-per-node 32 # (Number of requested cores per node)
#SBATCH -t 24:00:00 # (Requested wall time)

module load gnu10 R

R --no-save <<EOF
# ----------------------------------------------------------#
Sys.setenv(LANG = "en")

# Setup
library(tidyverse)
library(here)
library(mgcv)

fit_hgam <- function(
    x_var = "age", y_var = "var", group_var = "dataset_id",
    error_family = "gaussian(link = 'identity')", weights_var = NULL,
    smooth_basis = c("tp", "cr"), data_source, sel_k = 10, sel_m = NULL,
    common_trend = TRUE, use_parallel = TRUE, use_discrete = FALSE,
    max_iterations = 200, verbose = TRUE) {
  smooth_basis <- match.arg(smooth_basis)
  if (use_parallel == FALSE) {
    use_discrete <- FALSE
  }
  if (is.null(sel_m)) {
    sel_m <-
      ifelse(common_trend, 1, 2)
  }
  if (is.null(weights_var) == FALSE) {
    data_source <-
      data_source %>%
      dplyr::mutate(
        weights = with(
          data_source,
          get(weights_var)
        )
      )
  } else {
    data_source <-
      data_source %>%
      dplyr::mutate(
        weights = rep(
          1,
          nrow(data_source)
        )
      )
  }
  current_env <- environment()
  data_source <-
    data_source %>%
    dplyr::mutate(
      `:=`(
        !!group_var,
        as.factor(get(group_var))
      )
    )
  n_groups <-
    data_source %>%
    dplyr::distinct(get(group_var)) %>%
    purrr::pluck(1) %>%
    length()

  formula_gam <-
    paste0(
      y_var, " ~ s(", x_var, ", k = ", sel_k,
      ", bs = '", smooth_basis, "'", ")"
    )

  formula_hgam <-
    paste(
      paste0(
        "s(", x_var, ", by = ", group_var,
        ", bs = '", smooth_basis, "'", ", k = ", sel_k, ", m = ",
        sel_m, ")"
      ), paste0(
        "s(", group_var, ", bs = 're'", ", k = ",
        n_groups, ")"
      ),
      sep = " + "
    )

  if (common_trend == TRUE) {
    formula_hgam_fin <-
      paste(formula_gam, formula_hgam,
        sep = " + "
      )
  } else {
    formula_hgam_fin <-
      paste0(y_var, " ~ ", formula_hgam)
  }
  if (n_groups > 2) {
    cl <- NULL
    if (use_parallel == TRUE) {
      number_of_cores <- parallel::detectCores()
      if (number_of_cores > n_groups) {
        number_of_cores <- n_groups
      }

      if (use_discrete == FALSE) {
        sel_cluster_type <-
          ifelse(.Platform["OS.type"] ==
            "unix", "FORK", "PSOCK")
        cl <- parallel::makeCluster(number_of_cores,
          type = sel_cluster_type
        )
        try(
          fin_mod <-
            mgcv::bam(
              formula = stats::as.formula(formula_hgam_fin),
              data = data_source, weights = weights, method = "fREML",
              family = eval(parse(text = error_family)),
              cluster = cl, control = mgcv::gam.control(
                trace = verbose,
                maxit = max_iterations
              )
            )
        )
        if (!is.null(cl)) {
          parallel::stopCluster(cl)
          cl <- NULL
        }
        gc(verbose = FALSE)
      } else {
        try(
          fin_mod <- mgcv::bam(
            formula = stats::as.formula(formula_hgam_fin),
            data = data_source, weights = weights, method = "fREML",
            family = eval(parse(text = error_family)),
            discrete = TRUE, control = mgcv::gam.control(
              trace = verbose,
              maxit = max_iterations, nthreads = number_of_cores
            )
          )
        )
      }
    } else {
      try(
        fin_mod <- mgcv::bam(
          formula = stats::as.formula(formula_hgam_fin),
          data = data_source, weights = weights, method = "fREML",
          family = eval(parse(text = error_family)), control = mgcv::gam.control(
            trace = verbose,
            maxit = max_iterations
          )
        )
      )
    }
  } else {
    try(
      fin_mod <- mgcv::bam(
        formula = stats::as.formula(formula_gam),
        data = data_source, weights = weights, method = "fREML",
        family = eval(parse(text = error_family)), control = mgcv::gam.control(
          trace = verbose,
          maxit = max_iterations
        )
      )
    )
  }
  if (!exists("fin_mod", envir = current_env)) {
    fin_mod <- NA_real_
  }
  return(fin_mod)
}


#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

data_list <-
  readr::read_rds(
    here::here(
      "LA/prec_annual_Asia_Arid_Desert/data_to_fit.rds"
    )
  )

sel_data <-
  data_list %>%
  purrr::chuck("sel_data")

if (
  nrow(sel_data) > 0
) {
  n_recors <-
    sel_data %>%
    purrr::chuck("dataset_id") %>%
    unique() %>%
    length()

  message(n_recors)

  use_parallel <-
    parallel::detectCores(logical = FALSE) < n_recors

  # Fit GAM model
  data_mod <-
    fit_hgam(
      data_source = sel_data,
      x_var = data_list %>%
        purrr::chuck("x_var"),
      y_var = y_var %>%
        purrr::chuck("y_var"),
      group_var = group_var %>%
        purrr::chuck("group_var"),
      weights_var = weights_var %>%
        purrr::chuck("weights_var"),
      smooth_basis = smooth_basis %>%
        purrr::chuck("smooth_basis"),
      error_family = error_family %>%
        purrr::chuck("error_family"),
      sel_k = sel_k %>%
        purrr::chuck("sel_k"),
      common_trend = TRUE,
      use_parallel = use_parallel,
      max_iterations = 200,
      verbose = TRUE
    )

  readr::write_rds(
    data_mod,
    file = here::here("LA/prec_annual_Asia_Arid_Desert/mod.rds")
  )
}
# ----------------------------------------------------------#

q()

EOF
