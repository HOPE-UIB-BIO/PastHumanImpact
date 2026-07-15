#' @title Get chain seeds from a brms model
#' @description
#' Extract the actual chain-specific Stan seeds stored in a fitted `brms`
#' model.
#' @param mod Fitted `brmsfit` object.
#' @return Named integer vector with one seed per chain.
#' @examples
#' \dontrun{
#' chain_seeds <- get_brms_chain_seeds(mod)
#' }
get_brms_chain_seeds <- function(mod) {
  assertthat::assert_that(
    inherits(mod, "brmsfit"),
    msg = "`mod` must be a fitted brms model."
  )

  stan_args <-
    mod[["fit"]]@stan_args

  if (
    length(stan_args) == 0L ||
      any(vapply(stan_args, function(x) is.null(x[["seed"]]), logical(1)))
  ) {
    cli::cli_abort("The fitted model does not contain chain seeds.")
  }

  res_seeds <-
    vapply(
      stan_args,
      function(x) as.integer(x[["seed"]][1]),
      integer(1)
    )

  names(res_seeds) <-
    stringr::str_c("chain_", seq_along(res_seeds))

  return(res_seeds)
}
