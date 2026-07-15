testthat::test_that("get_brms_chain_seeds() extracts every Stan seed", {
  class_name <-
    "PastHumanImpactTestStanFit"

  if (
    !methods::isClass(class_name)
  ) {
    methods::setClass(
      Class = class_name,
      slots = c(stan_args = "list")
    )
  }

  stan_fit <-
    methods::new(
      Class = class_name,
      stan_args = list(
        list(seed = 101L),
        list(seed = 202L)
      )
    )
  mod <-
    structure(
      list(fit = stan_fit),
      class = "brmsfit"
    )

  result <-
    get_brms_chain_seeds(mod)

  testthat::expect_identical(result, c(chain_1 = 101L, chain_2 = 202L))
})

testthat::test_that("get_brms_chain_seeds() rejects missing seeds", {
  class_name <-
    "PastHumanImpactTestStanFit"

  if (
    !methods::isClass(class_name)
  ) {
    methods::setClass(
      Class = class_name,
      slots = c(stan_args = "list")
    )
  }

  mod <-
    structure(
      list(
        fit = methods::new(
          Class = class_name,
          stan_args = list(list(chain_id = 1L))
        )
      ),
      class = "brmsfit"
    )

  testthat::expect_error(
    get_brms_chain_seeds(mod),
    regexp = "does not contain chain seeds"
  )
})
