testthat::test_that("compute_hvarpart_permutation_significance() returns significance table with injected backend", {
  dv <-
    data.frame(
      y1 = c(1, 2, 3, 4),
      y2 = c(2, 3, 4, 5)
    )

  iv <-
    data.frame(
      x1 = c(1, 2, 3, 4),
      x2 = c(4, 3, 2, 1)
    )

  rdacca_fun <-
    function(dv,
             iv,
             method,
             type,
             scale,
             add,
             sqrt.dist,
             n.perm) {
      list(
        Hier.part = data.frame(
          Individual = c(0.4, 0.6)
        )
      )
    }

  res <-
    compute_hvarpart_permutation_significance(
      dv = dv,
      iv = iv,
      series = FALSE,
      permutations = 2,
      verbose = FALSE,
      rdacca_fun = rdacca_fun
    )

  testthat::expect_true(all(c("Individual", "Pr(>I)") %in% names(res)))
  testthat::expect_equal(nrow(res), 2L)
})

testthat::test_that("compute_hvarpart_permutation_significance() validates arguments", {
  dv <- data.frame(y1 = c(1, 2), y2 = c(2, 3))
  iv <- data.frame(x1 = c(1, 2), x2 = c(2, 1))

  testthat::expect_error(
    compute_hvarpart_permutation_significance(
      dv = dv,
      iv = iv,
      permutations = 0
    ),
    regexp = "must be a positive"
  )

  testthat::expect_error(
    compute_hvarpart_permutation_significance(
      dv = dv,
      iv = iv,
      rdacca_fun = "not_a_function"
    ),
    regexp = "must be a function"
  )
})
