testthat::test_that(
  "write_package_dependencies_manifest() writes deterministic contents",
  {
    path_output_file <-
      tempfile(
        fileext = ".R"
      )

    flag_first_write <-
      write_package_dependencies_manifest(
        vec_package_names = c(
          "dplyr",
          "rdacca.hp",
          "dplyr"
        ),
        path_output_file = path_output_file
      )

    vec_first_contents <-
      readLines(
        con = path_output_file,
        warn = FALSE
      )

    flag_second_write <-
      write_package_dependencies_manifest(
        vec_package_names = c(
          "dplyr",
          "rdacca.hp",
          "dplyr"
        ),
        path_output_file = path_output_file
      )

    testthat::expect_true(flag_first_write)
    testthat::expect_false(flag_second_write)
    testthat::expect_identical(
      vec_first_contents,
      c(
        "# This file is generated from `package_list` in `R/00_Config_file.R`.",
        "# Do not edit it manually.",
        "# Literal library calls let renv discover dynamically loaded packages.",
        "if (",
        "  FALSE",
        ") {",
        "  library(dplyr)",
        "  library(rdacca.hp)",
        "}"
      )
    )
  }
)

testthat::test_that(
  "write_package_dependencies_manifest() validates package names",
  {
    path_output_file <-
      tempfile(
        fileext = ".R"
      )

    testthat::expect_error(
      write_package_dependencies_manifest(
        vec_package_names = "not-a-package",
        path_output_file = path_output_file
      ),
      "Invalid package name"
    )
  }
)
