testthat::test_that(
  "save_human_event_sensitivity_figures() validates inputs",
  {
    testthat::expect_error(
      save_human_event_sensitivity_figures(
        plot_spatial = NULL,
        temporal_plots = list(),
        path_spatial = "spatial",
        temporal_paths = character()
      ),
      "contract"
    )
  }
)