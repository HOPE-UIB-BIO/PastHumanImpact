#' @title Plot the filtered joint-human temporal composition comparison
#' @description Reuse the canonical Figure-4 region-by-age layout, including
#' compact age facets, right-side scale, age arrow, and established legend.
#' @param data_stack Eligible composition source table.
#' @param model_specifications The one-row joint-model specification.
#' @param age_min Youngest displayed age in years BP.
#' @param age_max Oldest displayed age in years BP.
#' @param space_colour Colour for spatial structure.
#' @return A canonical joint-human temporal composition figure.
#' @examples
#' \dontrun{plot_human_proxy_temporal_comparison(stack, specs)}
plot_human_proxy_temporal_comparison <- function(
  data_stack,
  model_specifications,
  age_min = 2000,
  age_max = 8000,
  space_colour = "#A79BB8"
) {
  assertthat::assert_that(
    is.data.frame(data_stack),
    is.data.frame(model_specifications),
    nrow(model_specifications) == 1L,
    identical(model_specifications$model_id[[1]], "joint_filtered"),
    msg = "Filtered joint temporal-plot inputs do not satisfy the contract."
  )
  res <- plot_h1_temporal_joint_human_proxy_composition(
    data_stack = data_stack,
    age_min = age_min,
    age_max = age_max,
    space_colour = space_colour,
    human_label = "Human (√SPD + KK10 + √HYDE)"
  )

  return(res)
}
