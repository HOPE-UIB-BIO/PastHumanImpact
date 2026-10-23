#' @title Build matched bridge and filtered joint human-proxy specifications
#' @description Define the filtered joint human block and a diagnostic matched
#' square-root-SPD bridge. Climate selection is performed independently and
#' reused across both specifications.
#' @return A two-row tibble with identifiers, labels, candidate columns, and
#' deterministic preference orders.
#' @examples
#' build_human_proxy_model_specifications()
build_human_proxy_model_specifications <- function() {
  res <- tibble::tibble(
    model_id = c("joint_filtered", "spd_matched_bridge"),
    model_label = c(
      "Filtered joint human block",
      "Matched square-root SPD bridge"
    ),
    human_candidates = list(
      c("spd_sqrt", "kk10_fraction", "hyde_sqrt"),
      "spd_sqrt"
    ),
    human_preference = list(
      c("spd_sqrt", "kk10_fraction", "hyde_sqrt"),
      "spd_sqrt"
    ),
    exploratory = c(FALSE, TRUE)
  )
  assertthat::assert_that(
    nrow(res) == 2L,
    !anyDuplicated(res[["model_id"]]),
    all(purrr::map_int(res[["human_candidates"]], length) >= 1L),
    msg = "Human-proxy model specifications are internally inconsistent."
  )

  return(res)
}
