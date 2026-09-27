#' @title Prepare canonical, bridge, and joint-human H1 decision evidence
#' @description Combine exact common-cohort spatial and temporal evidence for
#' the internal co-author decision memo. Public geography columns use
#' `continental_region` and `region` terminology.
#' @param canonical_balance Canonical square-root-SPD spatial balance table.
#' @param canonical_components Canonical square-root-SPD component table.
#' @param canonical_temporal Canonical square-root-SPD temporal composition.
#' @param bridge_comparisons Matched-SPD bridge comparison tables.
#' @param joint_comparisons Filtered joint-human comparison tables.
#' @param filtered_temporal_unique Unique adjusted R-squared results for the
#' matched bridge and joint time-controlled models.
#' @param age_min Young boundary in cal yr BP.
#' @param age_max Old boundary in cal yr BP.
#' @return A named list of common-cohort values, summaries, and transitions.
#' @examples
#' \dontrun{
#' prepare_human_proxy_decision_memo_evidence(
#'   canonical_balance, canonical_components, canonical_temporal,
#'   bridge, joint, unique_r2
#' )
#' }
prepare_human_proxy_decision_memo_evidence <- function(
  canonical_balance,
  canonical_components,
  canonical_temporal,
  bridge_comparisons,
  joint_comparisons,
  filtered_temporal_unique,
  age_min = 2000,
  age_max = 8000
) {
  assertthat::assert_that(
    is.data.frame(canonical_balance),
    is.data.frame(canonical_components),
    is.data.frame(canonical_temporal),
    is.list(bridge_comparisons),
    is.list(joint_comparisons),
    is.data.frame(filtered_temporal_unique),
    all(c("balance_common", "composition_common") %in%
      names(bridge_comparisons)),
    all(c("balance_common", "composition_common") %in%
      names(joint_comparisons)),
    msg = "Decision-memo evidence inputs do not satisfy the contract."
  )
  spatial <- prepare_human_proxy_spatial_decision_evidence(
    canonical_balance = canonical_balance,
    canonical_components = canonical_components,
    bridge_balance = bridge_comparisons[["balance_common"]],
    joint_balance = joint_comparisons[["balance_common"]],
    filtered_temporal_unique = filtered_temporal_unique
  )
  temporal <- prepare_human_proxy_temporal_decision_evidence(
    canonical_temporal = canonical_temporal,
    bridge_temporal = bridge_comparisons[["composition_common"]],
    joint_temporal = joint_comparisons[["composition_common"]],
    age_min = age_min,
    age_max = age_max
  )
  res <- c(spatial, temporal)

  return(res)
}
