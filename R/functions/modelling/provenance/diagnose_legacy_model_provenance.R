#' @title Audit provenance of one legacy brms model
#' @description
#' Locate one legacy fitted model, recover its chain-specific Stan seeds, and
#' record its saved family and sampling dimensions.
#' @param model_id Character scalar model identifier.
#' @param model_dir Existing directory containing fitted model files.
#' @return One-row tibble containing the audit result and recovered metadata.
#' @examples
#' \dontrun{
#' audit <- diagnose_legacy_model_provenance(
#'   model_id = "model_a",
#'   model_dir = "Data/Temporal_models/Mods"
#' )
#' }
diagnose_legacy_model_provenance <- function(
  model_id,
  model_dir
) {
  assertthat::assert_that(
    is.character(model_id),
    length(model_id) == 1L,
    !is.na(model_id),
    nzchar(model_id),
    msg = "`model_id` must be a non-empty character scalar."
  )
  assertthat::assert_that(
    is.character(model_dir),
    length(model_dir) == 1L,
    dir.exists(model_dir),
    msg = "`model_dir` must be an existing directory."
  )

  model_file_name <-
    RUtilpol::get_latest_file_name(
      file_name = model_id,
      dir = model_dir,
      verbose = FALSE
    )

  if (
    is.na(model_file_name)
  ) {
    res_audit <-
      tibble::tibble(
        model_id = model_id,
        model_file_name = NA_character_,
        model_chain_seeds_json = NA_character_,
        model_seed_source = "unavailable",
        model_provenance_status = "legacy_audit_failed",
        model_audit_reason = "model_file_not_found",
        saved_model_family = NA_character_,
        saved_model_link = NA_character_,
        saved_model_n_chains = NA_integer_,
        saved_model_iterations_per_chain = NA_integer_,
        saved_model_warmup_per_chain = NA_integer_
      )

    return(res_audit)
  }

  mod <-
    tryCatch(
      load_brms_model_file(
        model_dir = model_dir,
        model_file_name = model_file_name
      ),
      error = function(err) err
    )

  if (
    inherits(mod, "error") || !inherits(mod, "brmsfit")
  ) {
    audit_reason <-
      if (
        inherits(mod, "error")
      ) {
        conditionMessage(mod)
      } else {
        "saved_object_is_not_brmsfit"
      }

    res_audit <-
      tibble::tibble(
        model_id = model_id,
        model_file_name = model_file_name,
        model_chain_seeds_json = NA_character_,
        model_seed_source = "unavailable",
        model_provenance_status = "legacy_audit_failed",
        model_audit_reason = audit_reason,
        saved_model_family = NA_character_,
        saved_model_link = NA_character_,
        saved_model_n_chains = NA_integer_,
        saved_model_iterations_per_chain = NA_integer_,
        saved_model_warmup_per_chain = NA_integer_
      )

    return(res_audit)
  }

  chain_seeds <-
    tryCatch(
      compute_brms_chain_seeds(mod),
      error = function(err) err
    )

  if (
    inherits(chain_seeds, "error")
  ) {
    model_chain_seeds_json <- NA_character_
    model_seed_source <- "unavailable"
    model_provenance_status <- "legacy_audit_failed"
    model_audit_reason <- conditionMessage(chain_seeds)
  } else {
    model_chain_seeds_json <-
      chain_seeds %>%
      as.list() %>%
      jsonlite::toJSON(
        auto_unbox = TRUE,
        digits = NA
      ) %>%
      as.character()
    model_seed_source <- "recovered_stan_chain_seeds"
    model_provenance_status <- "legacy_seeds_recovered"
    model_audit_reason <- NA_character_
  }

  saved_model_family <-
    tryCatch(
      as.character(mod[["family"]][["family"]][1]),
      error = function(err) NA_character_
    )
  saved_model_link <-
    tryCatch(
      as.character(mod[["family"]][["link"]][1]),
      error = function(err) NA_character_
    )
  saved_model_n_chains <-
    tryCatch(
      as.integer(mod[["fit"]]@sim[["chains"]]),
      error = function(err) NA_integer_
    )
  saved_model_iterations_per_chain <-
    tryCatch(
      as.integer(mod[["fit"]]@sim[["iter"]]),
      error = function(err) NA_integer_
    )
  saved_model_warmup_per_chain <-
    tryCatch(
      as.integer(mod[["fit"]]@sim[["warmup"]]),
      error = function(err) NA_integer_
    )

  res_audit <-
    tibble::tibble(
      model_id = model_id,
      model_file_name = model_file_name,
      model_chain_seeds_json = model_chain_seeds_json,
      model_seed_source = model_seed_source,
      model_provenance_status = model_provenance_status,
      model_audit_reason = model_audit_reason,
      saved_model_family = saved_model_family,
      saved_model_link = saved_model_link,
      saved_model_n_chains = saved_model_n_chains,
      saved_model_iterations_per_chain =
        saved_model_iterations_per_chain,
      saved_model_warmup_per_chain = saved_model_warmup_per_chain
    )

  return(res_audit)
}
