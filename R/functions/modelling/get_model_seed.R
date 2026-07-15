#' @title Generate deterministic model sampling seeds
#' @description
#' Generate stable positive integer seeds from model IDs, attempt numbers, and
#' base seeds without relying on session state.
#' @param model_id Character vector of model identifiers.
#' @param seed_attempt Positive integer vector of model attempt numbers.
#' @param seed_base Non-negative integer vector of base seeds.
#' @return Integer vector with one deterministic seed per model attempt.
#' @examples
#' get_model_seed(
#'   model_id = c("model_a", "model_b"),
#'   seed_attempt = c(1L, 1L),
#'   seed_base = 1234L
#' )
get_model_seed <- function(
  model_id,
  seed_attempt = 1L,
  seed_base = 1234L
) {
  assertthat::assert_that(
    is.character(model_id),
    length(model_id) > 0L,
    all(!is.na(model_id)),
    all(nzchar(model_id)),
    msg = "`model_id` must contain non-empty character values."
  )
  assertthat::assert_that(
    is.numeric(seed_attempt),
    all(is.finite(seed_attempt)),
    all(seed_attempt > 0),
    all(seed_attempt %% 1 == 0),
    msg = "`seed_attempt` must contain positive integers."
  )
  assertthat::assert_that(
    is.numeric(seed_base),
    all(is.finite(seed_base)),
    all(seed_base >= 0),
    all(seed_base %% 1 == 0),
    msg = "`seed_base` must contain non-negative integers."
  )

  n_values <-
    max(
      length(model_id),
      length(seed_attempt),
      length(seed_base)
    )

  input_lengths <-
    c(
      length(model_id),
      length(seed_attempt),
      length(seed_base)
    )

  assertthat::assert_that(
    all(input_lengths %in% c(1L, n_values)),
    msg = "Seed inputs must have length one or the same length."
  )

  model_id <- rep(model_id, length.out = n_values)
  seed_attempt <- rep(seed_attempt, length.out = n_values)
  seed_base <- rep(seed_base, length.out = n_values)
  modulus <- 2147483646
  res_seed <- integer(n_values)

  for (
    value_index in seq_len(n_values)
  ) {
    hash_value <- seed_base[value_index] %% modulus
    hash_input <-
      stringr::str_c(
        model_id[value_index],
        seed_attempt[value_index],
        sep = "::"
      )

    for (
      code_point in utf8ToInt(hash_input)
    ) {
      hash_value <-
        (hash_value * 131 + code_point) %% modulus
    }

    res_seed[value_index] <-
      as.integer(hash_value + 1)
  }

  return(res_seed)
}
