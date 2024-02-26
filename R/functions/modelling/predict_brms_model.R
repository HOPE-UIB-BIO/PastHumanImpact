predict_brms_model <- function(mod) {
  data_predicted <-
    ggeffects::predict_response(
      model = mod,
      terms = "age",
      margin = "marginalmeans",
      back_transform = TRUE
    ) %>%
    as.data.frame() %>%
    dplyr::rename(
      age = x,
      value = predicted
    )

  # back transform if needed -----
  sel_family <-
    insight::get_family(mod)

  sel_link <-
    sel_family %>%
    purrr::pluck("link") %>%
    as.character()

  if (
    sel_link == "log"
  ) {
    data_predicted <-
      data_predicted %>%
      dplyr::mutate(
        value = exp(value),
        conf.low = exp(conf.low),
        conf.high = exp(conf.high)
      )
  }
  return(data_predicted)
}
