#' @title Run generalised additive models
#' @description Get all the variables interpolated at the same age levels
#' @param data Use all derived PAP estimates and predictor variables
#' @return For each sequence give new predicted data for all variables at the same age levels

# # tables with names of variables, errors, and dataframes
# vars_table <-
#   tibble::tibble(
#     var_name = c(
#       "n0",
#       "n1",
#       "n2",
#       "dcca_axis_1",
#       
#       "n2_divided_by_n1",
#       "n1_divided_by_n0",
#       "ROC",
#       "Peak"
#     ),
#     sel_error = c(
#       rep("mgcv::Tweedie(p = 1.1)", 4),
#       rep("mgcv::betar(link = 'logit')", 2),
#       "mgcv::Tweedie(p = 1.1)",
#       "stats::quasibinomial(link = 'logit')"
#     ),
#     sel_data = c(
#       rep("data_diversity", 6),
#       rep("data_roc", 2)
#     )
#   )
# 
# max_temporal_k <- 24
