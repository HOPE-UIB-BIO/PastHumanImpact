#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  render report -workflow
#
#
#                   O. Mottl, V. Felde
#                         2023
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

library(quarto)
library(rmarkdown)

#----------------------------------------------------------#
# 1. render -----
#----------------------------------------------------------#

rmarkdown::render(
  input = here::here(
    "R/reports/reporn_methods_workflow/report_methodology_workflow.qmd"
  ),
  output_file = here::here(
    "R/reports/reporn_methods_workflow/report_methodology_workflow.html"
  ),
  output_format = rmarkdown::html_document(
    self_contained = TRUE,
    fig_width = 10
  )
)

rmarkdown::render(
  input = here::here(
    "R/reports/reporn_methods_workflow/report_methodology_workflow.qmd"
  ),
  output_format = rmarkdown::pdf_document(
    fig_width = 10
  ),
  output_file = here::here(
    "R/reports/reporn_methods_workflow/report_methodology_workflow.pdf"
  )
)
