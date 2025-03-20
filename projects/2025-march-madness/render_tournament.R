# setup ------------------------------------------------------------------------

leagues <- c("mens")

for (league in leagues) {
  
  file.copy(
    from = "projects/2025-march-madness/_template.qmd",
    to = glue::glue("projects/2025-march-madness/{league}.qmd"),
    overwrite = TRUE
  )
  
  quarto::quarto_render(
    input = glue::glue("projects/2025-march-madness/{league}.qmd"),
    execute_params = list(league = league)
  )
  
}


