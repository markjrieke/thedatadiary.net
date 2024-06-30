# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)

# set branch (dev or main)
branch <- "main"

# import list of states to render
states <- 
  read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/main/data/static/electors.csv") %>%
  arrange(state) %>%
  pull(state)

# setup for rendering ----------------------------------------------------------

for (state in states) {
  
  file.copy(
    from = "2024-potus/_state_template.qmd",
    to = glue::glue("2024-potus/{state}.qmd"),
    overwrite = TRUE
  )
  
}

# render all -------------------------------------------------------------------

# national has a unique layout
quarto::quarto_render(
  "2024-potus/National.qmd",
  execute_params = list(branch = branch)
)

# render all state pages
for (state in states) {
  
  quarto::quarto_render(
    glue::glue("2024-potus/{state}.qmd"),
    execute_params = list(branch = branch, state = state)
  )
  
}

