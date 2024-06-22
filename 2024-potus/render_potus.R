# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)

# set branch (dev or main)
branch <- "dev"

# import list of states to render
states <- 
  read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/main/data/static/electors.csv") %>%
  arrange(state) %>%
  pull(state)

# render function --------------------------------------------------------------

render_state <- function(branch, state) {
  
  # state-level files
  state_file <- glue::glue("2024-potus/{state}.qmd")
  
  # create the doc if it doesn't exist already
  if (!file.exists(state_file)) {
    file.copy(
      from = "2024-potus/_state_template.qmd",
      to = state_file
    )
  }
  
  # render!
  quarto::quarto_render(
    state_file,
    execute_params = list(branch = branch, state = state)
  )
  
}

# render all -------------------------------------------------------------------

# national has a unique layout
quarto::quarto_render(
  "2024-potus/National.qmd",
  execute_params = list(branch = branch)
)

# render all state pages
states %>%
  as.list() %>%
  walk(~render_state(branch = branch, state = .x))

