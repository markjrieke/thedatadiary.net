render_child <- function(option, league, date) {

  # read in template markdown  
  daily_text <- xfun::read_utf8("_daily.qmd")
  
  # environment to pass to knitr
  daily_envir <-
    rlang::env(
      option = option,
      league = league,
      date = date
    )
  
  # render as a child of the overall doc
  res <-
    knitr::knit_child(
      text = daily_text,
      envir = daily_envir,
      quiet = TRUE
    )
  
  # print out for rendering
  cat(res, sep = '\n')
  cat("\n")
  
}



