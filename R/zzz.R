.onLoad <- function(libname, pkgname) {
  # Ensure magrittr is loaded
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package 'magrittr' is needed but not installed.")
  } else {
    # Attach magrittr to make %>% available
    library(magrittr)
  }

  library(shiny)
  options(shiny.maxRequestSize=150*1024^2)

  # Set default package options
  #default_opts <- list(reverse.legend.color = FALSE)
  #current_opts <- getOption("mypackage.options", list())
  #new_opts <- modifyList(default_opts, current_opts)
  #options(mypackage.options = new_opts)

  # Display a message indicating that the package has been loaded
  message("surveyPrevRShiny loaded successfully.")
  message("Please run 'surveyPrevRShiny::run_app()' to start the app.")

}
