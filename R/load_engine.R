# Source the whole engine into the calling environment.
# Usage from the project root:  source("R/load_engine.R"); source_engine()

source_engine <- function(root = "R/engine") {
  files <- c("utils.R", "demand.R", "payoff.R", "resolve.R",
             "equilibria.R", "load_games.R")
  for (f in files) source(file.path(root, f))
  invisible(TRUE)
}
