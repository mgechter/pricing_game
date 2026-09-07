# Runs once before the test files. testthat sets the working directory to this
# folder, so the project root is two levels up.
root <- normalizePath(file.path(getwd(), "..", ".."))

source(file.path(root, "R", "load_engine.R"))
source_engine(file.path(root, "R", "engine"))
source(file.path(root, "R", "parity.R"))
source(file.path(root, "R", "db.R"))
source(file.path(root, "R", "pairing.R"))
source(file.path(root, "R", "session_logic.R"))
source(file.path(root, "R", "export.R"))
source(file.path(root, "R", "debrief_data.R"))
source(file.path(root, "R", "debrief_plots.R"))

DATA_DIR   <- file.path(root, "zzz-2026-output", "pricing_game data")
GAMES_PATH <- file.path(root, "config", "games.yaml")
