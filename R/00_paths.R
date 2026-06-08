# Project-relative paths. These scripts assume they are run from the repository
# root, but this file also tries to recover if called from scripts/.
#
# Optional environment variables allow notebooks or CI jobs to redirect generated
# artifacts without changing the canonical script defaults:
#   LENS_DATA_RAW_DIR
#   LENS_DATA_PROCESSED_DIR
#   LENS_OUTPUT_DIR
#   LENS_FIGURES_DIR

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    has_repo_markers <- file.exists(file.path(current, "data")) &&
      file.exists(file.path(current, "scripts")) &&
      file.exists(file.path(current, "R"))

    if (has_repo_markers) return(current)

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Could not find project root. Run scripts from the repo root, e.g. Rscript scripts/01_prepare_data.R",
        call. = FALSE
      )
    }
    current <- parent
  }
}

if (!exists("PROJECT_ROOT", inherits = FALSE)) {
  PROJECT_ROOT <- find_project_root(getwd())
}

path_project <- function(...) file.path(PROJECT_ROOT, ...)

is_absolute_path <- function(path) {
  grepl("^/", path) || grepl("^[A-Za-z]:[/\\\\]", path)
}

env_path <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  if (is_absolute_path(value)) {
    normalizePath(value, winslash = "/", mustWork = FALSE)
  } else {
    path_project(value)
  }
}

DATA_RAW <- env_path("LENS_DATA_RAW_DIR", path_project("data", "raw_public"))
DATA_PROCESSED <- env_path("LENS_DATA_PROCESSED_DIR", path_project("data", "processed"))
OUTPUT_ROOT <- env_path("LENS_OUTPUT_DIR", path_project("outputs"))
FIGURES_DIR <- env_path("LENS_FIGURES_DIR", path_project("figures"))

OUT_FE <- file.path(OUTPUT_ROOT, "fe_models")
OUT_OLSE <- file.path(OUTPUT_ROOT, "olse_measurement")
OUT_SEM <- file.path(OUTPUT_ROOT, "sem_models")
OUT_SEM_EXPLORATORY <- file.path(OUT_SEM, "exploratory")
OUT_TABLES <- file.path(OUTPUT_ROOT, "paper_tables")
OUT_FIGURES <- FIGURES_DIR

required_dirs <- c(
  DATA_RAW,
  DATA_PROCESSED,
  OUT_FE,
  OUT_OLSE,
  OUT_SEM,
  OUT_SEM_EXPLORATORY,
  OUT_TABLES,
  OUT_FIGURES
)
invisible(lapply(required_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
