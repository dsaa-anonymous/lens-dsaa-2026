# Load packages required by the first two reproducibility scripts.
# Dependency management should be handled with renv, not install.packages()
# inside analysis scripts.

required_packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "tibble",
  "janitor",
  "broom",
  "fixest",
  "modelsummary",
  "clubSandwich",
  "writexl",
  "ggplot2"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_packages, collapse = ", "), "\n",
    "Install them with renv::restore() if renv.lock exists, or run:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(janitor)
  library(broom)
  library(fixest)
  library(modelsummary)
  library(clubSandwich)
  library(writexl)
})
