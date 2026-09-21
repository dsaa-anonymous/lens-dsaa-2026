# 04b_sem_power_analysis.R
# Purpose: evaluate sample-size adequacy for the primary LENS SEM
# using an RMSEA-based global model-fit power analysis.
#
# Required prior step:
#   Rscript scripts/04_sem_mechanism_model.R
#
# Main inputs:
#   outputs/sem_models/sem_primary_fit.csv
#   outputs/sem_models/sem_analysis_dataset_summary.csv
#
# Main outputs:
#   outputs/sem_models/sem_global_fit_power.csv
#   outputs/sem_models/sem_global_fit_power_sensitivity.csv

source(file.path("R", "00_packages.R"))
source(file.path("R", "00_paths.R"))
source(file.path("R", "utils_io.R"))

if (!exists("OUT_SEM", inherits = FALSE)) {
  OUT_SEM <- file.path(PROJECT_ROOT, "outputs", "sem_models")
}

dir.create(OUT_SEM, recursive = TRUE, showWarnings = FALSE)

message("Project root: ", PROJECT_ROOT)
message("Reading SEM outputs from: ", OUT_SEM)

# -------------------------------------------------------------------------
# Input files produced by Script 04
# -------------------------------------------------------------------------

primary_fit_path <- file.path(
  OUT_SEM,
  "sem_primary_fit.csv"
)

analysis_summary_path <- file.path(
  OUT_SEM,
  "sem_analysis_dataset_summary.csv"
)

if (!file.exists(primary_fit_path)) {
  stop(
    "Could not find sem_primary_fit.csv. Run first:\n",
    "Rscript scripts/04_sem_mechanism_model.R",
    call. = FALSE
  )
}

if (!file.exists(analysis_summary_path)) {
  stop(
    "Could not find sem_analysis_dataset_summary.csv. Run first:\n",
    "Rscript scripts/04_sem_mechanism_model.R",
    call. = FALSE
  )
}

primary_fit <- readr::read_csv(
  primary_fit_path,
  show_col_types = FALSE
)

analysis_summary <- readr::read_csv(
  analysis_summary_path,
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Validate primary SEM output
# -------------------------------------------------------------------------

if (nrow(primary_fit) != 1) {
  stop(
    "Expected exactly one row in sem_primary_fit.csv.",
    call. = FALSE
  )
}

if (!"status" %in% names(primary_fit) ||
    primary_fit$status[[1]] != "ok") {
  stop(
    "Primary SEM was not successfully estimated.",
    call. = FALSE
  )
}

if (!all(c("df", "n_students") %in%
         c(names(primary_fit), names(analysis_summary)))) {
  stop(
    "Required df or sample-size information is missing.",
    call. = FALSE
  )
}

model_df <- as.numeric(primary_fit$df[[1]])
available_n <- as.integer(analysis_summary$n_students[[1]])

if (!is.finite(model_df) || model_df <= 0) {
  stop("Invalid primary SEM degrees of freedom.", call. = FALSE)
}

if (!is.finite(available_n) || available_n <= 0) {
  stop("Invalid SEM sample size.", call. = FALSE)
}

message("Primary SEM df: ", model_df)
message("Available SEM sample size: ", available_n)

# -------------------------------------------------------------------------
# RMSEA-based global-fit power
#
# Noncentral chi-square approach following the conventional
# close-fit framework:
#
# H0: RMSEA = 0.05
# H1: RMSEA = 0.08
#
# alpha = .05
# target power = .80
#
# This evaluates sensitivity to global model misfit. It is NOT a
# power calculation for each individual SEM path.
# -------------------------------------------------------------------------

rmsea_power <- function(
    n,
    df,
    rmsea_null = 0.05,
    rmsea_alt = 0.08,
    alpha = 0.05) {

  if (n <= 1) return(NA_real_)

  lambda_null <- (n - 1) * df * rmsea_null^2
  lambda_alt  <- (n - 1) * df * rmsea_alt^2

  critical_value <- stats::qchisq(
    p = 1 - alpha,
    df = df,
    ncp = lambda_null
  )

  power <- 1 - stats::pchisq(
    q = critical_value,
    df = df,
    ncp = lambda_alt
  )

  as.numeric(power)
}

minimum_n_for_power <- function(
    df,
    rmsea_null,
    rmsea_alt,
    alpha = 0.05,
    target_power = 0.80,
    min_n = 50,
    max_n = 100000) {

  candidate_n <- seq.int(min_n, max_n)

  candidate_power <- vapply(
    candidate_n,
    rmsea_power,
    numeric(1),
    df = df,
    rmsea_null = rmsea_null,
    rmsea_alt = rmsea_alt,
    alpha = alpha
  )

  qualifying <- which(candidate_power >= target_power)

  if (length(qualifying) == 0) {
    return(NA_integer_)
  }

  candidate_n[min(qualifying)]
}

# -------------------------------------------------------------------------
# Primary, literature-standard close-fit analysis
# -------------------------------------------------------------------------

alpha <- 0.05
target_power <- 0.80
rmsea_null <- 0.05
rmsea_alt <- 0.08

required_n <- minimum_n_for_power(
  df = model_df,
  rmsea_null = rmsea_null,
  rmsea_alt = rmsea_alt,
  alpha = alpha,
  target_power = target_power
)

achieved_power <- rmsea_power(
  n = available_n,
  df = model_df,
  rmsea_null = rmsea_null,
  rmsea_alt = rmsea_alt,
  alpha = alpha
)

primary_power_summary <- tibble::tibble(
  analysis = "RMSEA global model-fit power",
  model = primary_fit$model[[1]],
  df = model_df,
  alpha = alpha,
  rmsea_null = rmsea_null,
  rmsea_alternative = rmsea_alt,
  target_power = target_power,
  required_n = required_n,
  available_n = available_n,
  achieved_power = achieved_power
)

readr::write_csv(
  primary_power_summary,
  file.path(OUT_SEM, "sem_global_fit_power.csv")
)

message(
  "Wrote: ",
  file.path(OUT_SEM, "sem_global_fit_power.csv")
)

# -------------------------------------------------------------------------
# Internal sensitivity analysis
#
# This is useful for checking how conclusions depend on the size of
# the RMSEA difference considered substantively detectable.
#
# The .05 -> .08 comparison remains the prespecified primary
# close-fit analysis.
# -------------------------------------------------------------------------

sensitivity_grid <- tibble::tibble(
  rmsea_null = 0.05,
  rmsea_alternative = c(0.06, 0.07, 0.08)
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    df = model_df,
    alpha = alpha,
    target_power = target_power,
    required_n = minimum_n_for_power(
      df = model_df,
      rmsea_null = rmsea_null,
      rmsea_alt = rmsea_alternative,
      alpha = alpha,
      target_power = target_power
    ),
    available_n = available_n,
    achieved_power = rmsea_power(
      n = available_n,
      df = model_df,
      rmsea_null = rmsea_null,
      rmsea_alt = rmsea_alternative,
      alpha = alpha
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    df,
    alpha,
    rmsea_null,
    rmsea_alternative,
    target_power,
    required_n,
    available_n,
    achieved_power
  )

readr::write_csv(
  sensitivity_grid,
  file.path(
    OUT_SEM,
    "sem_global_fit_power_sensitivity.csv"
  )
)

message(
  "Wrote: ",
  file.path(
    OUT_SEM,
    "sem_global_fit_power_sensitivity.csv"
  )
)

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

message("")
message("Primary RMSEA global-fit power analysis:")
print(primary_power_summary)

message("")
message("RMSEA sensitivity analysis:")
print(sensitivity_grid)

message("")
message("Important interpretation:")
message(
  "This analysis evaluates power for detecting global SEM misfit. ",
  "It does not establish power for each individual structural or ",
  "indirect path, and it does not explicitly model the 19-course ",
  "cluster structure."
)

message("")
message("04b_sem_power_analysis.R completed successfully.")