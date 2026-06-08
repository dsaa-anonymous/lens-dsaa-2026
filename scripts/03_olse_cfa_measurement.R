#!/usr/bin/env Rscript

# 03_olse_cfa_measurement.R
# Purpose: reproduce the RQ2 OLSE reliability, ordinal CFA, measurement
# comparability, and SEM-ready OLSE scoring outputs.
#
# Input:
#   data/processed/student_term_fe_analysis.csv
#
# Main outputs:
#   outputs/olse_measurement/olse_item_missingness.csv
#   outputs/olse_measurement/olse_item_distribution_overall.csv
#   outputs/olse_measurement/olse_item_distribution_by_status.csv
#   outputs/olse_measurement/olse_domain_summary_by_status.csv
#   outputs/olse_measurement/olse_reliability_alpha.csv
#   outputs/olse_measurement/cfa_5factor_fit.csv
#   outputs/olse_measurement/cfa_5factor_standardized_loadings.csv
#   outputs/olse_measurement/cfa_higher_order_fit.csv
#   outputs/olse_measurement/cfa_higher_order_standardized_loadings.csv
#   outputs/olse_measurement/olse_pre_post_measurement_comparability.csv
#   outputs/olse_measurement/olse_single_factor_cfa_fit.csv
#   outputs/olse_measurement/olse_single_factor_cfa_loadings.csv
#   outputs/olse_measurement/olse_cfa_fit_comparison.csv
#   outputs/olse_measurement/olse_five_factor_latent_correlations.csv
#   data/processed/olse_scored_for_sem.csv

source(file.path("R", "00_packages.R"))
source(file.path("R", "00_paths.R"))
source(file.path("R", "utils_io.R"))

# -------------------------------------------------------------------------
# Additional package checks for OLSE/CFA workflow
# -------------------------------------------------------------------------

extra_packages <- c("lavaan", "semTools", "psych")
missing_extra <- extra_packages[!vapply(extra_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_extra) > 0) {
  stop(
    "Missing required R packages for Script 03: ", paste(missing_extra, collapse = ", "), "\n",
    "Install them with renv::restore() if renv.lock exists, or run:\n",
    "install.packages(c(", paste(sprintf('"%s"', missing_extra), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(lavaan)
  library(semTools)
  library(psych)
})

# OUT_OLSE may not exist if you are using the first-two starter paths file.
if (!exists("OUT_OLSE", inherits = FALSE)) {
  OUT_OLSE <- file.path(PROJECT_ROOT, "outputs", "olse_measurement")
}

dir.create(OUT_OLSE, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_PROCESSED, recursive = TRUE, showWarnings = FALSE)

message("Project root: ", PROJECT_ROOT)
message("Reading processed data from: ", DATA_PROCESSED)
message("Writing OLSE/CFA outputs to: ", OUT_OLSE)

# -------------------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------------------

mean_if_enough <- function(df, cols, min_prop = 0.50) {
  vals <- df[, cols, drop = FALSE]
  vals <- dplyr::mutate(vals, dplyr::across(dplyr::everything(), as.numeric))
  observed <- rowSums(!is.na(vals))
  required <- ceiling(length(cols) * min_prop)
  out <- rowMeans(vals, na.rm = TRUE)
  out[observed < required] <- NA_real_
  out
}

alpha_safe <- function(data, items) {
  x <- data |>
    dplyr::select(dplyr::all_of(items)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  tryCatch(
    psych::alpha(x, warnings = FALSE)$total$raw_alpha,
    error = function(e) NA_real_
  )
}

extract_fit_long <- function(fit, model_name) {
  if (inherits(fit, "error")) {
    return(tibble::tibble(
      model = model_name,
      status = "error",
      fit_index = NA_character_,
      value = NA_real_,
      error = fit$message
    ))
  }

  lavaan::fitMeasures(
    fit,
    c(
      "chisq.scaled",
      "df.scaled",
      "pvalue.scaled",
      "cfi.scaled",
      "tli.scaled",
      "rmsea.scaled",
      "srmr"
    )
  ) |>
    tibble::enframe(name = "fit_index", value = "value") |>
    dplyr::mutate(
      model = model_name,
      status = "ok",
      error = NA_character_,
      .before = 1
    )
}

extract_fit_wide <- function(fit, model_name) {
  if (inherits(fit, "error")) {
    return(tibble::tibble(
      model = model_name,
      status = "error",
      error = fit$message,
      chisq.scaled = NA_real_,
      df.scaled = NA_real_,
      pvalue.scaled = NA_real_,
      cfi.scaled = NA_real_,
      tli.scaled = NA_real_,
      rmsea.scaled = NA_real_,
      srmr = NA_real_
    ))
  }

  vals <- lavaan::fitMeasures(
    fit,
    c(
      "chisq.scaled",
      "df.scaled",
      "pvalue.scaled",
      "cfi.scaled",
      "tli.scaled",
      "rmsea.scaled",
      "srmr"
    )
  )

  tibble::tibble(
    model = model_name,
    status = "ok",
    error = NA_character_,
    chisq.scaled = unname(vals["chisq.scaled"]),
    df.scaled = unname(vals["df.scaled"]),
    pvalue.scaled = unname(vals["pvalue.scaled"]),
    cfi.scaled = unname(vals["cfi.scaled"]),
    tli.scaled = unname(vals["tli.scaled"]),
    rmsea.scaled = unname(vals["rmsea.scaled"]),
    srmr = unname(vals["srmr"])
  )
}

standardized_loadings_safe <- function(fit) {
  if (inherits(fit, "error")) return(tibble::tibble())

  lavaan::standardizedSolution(fit) |>
    tibble::as_tibble() |>
    dplyr::filter(op == "=~") |>
    dplyr::arrange(lhs, rhs)
}

# -------------------------------------------------------------------------
# Read processed student-course data
# -------------------------------------------------------------------------

student_file <- file.path(DATA_PROCESSED, "student_term_fe_analysis.csv")

if (!file.exists(student_file)) {
  stop(
    "Missing processed file: ", student_file, "\n",
    "Run first: Rscript scripts/01_prepare_data.R",
    call. = FALSE
  )
}

student <- readr::read_csv(student_file, show_col_types = FALSE) |>
  janitor::clean_names()

q1_items <- paste0("q1_", 1:8)
q2_items <- paste0("q2_", 1:5)
q3_items <- paste0("q3_", 1:6)
q4_items <- paste0("q4_", 1:5)
q5_items <- paste0("q5_", 1:6)
olse_items <- c(q1_items, q2_items, q3_items, q4_items, q5_items)

missing_items <- setdiff(olse_items, names(student))
if (length(missing_items) > 0) {
  stop("Missing OLSE item columns: ", paste(missing_items, collapse = ", "), call. = FALSE)
}

student <- student |>
  dplyr::mutate(
    post_redesign = as.integer(post_redesign),
    redesign_status = factor(
      dplyr::if_else(post_redesign == 1, "post", "pre"),
      levels = c("pre", "post")
    ),
    final_numeric_grade = as.numeric(final_numeric_grade)
  ) |>
  dplyr::mutate(dplyr::across(dplyr::all_of(olse_items), ~ suppressWarnings(as.numeric(.x))))

message("Rows: ", nrow(student))
message("Courses: ", dplyr::n_distinct(student$course_id))
message("Terms: ", dplyr::n_distinct(student$term))
message("OLSE items: ", length(olse_items))

# -------------------------------------------------------------------------
# Item missingness and response distributions
# -------------------------------------------------------------------------

item_missingness <- student |>
  dplyr::summarise(dplyr::across(dplyr::all_of(olse_items), ~ mean(is.na(.x)))) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "item", values_to = "missing_rate") |>
  dplyr::arrange(dplyr::desc(missing_rate))

item_distribution <- student |>
  dplyr::select(dplyr::all_of(olse_items)) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "item", values_to = "response") |>
  dplyr::filter(!is.na(response)) |>
  dplyr::count(item, response, name = "n") |>
  dplyr::group_by(item) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::ungroup()

item_distribution_by_status <- student |>
  dplyr::select(redesign_status, dplyr::all_of(olse_items)) |>
  tidyr::pivot_longer(dplyr::all_of(olse_items), names_to = "item", values_to = "response") |>
  dplyr::filter(!is.na(response)) |>
  dplyr::count(redesign_status, item, response, name = "n") |>
  dplyr::group_by(redesign_status, item) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::ungroup()

safe_write_csv(item_missingness, file.path(OUT_OLSE, "olse_item_missingness.csv"))
safe_write_csv(item_distribution, file.path(OUT_OLSE, "olse_item_distribution_overall.csv"))
safe_write_csv(item_distribution_by_status, file.path(OUT_OLSE, "olse_item_distribution_by_status.csv"))

# Also keep the exact category-count filenames used by the earlier notebook.
safe_write_csv(item_distribution, file.path(OUT_OLSE, "olse_item_category_counts.csv"))
safe_write_csv(item_distribution_by_status, file.path(OUT_OLSE, "olse_item_category_counts_by_status.csv"))

# -------------------------------------------------------------------------
# Domain means/parcels and standardized scores
# -------------------------------------------------------------------------

student$olse_q1_mean <- mean_if_enough(student, q1_items)
student$olse_q2_mean <- mean_if_enough(student, q2_items)
student$olse_q3_mean <- mean_if_enough(student, q3_items)
student$olse_q4_mean <- mean_if_enough(student, q4_items)
student$olse_q5_mean <- mean_if_enough(student, q5_items)
student$olse_overall_mean <- mean_if_enough(student, olse_items)

student <- student |>
  dplyr::mutate(
    olse_q1_mean_z = safe_scale(olse_q1_mean),
    olse_q2_mean_z = safe_scale(olse_q2_mean),
    olse_q3_mean_z = safe_scale(olse_q3_mean),
    olse_q4_mean_z = safe_scale(olse_q4_mean),
    olse_q5_mean_z = safe_scale(olse_q5_mean),
    olse_overall_z = safe_scale(olse_overall_mean),
    final_numeric_grade_z = safe_scale(final_numeric_grade)
  )

domain_summary <- student |>
  dplyr::group_by(redesign_status) |>
  dplyr::summarise(
    n = dplyr::n(),
    dplyr::across(
      c(
        olse_q1_mean,
        olse_q2_mean,
        olse_q3_mean,
        olse_q4_mean,
        olse_q5_mean,
        olse_overall_mean
      ),
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ stats::sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

safe_write_csv(domain_summary, file.path(OUT_OLSE, "olse_domain_summary_by_status.csv"))

# -------------------------------------------------------------------------
# Reliability
# -------------------------------------------------------------------------

reliability <- tibble::tibble(
  scale = c("Q1", "Q2", "Q3", "Q4", "Q5", "Overall_30_item"),
  n_items = c(length(q1_items), length(q2_items), length(q3_items), length(q4_items), length(q5_items), length(olse_items)),
  alpha = c(
    alpha_safe(student, q1_items),
    alpha_safe(student, q2_items),
    alpha_safe(student, q3_items),
    alpha_safe(student, q4_items),
    alpha_safe(student, q5_items),
    alpha_safe(student, olse_items)
  )
)

safe_write_csv(reliability, file.path(OUT_OLSE, "olse_reliability_alpha.csv"))

# -------------------------------------------------------------------------
# lavaan model syntax
# -------------------------------------------------------------------------

five_factor_model <- paste0(
  "Q1 =~ ", paste(q1_items, collapse = " + "), "\n",
  "Q2 =~ ", paste(q2_items, collapse = " + "), "\n",
  "Q3 =~ ", paste(q3_items, collapse = " + "), "\n",
  "Q4 =~ ", paste(q4_items, collapse = " + "), "\n",
  "Q5 =~ ", paste(q5_items, collapse = " + ")
)

higher_order_model <- paste0(
  five_factor_model, "\n",
  "Overall_OLSE =~ Q1 + Q2 + Q3 + Q4 + Q5"
)

single_factor_model <- paste0(
  "OLSE =~ ", paste(olse_items, collapse = " + ")
)

# -------------------------------------------------------------------------
# Five-factor ordinal CFA
# -------------------------------------------------------------------------

message("Fitting five-factor ordinal CFA.")

fit_5factor <- tryCatch(
  lavaan::cfa(
    five_factor_model,
    data = student,
    ordered = olse_items,
    estimator = "WLSMV",
    parameterization = "theta",
    std.lv = TRUE,
    missing = "pairwise"
  ),
  error = function(e) e
)

fit_5factor_measures <- extract_fit_long(fit_5factor, "five_factor_ordinal_cfa")
fit_5factor_loadings <- standardized_loadings_safe(fit_5factor)

safe_write_csv(fit_5factor_measures, file.path(OUT_OLSE, "cfa_5factor_fit.csv"))
safe_write_csv(fit_5factor_loadings, file.path(OUT_OLSE, "cfa_5factor_standardized_loadings.csv"))

# -------------------------------------------------------------------------
# Higher-order ordinal CFA
# -------------------------------------------------------------------------

message("Fitting higher-order ordinal CFA.")

fit_higher <- tryCatch(
  lavaan::cfa(
    higher_order_model,
    data = student,
    ordered = olse_items,
    estimator = "WLSMV",
    parameterization = "theta",
    std.lv = TRUE,
    missing = "pairwise"
  ),
  error = function(e) e
)

fit_higher_measures <- extract_fit_long(fit_higher, "higher_order_ordinal_cfa")
fit_higher_loadings <- standardized_loadings_safe(fit_higher)

safe_write_csv(fit_higher_measures, file.path(OUT_OLSE, "cfa_higher_order_fit.csv"))
safe_write_csv(fit_higher_loadings, file.path(OUT_OLSE, "cfa_higher_order_standardized_loadings.csv"))

# -------------------------------------------------------------------------
# Pre/post measurement comparability checks
# -------------------------------------------------------------------------

message("Fitting pre/post measurement comparability CFA models.")

fit_group_configural <- tryCatch(
  lavaan::cfa(
    five_factor_model,
    data = student,
    group = "redesign_status",
    ordered = olse_items,
    estimator = "WLSMV",
    parameterization = "theta",
    std.lv = TRUE,
    missing = "pairwise"
  ),
  error = function(e) e
)

fit_group_metric <- tryCatch(
  lavaan::cfa(
    five_factor_model,
    data = student,
    group = "redesign_status",
    group.equal = c("loadings"),
    ordered = olse_items,
    estimator = "WLSMV",
    parameterization = "theta",
    std.lv = TRUE,
    missing = "pairwise"
  ),
  error = function(e) e
)

fit_group_threshold <- tryCatch(
  lavaan::cfa(
    five_factor_model,
    data = student,
    group = "redesign_status",
    group.equal = c("loadings", "thresholds"),
    ordered = olse_items,
    estimator = "WLSMV",
    parameterization = "theta",
    std.lv = TRUE,
    missing = "pairwise"
  ),
  error = function(e) e
)

invariance_fit <- dplyr::bind_rows(
  extract_fit_wide(fit_group_configural, "configural_pre_post"),
  extract_fit_wide(fit_group_metric, "metric_loadings_equal"),
  extract_fit_wide(fit_group_threshold, "thresholds_and_loadings_equal")
)

configural_cfi <- invariance_fit |>
  dplyr::filter(model == "configural_pre_post") |>
  dplyr::pull(cfi.scaled) |>
  dplyr::first()

configural_rmsea <- invariance_fit |>
  dplyr::filter(model == "configural_pre_post") |>
  dplyr::pull(rmsea.scaled) |>
  dplyr::first()

invariance_fit <- invariance_fit |>
  dplyr::mutate(
    delta_cfi_from_configural = dplyr::if_else(
      status == "ok" & !is.na(cfi.scaled) & !is.na(configural_cfi),
      cfi.scaled - configural_cfi,
      NA_real_
    ),
    delta_rmsea_from_configural = dplyr::if_else(
      status == "ok" & !is.na(rmsea.scaled) & !is.na(configural_rmsea),
      rmsea.scaled - configural_rmsea,
      NA_real_
    )
  )

safe_write_csv(invariance_fit, file.path(OUT_OLSE, "olse_pre_post_measurement_comparability.csv"))

# -------------------------------------------------------------------------
# Supplemental robustness check: single-factor OLSE CFA
# -------------------------------------------------------------------------

message("Fitting single-factor ordinal CFA.")

fit_single_factor <- tryCatch(
  lavaan::cfa(
    model = single_factor_model,
    data = student,
    ordered = olse_items,
    estimator = "WLSMV",
    parameterization = "theta",
    std.lv = TRUE,
    missing = "pairwise"
  ),
  error = function(e) e
)

single_factor_fit <- extract_fit_long(fit_single_factor, "single_factor_ordinal_cfa")

single_factor_loadings <- if (inherits(fit_single_factor, "error")) {
  tibble::tibble()
} else {
  lavaan::parameterEstimates(fit_single_factor, standardized = TRUE) |>
    tibble::as_tibble() |>
    dplyr::filter(op == "=~") |>
    dplyr::select(
      factor = lhs,
      item = rhs,
      estimate = est,
      se,
      z,
      pvalue,
      std_loading = std.all
    )
}

safe_write_csv(single_factor_fit, file.path(OUT_OLSE, "olse_single_factor_cfa_fit.csv"))
safe_write_csv(single_factor_loadings, file.path(OUT_OLSE, "olse_single_factor_cfa_loadings.csv"))

# -------------------------------------------------------------------------
# CFA comparison and latent correlations
# -------------------------------------------------------------------------

cfa_fit_comparison <- dplyr::bind_rows(
  extract_fit_long(fit_5factor, "Five-domain correlated CFA"),
  extract_fit_long(fit_higher, "Higher-order CFA"),
  extract_fit_long(fit_single_factor, "Single-factor CFA")
)

safe_write_csv(cfa_fit_comparison, file.path(OUT_OLSE, "olse_cfa_fit_comparison.csv"))

if (!inherits(fit_5factor, "error")) {
  latent_cor <- lavaan::lavInspect(fit_5factor, "cor.lv") |>
    as.data.frame() |>
    tibble::rownames_to_column("factor")

  safe_write_csv(latent_cor, file.path(OUT_OLSE, "olse_five_factor_latent_correlations.csv"))
}

# -------------------------------------------------------------------------
# SEM-ready OLSE score/parcel dataset
# -------------------------------------------------------------------------

message("Writing SEM-ready OLSE scored file.")

base_scored_cols <- c(
  "student_id",
  "course_id",
  "term",
  "section_id",
  "instructor_id",
  "switch_term",
  "term_index",
  "course_term_id",
  "section_term_id",
  "post_redesign",
  "redesign_status",
  "final_numeric_grade",
  "final_numeric_grade_z",
  "olse_q1_mean",
  "olse_q2_mean",
  "olse_q3_mean",
  "olse_q4_mean",
  "olse_q5_mean",
  "olse_q1_mean_z",
  "olse_q2_mean_z",
  "olse_q3_mean_z",
  "olse_q4_mean_z",
  "olse_q5_mean_z",
  "olse_overall_mean",
  "olse_overall_z",
  "mastery_redesign",
  "social_vicarious_redesign",
  "structural_compliance_redesign",
  "redesign_intensity",
  "mastery_redesign_z",
  "social_vicarious_redesign_z",
  "structural_compliance_redesign_z",
  "redesign_intensity_z",
  "post_x_mastery_redesign_z",
  "post_x_social_vicarious_redesign_z",
  "post_x_structural_compliance_redesign_z",
  "post_x_redesign_intensity_z"
)

scored <- student |>
  dplyr::select(dplyr::any_of(base_scored_cols))

if (!("course_term_id" %in% names(scored))) {
  scored <- scored |>
    dplyr::mutate(course_term_id = paste(course_id, term, sep = "__"))
}

if (!inherits(fit_higher, "error")) {
  fs <- tryCatch(
    lavaan::lavPredict(fit_higher, method = "regression") |>
      as.data.frame(),
    error = function(e) NULL
  )

  if (!is.null(fs)) {
    names(fs) <- paste0("fs_", names(fs))
    scored <- dplyr::bind_cols(scored, fs)
  }
}

safe_write_csv(scored, file.path(DATA_PROCESSED, "olse_scored_for_sem.csv"))
safe_write_csv(scored, file.path(OUT_OLSE, "olse_scored_for_sem.csv"))

# Backward-compatible filename from the original notebook.
safe_write_csv(scored, file.path(OUT_OLSE, "project_soar_olse_scored_for_sem_v4.csv"))

# Optional Excel workbook for convenience.
if (requireNamespace("writexl", quietly = TRUE)) {
  try(
    writexl::write_xlsx(
      list(
        olse_scored_for_sem = scored,
        reliability = reliability,
        domain_summary = domain_summary,
        five_factor_fit = fit_5factor_measures,
        higher_order_fit = fit_higher_measures,
        comparability = invariance_fit,
        cfa_comparison = cfa_fit_comparison
      ),
      file.path(OUT_OLSE, "project_soar_olse_measurement_outputs_v4.xlsx")
    ),
    silent = TRUE
  )
}

message("")
message("Reliability summary:")
print(reliability)

message("")
message("CFA fit comparison:")
print(cfa_fit_comparison)

message("")
message("03_olse_cfa_measurement.R completed successfully.")
message("OLSE/CFA outputs written to: ", OUT_OLSE)
message("SEM-ready file written to: ", file.path(DATA_PROCESSED, "olse_scored_for_sem.csv"))
