#!/usr/bin/env Rscript

# 04c_baseline_ablation_comparisons.R
# Purpose: reproduce simple baseline/ablation comparisons requested for the
# LENS DSAA 2026 camera-ready paper.
#
# The analyses isolate what is gained by progressively linking additional
# information layers:
#   1) Simple pre/post grade comparison (no fixed effects)
#   2) Course + term fixed-effects grade model
#   3) Survey-only OLSE -> grade model (no redesign features)
#   4) Redesign-only grade model (no OLSE)
#   5) Full LENS SEM reference (redesign -> OLSE -> grade)
#
# Required prior steps:
#   Rscript scripts/01_prepare_data.R
#   Rscript scripts/03_olse_cfa_measurement.R
#   Rscript scripts/04_sem_mechanism_model.R
#
# Script 02 is not required because the FE/redesign-only models are refit here
# using the same specifications. If Script 02 outputs exist, the resulting
# estimates should match them.
#
# Main inputs:
#   data/processed/student_term_fe_analysis.csv
#   data/processed/olse_scored_for_sem.csv
#   outputs/sem_models/sem_primary_fit.csv
#   outputs/sem_models/sem_primary_params.csv
#
# Main outputs:
#   outputs/baseline_comparisons/baseline_prepost_grade.csv
#   outputs/baseline_comparisons/baseline_prepost_descriptives.csv
#   outputs/baseline_comparisons/baseline_fixed_effects_reference.csv
#   outputs/baseline_comparisons/baseline_survey_only.csv
#   outputs/baseline_comparisons/baseline_redesign_only.csv
#   outputs/baseline_comparisons/full_lens_sem_reference.csv
#   outputs/baseline_comparisons/lens_baseline_ablation_summary.csv

source(file.path("R", "00_packages.R"))
source(file.path("R", "00_paths.R"))
source(file.path("R", "utils_io.R"))

OUT_BASELINE <- file.path(OUTPUT_ROOT, "baseline_comparisons")
dir.create(OUT_BASELINE, recursive = TRUE, showWarnings = FALSE)

message("Project root: ", PROJECT_ROOT)
message("Reading processed data from: ", DATA_PROCESSED)
message("Writing baseline outputs to: ", OUT_BASELINE)

# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    ifelse(x < 0.001, "<.001", sub("^0", "", formatC(x, format = "f", digits = 3)))
  )
}

extract_fixest_term <- function(model, term_name, analysis_label, effect_scale) {
  out <- broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == term_name)

  if (nrow(out) != 1) {
    stop(
      "Expected exactly one coefficient for term '", term_name,
      "' in analysis '", analysis_label, "'.",
      call. = FALSE
    )
  }

  out |>
    dplyr::transmute(
      analysis = analysis_label,
      term = term,
      estimate = estimate,
      std_error = std.error,
      statistic = statistic,
      p_value = p.value,
      conf_low = conf.low,
      conf_high = conf.high,
      n = stats::nobs(model),
      effect_scale = effect_scale
    )
}

# -------------------------------------------------------------------------
# Load common student-course analysis data
# -------------------------------------------------------------------------

student_term_path <- file.path(DATA_PROCESSED, "student_term_fe_analysis.csv")

if (!file.exists(student_term_path)) {
  stop(
    "Could not find student_term_fe_analysis.csv. Run first:\n",
    "Rscript scripts/01_prepare_data.R",
    call. = FALSE
  )
}

student_term <- readr::read_csv(student_term_path, show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::mutate(
    course_id = as.character(course_id),
    term = as.character(term),
    post_redesign = as.integer(post_redesign),
    final_numeric_grade = suppressWarnings(as.numeric(final_numeric_grade)),
    olse_overall_mean = suppressWarnings(as.numeric(olse_overall_mean))
  )

# Defensively reconstruct standardized variables if needed.
if (!"final_numeric_grade_z" %in% names(student_term) ||
    all(is.na(student_term$final_numeric_grade_z))) {
  student_term$final_numeric_grade_z <- safe_scale(student_term$final_numeric_grade)
}

if (!"olse_overall_z" %in% names(student_term) ||
    all(is.na(student_term$olse_overall_z))) {
  student_term$olse_overall_z <- safe_scale(student_term$olse_overall_mean)
}

required_common <- c(
  "course_id", "term", "post_redesign", "final_numeric_grade",
  "final_numeric_grade_z", "olse_overall_mean", "olse_overall_z"
)

missing_common <- setdiff(required_common, names(student_term))
if (length(missing_common) > 0) {
  stop(
    "Missing required baseline-analysis variables: ",
    paste(missing_common, collapse = ", "),
    call. = FALSE
  )
}

message("Student-course rows: ", nrow(student_term))
message("Courses: ", dplyr::n_distinct(student_term$course_id))

# -------------------------------------------------------------------------
# 1. Simple pre/post grade baseline: no course or term fixed effects
# -------------------------------------------------------------------------

message("Fitting simple pre/post grade baseline.")

prepost_desc <- student_term |>
  dplyr::group_by(post_redesign) |>
  dplyr::summarise(
    n = dplyr::n(),
    grade_mean = mean(final_numeric_grade, na.rm = TRUE),
    grade_sd = stats::sd(final_numeric_grade, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    redesign_period = dplyr::if_else(post_redesign == 1L, "post", "pre")
  ) |>
  dplyr::select(redesign_period, post_redesign, n, grade_mean, grade_sd)

pre_mean <- prepost_desc$grade_mean[prepost_desc$post_redesign == 0L]
post_mean <- prepost_desc$grade_mean[prepost_desc$post_redesign == 1L]
raw_difference <- post_mean - pre_mean

# Cluster-robust SEs are retained because student-course observations are
# nested within courses; the model itself remains an unadjusted pre/post model.
prepost_model <- fixest::feols(
  final_numeric_grade ~ post_redesign,
  data = student_term,
  cluster = ~ course_id
)

prepost_result <- extract_fixest_term(
  prepost_model,
  term_name = "post_redesign",
  analysis_label = "Simple pre/post grade comparison",
  effect_scale = "raw grade points"
) |>
  dplyr::mutate(
    pre_mean = pre_mean,
    post_mean = post_mean,
    raw_mean_difference = raw_difference,
    course_fe = FALSE,
    term_fe = FALSE,
    includes_olse = FALSE,
    includes_redesign_features = FALSE
  )

safe_write_csv(prepost_desc, file.path(OUT_BASELINE, "baseline_prepost_descriptives.csv"))
safe_write_csv(prepost_result, file.path(OUT_BASELINE, "baseline_prepost_grade.csv"))

# -------------------------------------------------------------------------
# 2. Adjusted course + term FE grade model: reference for RQ1
# -------------------------------------------------------------------------

message("Fitting course + term fixed-effects reference model.")

fe_model <- fixest::feols(
  final_numeric_grade ~ post_redesign | course_id + term,
  data = student_term,
  cluster = ~ course_id
)

fe_result <- extract_fixest_term(
  fe_model,
  term_name = "post_redesign",
  analysis_label = "Course + term FE grade model",
  effect_scale = "raw grade points"
) |>
  dplyr::mutate(
    course_fe = TRUE,
    term_fe = TRUE,
    includes_olse = FALSE,
    includes_redesign_features = FALSE
  )

safe_write_csv(fe_result, file.path(OUT_BASELINE, "baseline_fixed_effects_reference.csv"))

# -------------------------------------------------------------------------
# 3. Survey-only baseline: OLSE -> grade, without redesign information
#    Use the OLSE-scored file produced by Script 03 so this baseline uses
#    exactly the same overall OLSE scoring/standardization as the SEM workflow.
# -------------------------------------------------------------------------

message("Fitting survey-only OLSE -> grade baseline.")

olse_scored_path <- file.path(DATA_PROCESSED, "olse_scored_for_sem.csv")

if (!file.exists(olse_scored_path)) {
  stop(
    "Could not find olse_scored_for_sem.csv. Run first:\n",
    "Rscript scripts/03_olse_cfa_measurement.R",
    call. = FALSE
  )
}

survey_data <- readr::read_csv(olse_scored_path, show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::mutate(
    course_id = as.character(course_id),
    final_numeric_grade = suppressWarnings(as.numeric(final_numeric_grade)),
    olse_overall_mean = suppressWarnings(as.numeric(olse_overall_mean))
  )

if (!"final_numeric_grade_z" %in% names(survey_data) ||
    all(is.na(survey_data$final_numeric_grade_z))) {
  survey_data$final_numeric_grade_z <- safe_scale(survey_data$final_numeric_grade)
}

if (!"olse_overall_z" %in% names(survey_data) ||
    all(is.na(survey_data$olse_overall_z))) {
  survey_data$olse_overall_z <- safe_scale(survey_data$olse_overall_mean)
}

survey_only_model <- fixest::feols(
  final_numeric_grade_z ~ olse_overall_z,
  data = survey_data,
  cluster = ~ course_id
)

olse_grade_cor <- stats::cor(
  survey_data$olse_overall_z,
  survey_data$final_numeric_grade_z,
  use = "complete.obs"
)

survey_only_result <- extract_fixest_term(
  survey_only_model,
  term_name = "olse_overall_z",
  analysis_label = "Survey-only OLSE -> grade",
  effect_scale = "SD grade per 1 SD OLSE"
) |>
  dplyr::mutate(
    olse_grade_correlation = olse_grade_cor,
    simple_r_squared = olse_grade_cor^2,
    course_fe = FALSE,
    term_fe = FALSE,
    includes_olse = TRUE,
    includes_redesign_features = FALSE
  )

safe_write_csv(survey_only_result, file.path(OUT_BASELINE, "baseline_survey_only.csv"))

# -------------------------------------------------------------------------
# 4. Redesign-only baseline: redesign -> grade, without OLSE
#    Preserve the RQ1 course + term FE adjustment.
# -------------------------------------------------------------------------

message("Fitting redesign-only grade baseline.")

redesign_terms <- c(
  "post_x_mastery_redesign_z",
  "post_x_social_vicarious_redesign_z",
  "post_x_structural_compliance_redesign_z"
)

missing_redesign_terms <- setdiff(redesign_terms, names(student_term))
if (length(missing_redesign_terms) > 0) {
  stop(
    "Missing redesign interaction variables: ",
    paste(missing_redesign_terms, collapse = ", "),
    call. = FALSE
  )
}

redesign_only_model <- fixest::feols(
  final_numeric_grade ~ post_redesign +
    post_x_mastery_redesign_z +
    post_x_social_vicarious_redesign_z +
    post_x_structural_compliance_redesign_z |
    course_id + term,
  data = student_term,
  cluster = ~ course_id
)

redesign_only_result <- broom::tidy(redesign_only_model, conf.int = TRUE) |>
  dplyr::filter(term %in% c("post_redesign", redesign_terms)) |>
  dplyr::mutate(
    analysis = "Redesign-only grade model",
    n = stats::nobs(redesign_only_model),
    effect_scale = "raw grade points",
    course_fe = TRUE,
    term_fe = TRUE,
    includes_olse = FALSE,
    includes_redesign_features = TRUE,
    .before = 1
  ) |>
  dplyr::rename(
    std_error = std.error,
    p_value = p.value,
    conf_low = conf.low,
    conf_high = conf.high
  )

safe_write_csv(redesign_only_result, file.path(OUT_BASELINE, "baseline_redesign_only.csv"))

# -------------------------------------------------------------------------
# 5. Full LENS SEM reference: reuse canonical Script 04 outputs
# -------------------------------------------------------------------------

message("Reading full LENS SEM reference outputs.")

sem_fit_path <- file.path(OUT_SEM, "sem_primary_fit.csv")
sem_params_path <- file.path(OUT_SEM, "sem_primary_params.csv")

if (!file.exists(sem_fit_path) || !file.exists(sem_params_path)) {
  stop(
    "Could not find primary SEM outputs. Run first:\n",
    "Rscript scripts/04_sem_mechanism_model.R",
    call. = FALSE
  )
}

sem_fit <- readr::read_csv(sem_fit_path, show_col_types = FALSE)
sem_params <- readr::read_csv(sem_params_path, show_col_types = FALSE)

if ("status" %in% names(sem_fit) && !all(sem_fit$status == "ok")) {
  warning("Primary SEM fit table contains a non-'ok' status.", call. = FALSE)
}

lens_labels <- c(
  "b",
  "c_post",
  "ind_post",
  "ind_mastery",
  "ind_social",
  "ind_structural"
)

full_lens_reference <- sem_params |>
  dplyr::filter(label %in% lens_labels) |>
  dplyr::select(dplyr::any_of(c(
    "model", "status", "lhs", "op", "rhs", "label", "est", "se",
    "z", "pvalue", "ci.lower", "ci.upper", "std.all"
  ))) |>
  dplyr::mutate(
    analysis = "Full linked LENS SEM",
    includes_olse = TRUE,
    includes_redesign_features = TRUE,
    mechanism_path = TRUE,
    .before = 1
  )

missing_lens_labels <- setdiff(lens_labels, full_lens_reference$label)
if (length(missing_lens_labels) > 0) {
  warning(
    "The following expected primary SEM labels were not found: ",
    paste(missing_lens_labels, collapse = ", "),
    call. = FALSE
  )
}

safe_write_csv(full_lens_reference, file.path(OUT_BASELINE, "full_lens_sem_reference.csv"))

# -------------------------------------------------------------------------
# Compact reviewer-facing comparison summary
# -------------------------------------------------------------------------

prepost_row <- prepost_result[1, ]
fe_row <- fe_result[1, ]
survey_row <- survey_only_result[1, ]

redesign_lookup <- redesign_only_result |>
  dplyr::select(term, estimate, p_value)

get_redesign <- function(term_name) {
  row <- redesign_lookup |>
    dplyr::filter(term == term_name)
  if (nrow(row) == 0) return(c(estimate = NA_real_, p_value = NA_real_))
  c(estimate = row$estimate[[1]], p_value = row$p_value[[1]])
}

mastery <- get_redesign("post_x_mastery_redesign_z")
social <- get_redesign("post_x_social_vicarious_redesign_z")
structural <- get_redesign("post_x_structural_compliance_redesign_z")

lens_lookup <- full_lens_reference |>
  dplyr::select(label, est, pvalue, ci.lower, ci.upper)

get_lens <- function(label_name) {
  row <- lens_lookup |>
    dplyr::filter(label == label_name)
  if (nrow(row) == 0) {
    return(c(est = NA_real_, pvalue = NA_real_, ci.lower = NA_real_, ci.upper = NA_real_))
  }
  c(
    est = row$est[[1]],
    pvalue = row$pvalue[[1]],
    ci.lower = row$ci.lower[[1]],
    ci.upper = row$ci.upper[[1]]
  )
}

ind_post <- get_lens("ind_post")
ind_mastery <- get_lens("ind_mastery")
ind_social <- get_lens("ind_social")
ind_structural <- get_lens("ind_structural")

comparison_summary <- tibble::tibble(
  analysis = c(
    "Simple pre/post grade",
    "Course + term FE grade",
    "Survey-only",
    "Redesign-only",
    "Full linked LENS"
  ),
  grade = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  course_term_fixed_effects = c(FALSE, TRUE, FALSE, TRUE, FALSE),
  olse = c(FALSE, FALSE, TRUE, FALSE, TRUE),
  redesign_features = c(FALSE, FALSE, FALSE, TRUE, TRUE),
  indirect_olse_path = c(FALSE, FALSE, FALSE, FALSE, TRUE),
  focal_result = c(
    sprintf(
      "Raw post-pre grade difference = %s; model estimate = %s, p %s",
      fmt_num(raw_difference), fmt_num(prepost_row$estimate), fmt_p(prepost_row$p_value)
    ),
    sprintf(
      "Adjusted post-redesign grade association = %s, p %s",
      fmt_num(fe_row$estimate), fmt_p(fe_row$p_value)
    ),
    sprintf(
      "OLSE-grade standardized association = %s, p %s; r = %s",
      fmt_num(survey_row$estimate), fmt_p(survey_row$p_value), fmt_num(olse_grade_cor)
    ),
    sprintf(
      "Mastery = %s (p %s); social/vicarious = %s (p %s); structural/compliance = %s (p %s)",
      fmt_num(mastery[["estimate"]]), fmt_p(mastery[["p_value"]]),
      fmt_num(social[["estimate"]]), fmt_p(social[["p_value"]]),
      fmt_num(structural[["estimate"]]), fmt_p(structural[["p_value"]])
    ),
    sprintf(
      "Indirects: post = %s; mastery = %s; social/vicarious = %s; structural/compliance = %s",
      fmt_num(ind_post[["est"]]), fmt_num(ind_mastery[["est"]]),
      fmt_num(ind_social[["est"]]), fmt_num(ind_structural[["est"]])
    )
  ),
  added_information = c(
    "Unadjusted outcome change only",
    "Separates post-redesign association from stable course differences and common term differences",
    "Shows OLSE-grade association without redesign information",
    "Links redesign dimensions to grades without modeling OLSE",
    "Jointly links redesign dimensions, latent OLSE, direct grade associations, and indirect OLSE-linked associations"
  )
)

safe_write_csv(
  comparison_summary,
  file.path(OUT_BASELINE, "lens_baseline_ablation_summary.csv")
)

# Optional workbook for convenient inspection.
if (requireNamespace("writexl", quietly = TRUE)) {
  try(
    writexl::write_xlsx(
      list(
        comparison_summary = comparison_summary,
        prepost_descriptives = prepost_desc,
        prepost_model = prepost_result,
        fixed_effects_reference = fe_result,
        survey_only = survey_only_result,
        redesign_only = redesign_only_result,
        full_lens_reference = full_lens_reference,
        full_lens_fit = sem_fit
      ),
      file.path(OUT_BASELINE, "lens_baseline_ablation_outputs.xlsx")
    ),
    silent = TRUE
  )
}

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

message("")
message("Baseline/ablation comparison summary:")
print(comparison_summary, n = Inf, width = Inf)

message("")
message("Interpretation note:")
message(
  "These specifications are intentionally simpler analyses that answer different questions. ",
  "Do not interpret them as a single predictive model competition or compare AIC/R-squared ",
  "across the regression and SEM specifications as if they were identical models."
)

message("")
message("04c_baseline_ablation_comparisons.R completed successfully.")
message("Outputs written to: ", OUT_BASELINE)
