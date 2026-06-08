#!/usr/bin/env Rscript

# 01_prepare_data.R
# Purpose: read the three public CSV files, validate them, create derived
# identifiers/interactions, and write clean processed datasets used by later
# analysis scripts.

source(file.path("R", "00_packages.R"))
source(file.path("R", "00_paths.R"))
source(file.path("R", "utils_io.R"))

message("Project root: ", PROJECT_ROOT)
message("Reading public data from: ", DATA_RAW)

data <- read_public_data()
student_raw <- data$student_course
course_term_raw <- data$course_term
course_redesign_raw <- data$course_redesign

validate_inputs(student_raw, course_term_raw, course_redesign_raw)

message("Input validation passed.")
message("Student-course rows: ", nrow(student_raw))
message("Course-term rows: ", nrow(course_term_raw))
message("Course-redesign rows: ", nrow(course_redesign_raw))

# Keep only redesign columns needed by the grade models.
cluster_delta_cols <- paste0("cluster_", 1:8, "_delta")
redesign_keep <- c(
  "course_id",
  cluster_delta_cols,
  "mastery_redesign",
  "social_vicarious_redesign",
  "structural_compliance_redesign",
  "redesign_intensity",
  "mastery_redesign_z",
  "social_vicarious_redesign_z",
  "structural_compliance_redesign_z",
  "redesign_intensity_z"
)
redesign_keep <- intersect(redesign_keep, names(course_redesign_raw))

course_redesign_clean <- course_redesign_raw |>
  dplyr::select(dplyr::all_of(redesign_keep))

# Student-course analytic file for FE and downstream analyses.
student_term <- student_raw |>
  dplyr::mutate(
    course_id = as.character(course_id),
    term = as.character(term),
    section_id = as.character(section_id),
    instructor_id = as.character(instructor_id),
    term_index = term_to_index(term),
    post_redesign = as.integer(post_redesign),
    course_term_id = paste(course_id, term, sep = "__"),
    section_term_id = paste(course_id, term, section_id, sep = "__"),
    final_numeric_grade = as.numeric(final_numeric_grade),
    final_numeric_grade_z = safe_scale(final_numeric_grade),
    olse_overall_mean = as.numeric(olse_overall_mean),
    olse_overall_student_z = safe_scale(olse_overall_mean),
    final_grade_letter_checked = point_to_letter(final_numeric_grade)
  ) |>
  dplyr::left_join(course_redesign_clean, by = "course_id")

# Compute z-scores for raw cluster deltas and post-redesign interactions.
for (k in 1:8) {
  raw <- paste0("cluster_", k, "_delta")
  zed <- paste0("cluster_", k, "_delta_z")
  int <- paste0("post_x_cluster_", k, "_delta_z")

  if (raw %in% names(student_term)) {
    student_term[[zed]] <- safe_scale(student_term[[raw]])
    student_term[[int]] <- student_term$post_redesign * student_term[[zed]]
  }
}

# Standardize composite variables if needed, then create post interactions.
composite_vars <- c(
  "mastery_redesign",
  "social_vicarious_redesign",
  "structural_compliance_redesign",
  "redesign_intensity"
)

for (v in composite_vars) {
  vz <- paste0(v, "_z")
  vi <- paste0("post_x_", v, "_z")

  if (v %in% names(student_term) && !(vz %in% names(student_term))) {
    student_term[[vz]] <- safe_scale(student_term[[v]])
  }

  if (vz %in% names(student_term)) {
    student_term[[vi]] <- student_term$post_redesign * student_term[[vz]]
  }
}

course_term_clean <- course_term_raw |>
  dplyr::mutate(
    course_id = as.character(course_id),
    term = as.character(term),
    section_id = as.character(section_id),
    instructor_id = as.character(instructor_id),
    term_index = term_to_index(term),
    post_redesign = as.integer(post_redesign),
    course_term_id = paste(course_id, term, sep = "__"),
    section_term_id = paste(course_id, term, section_id, sep = "__")
  )

# Course-term aggregation from student file for FE robustness checks.
course_term_from_students <- student_term |>
  dplyr::group_by(course_id, term, term_index, post_redesign, course_term_id) |>
  dplyr::summarise(
    n_students = dplyr::n(),
    final_grade_mean = mean(final_numeric_grade, na.rm = TRUE),
    final_grade_sd = stats::sd(final_numeric_grade, na.rm = TRUE),
    olse_mean = mean(olse_overall_mean, na.rm = TRUE),
    .groups = "drop"
  )

# Diagnostics.
grade_mismatch_n <- sum(student_term$final_grade_letter_sampled != student_term$final_grade_letter_checked, na.rm = TRUE)
student_olse_grade_cor <- stats::cor(
  student_term$olse_overall_mean,
  student_term$final_numeric_grade,
  use = "pairwise.complete.obs"
)

validation_summary <- tibble::tibble(
  metric = c(
    "student_course_rows",
    "course_term_rows",
    "course_redesign_rows",
    "courses",
    "course_term_cells",
    "section_term_rows",
    "pre_redesign_observations",
    "post_redesign_observations",
    "letter_grade_mismatches",
    "student_olse_grade_correlation"
  ),
  value = c(
    nrow(student_term),
    nrow(course_term_clean),
    nrow(course_redesign_clean),
    dplyr::n_distinct(student_term$course_id),
    dplyr::n_distinct(student_term$course_term_id),
    dplyr::n_distinct(student_term$section_term_id),
    sum(student_term$post_redesign == 0, na.rm = TRUE),
    sum(student_term$post_redesign == 1, na.rm = TRUE),
    grade_mismatch_n,
    round(student_olse_grade_cor, 6)
  )
)

safe_write_csv(student_term, file.path(DATA_PROCESSED, "student_term_fe_analysis.csv"))
safe_write_csv(course_term_from_students, file.path(DATA_PROCESSED, "course_term_grade_aggregation.csv"))
safe_write_csv(course_term_clean, file.path(DATA_PROCESSED, "course_term_clean.csv"))
safe_write_csv(course_redesign_clean, file.path(DATA_PROCESSED, "course_redesign_clean.csv"))
safe_write_csv(validation_summary, file.path(DATA_PROCESSED, "input_validation_summary.csv"))

message("Data preparation complete.")
message("Processed data written to: ", DATA_PROCESSED)
