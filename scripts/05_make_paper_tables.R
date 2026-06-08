#!/usr/bin/env Rscript

# 05_make_paper_tables.R
# Purpose: collect outputs from scripts 01-04 and create clean paper-facing
# CSV tables under outputs/paper_tables/.
#
# This script does not refit models. It only reads existing CSV outputs.
# Run after:
#   Rscript scripts/01_prepare_data.R
#   Rscript scripts/02_fixed_effects_models.R
#   Rscript scripts/03_olse_cfa_measurement.R
#   Rscript scripts/04_sem_mechanism_models.R

# -------------------------------------------------------------------------
# 0. Shared setup
# -------------------------------------------------------------------------

source(file.path("R", "00_packages.R"))
source(file.path("R", "00_paths.R"))
source(file.path("R", "utils_io.R"))

# The first-two starter 00_paths.R may not define these later output folders.
if (!exists("OUT_OLSE", inherits = FALSE)) {
  OUT_OLSE <- file.path(PROJECT_ROOT, "outputs", "olse_measurement")
}
if (!exists("OUT_SEM", inherits = FALSE)) {
  OUT_SEM <- file.path(PROJECT_ROOT, "outputs", "sem_models")
}
if (!exists("OUT_TABLES", inherits = FALSE)) {
  OUT_TABLES <- file.path(PROJECT_ROOT, "outputs", "paper_tables")
}

dir.create(OUT_TABLES, recursive = TRUE, showWarnings = FALSE)

message("Project root: ", PROJECT_ROOT)
message("Reading FE outputs from: ", OUT_FE)
message("Reading OLSE/CFA outputs from: ", OUT_OLSE)
message("Reading SEM outputs from: ", OUT_SEM)
message("Writing paper tables to: ", OUT_TABLES)

# -------------------------------------------------------------------------
# 1. Local helpers
# -------------------------------------------------------------------------

read_required_csv <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Required file not found: ", path, "\n",
      "Run the earlier scripts before this one.",
      call. = FALSE
    )
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
}

read_optional_csv <- function(path) {
  if (!file.exists(path)) {
    warning("Optional file not found: ", path, call. = FALSE)
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
}

first_value <- function(x, default = NA_real_) {
  if (length(x) == 0) return(default)
  if (all(is.na(x))) return(default)
  x[which(!is.na(x))[1]]
}

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "< .001",
    TRUE ~ formatC(p, format = "f", digits = 4)
  )
}

round3 <- function(x) round(as.numeric(x), 3)
round4 <- function(x) round(as.numeric(x), 4)

standardize_fe_model_name <- function(x) {
  dplyr::case_when(
    x %in% c("Course + term FE", "Grade") ~ "Course + term FE",
    x %in% c("Grade z-score outcome", "Grade z") ~ "Grade z-score outcome",
    x %in% c("Course + term + instructor", "Course + term + instructor FE") ~ "Course + term + instructor",
    x %in% c("Course-term aggregate", "Course-term aggregation") ~ "Course-term aggregate",
    TRUE ~ x
  )
}

path_label_sem <- function(lhs, op, rhs, label) {
  dplyr::case_when(
    op == "~" & lhs == "OLSE" & rhs == "post_redesign" ~ "OLSE <- post-redesign",
    op == "~" & lhs == "OLSE" & rhs == "post_x_mastery_z" ~ "OLSE <- post x mastery",
    op == "~" & lhs == "OLSE" & rhs == "post_x_social_z" ~ "OLSE <- post x social/vicarious",
    op == "~" & lhs == "OLSE" & rhs == "post_x_structural_z" ~ "OLSE <- post x structural/compliance",
    op == "~" & lhs == "final_numeric_grade_z" & rhs == "OLSE" ~ "Grade <- OLSE",
    op == "~" & lhs == "final_numeric_grade_z" & rhs == "post_redesign" ~ "Grade <- post-redesign",
    op == ":=" & label == "ind_post" ~ "Post -> OLSE -> grade",
    op == ":=" & label == "ind_mastery" ~ "Mastery -> OLSE -> grade",
    op == ":=" & label == "ind_social" ~ "Social/vicarious -> OLSE -> grade",
    op == ":=" & label == "ind_structural" ~ "Structural/compliance -> OLSE -> grade",
    op == ":=" & label == "ind_intensity" ~ "Intensity -> OLSE -> grade",
    TRUE ~ paste(lhs, op, rhs)
  )
}

# -------------------------------------------------------------------------
# 2. Load outputs from earlier scripts
# -------------------------------------------------------------------------

student_term <- read_required_csv(file.path(DATA_PROCESSED, "student_term_fe_analysis.csv"))

desc_prepost <- read_required_csv(file.path(OUT_FE, "descriptive_prepost_summary.csv"))
fe_coefficients <- read_required_csv(file.path(OUT_FE, "project_soar_fe_grade_coefficients.csv"))
cr2 <- read_required_csv(file.path(OUT_FE, "primary_fe_cr2_small_cluster_test.csv"))
loo_summary <- read_required_csv(file.path(OUT_FE, "leave_one_course_out_fe_summary.csv"))

reliability <- read_required_csv(file.path(OUT_OLSE, "olse_reliability_alpha.csv"))
cfa_fit_comparison <- read_required_csv(file.path(OUT_OLSE, "olse_cfa_fit_comparison.csv"))

sem_primary_params <- read_required_csv(file.path(OUT_SEM, "sem_primary_params.csv"))
sem_sensitivity <- read_required_csv(file.path(OUT_SEM, "sem_sensitivity_summary_for_paper.csv"))

# -------------------------------------------------------------------------
# 3. Table III: dataset summary and pre/post descriptives
# -------------------------------------------------------------------------

pre_row <- desc_prepost |>
  dplyr::filter(post_redesign == 0) |>
  dplyr::slice(1)

post_row <- desc_prepost |>
  dplyr::filter(post_redesign == 1) |>
  dplyr::slice(1)

table_iii <- tibble::tibble(
  quantity = c(
    "Student-course observations",
    "Courses",
    "Course-term cells",
    "Section-term rows",
    "Pre-redesign observations",
    "Post-redesign observations",
    "Mean pre-redesign grade",
    "Mean post-redesign grade",
    "Mean pre-redesign OLSE",
    "Mean post-redesign OLSE"
  ),
  value = c(
    nrow(student_term),
    dplyr::n_distinct(student_term$course_id),
    dplyr::n_distinct(paste(student_term$course_id, student_term$term, sep = "__")),
    dplyr::n_distinct(paste(student_term$course_id, student_term$term, student_term$section_id, sep = "__")),
    first_value(pre_row$n),
    first_value(post_row$n),
    first_value(pre_row$grade_mean),
    first_value(post_row$grade_mean),
    first_value(pre_row$olse_mean),
    first_value(post_row$olse_mean)
  )
)

safe_write_csv(table_iii, file.path(OUT_TABLES, "table_iii_dataset_summary.csv"))

# -------------------------------------------------------------------------
# 4. Table IV: fixed-effects grade model results
# -------------------------------------------------------------------------

fe_post <- fe_coefficients |>
  dplyr::mutate(model_clean = standardize_fe_model_name(model)) |>
  dplyr::filter(term == "post_redesign") |>
  dplyr::filter(model_clean %in% c(
    "Course + term FE",
    "Grade z-score outcome",
    "Course + term + instructor",
    "Course-term aggregate"
  )) |>
  dplyr::mutate(
    model_order = match(model_clean, c(
      "Course + term FE",
      "Grade z-score outcome",
      "Course + term + instructor",
      "Course-term aggregate"
    ))
  ) |>
  dplyr::arrange(model_order) |>
  dplyr::distinct(model_clean, .keep_all = TRUE)

table_iv <- fe_post |>
  dplyr::transmute(
    model = model_clean,
    estimate = estimate,
    std_error = std_error,
    p_value = p_value
  )

safe_write_csv(table_iv, file.path(OUT_TABLES, "table_iv_fe_results.csv"))

# -------------------------------------------------------------------------
# 5. Table V: RQ1 robustness checks
# -------------------------------------------------------------------------

instructor_row <- fe_post |>
  dplyr::filter(model_clean == "Course + term + instructor") |>
  dplyr::slice(1)

aggregate_row <- fe_post |>
  dplyr::filter(model_clean == "Course-term aggregate") |>
  dplyr::slice(1)

cr2_row <- cr2 |>
  dplyr::filter(term == "post_redesign" | coef == "post_redesign") |>
  dplyr::slice(1)

loo_row <- loo_summary |>
  dplyr::slice(1)

table_v <- tibble::tibble(
  check = c(
    "CR2/Satterthwaite",
    "Leave-one-course-out",
    "Instructor-adjusted FE",
    "Course-term aggregate"
  ),
  estimate = c(
    first_value(cr2_row$beta),
    NA_real_,
    first_value(instructor_row$estimate),
    first_value(aggregate_row$estimate)
  ),
  std_error = c(
    first_value(cr2_row$se),
    NA_real_,
    first_value(instructor_row$std_error),
    first_value(aggregate_row$std_error)
  ),
  df = c(
    first_value(cr2_row$df_satt),
    NA_real_,
    NA_real_,
    NA_real_
  ),
  p_value = c(
    first_value(cr2_row$p_satt),
    first_value(loo_row$max_p),
    first_value(instructor_row$p_value),
    first_value(aggregate_row$p_value)
  ),
  main_finding = c(
    paste0(
      "beta = ", round3(first_value(cr2_row$beta)),
      ", SE = ", round3(first_value(cr2_row$se)),
      ", df = ", round(first_value(cr2_row$df_satt), 2),
      ", p = ", fmt_p(first_value(cr2_row$p_satt))
    ),
    paste0(
      "All estimates positive = ", first_value(loo_row$all_positive),
      "; beta range = ", round3(first_value(loo_row$min_estimate)),
      "-", round3(first_value(loo_row$max_estimate))
    ),
    paste0(
      "beta = ", round3(first_value(instructor_row$estimate)),
      ", SE = ", round3(first_value(instructor_row$std_error)),
      ", p = ", fmt_p(first_value(instructor_row$p_value))
    ),
    paste0(
      "beta = ", round3(first_value(aggregate_row$estimate)),
      ", SE = ", round3(first_value(aggregate_row$std_error)),
      ", p = ", fmt_p(first_value(aggregate_row$p_value))
    )
  )
)

safe_write_csv(table_v, file.path(OUT_TABLES, "table_v_rq1_robustness.csv"))

# -------------------------------------------------------------------------
# 6. Table VI: OLSE reliability
# -------------------------------------------------------------------------

table_vi <- reliability |>
  dplyr::mutate(
    scale = dplyr::recode(
      scale,
      "Q1" = "OLSE Domain Q1",
      "Q2" = "OLSE Domain Q2",
      "Q3" = "OLSE Domain Q3",
      "Q4" = "OLSE Domain Q4",
      "Q5" = "OLSE Domain Q5",
      "Overall_30_item" = "Overall OLSE scale",
      .default = scale
    )
  ) |>
  dplyr::transmute(
    scale = scale,
    items = n_items,
    alpha = alpha
  )

safe_write_csv(table_vi, file.path(OUT_TABLES, "table_vi_olse_reliability.csv"))

# -------------------------------------------------------------------------
# 7. Table VII: primary SEM structural paths
# -------------------------------------------------------------------------

sem_primary_params <- sem_primary_params |>
  dplyr::mutate(
    path = path_label_sem(lhs, op, rhs, label)
  )

wanted_paths_vii <- c(
  "OLSE <- post-redesign",
  "OLSE <- post x mastery",
  "OLSE <- post x social/vicarious",
  "OLSE <- post x structural/compliance",
  "Grade <- OLSE",
  "Grade <- post-redesign"
)

table_vii <- sem_primary_params |>
  dplyr::filter(path %in% wanted_paths_vii) |>
  dplyr::mutate(path_order = match(path, wanted_paths_vii)) |>
  dplyr::arrange(path_order) |>
  dplyr::distinct(path, .keep_all = TRUE) |>
  dplyr::transmute(
    path = path,
    estimate = est,
    std_error = se,
    p_value = pvalue
  )

safe_write_csv(table_vii, file.path(OUT_TABLES, "table_vii_sem_paths.csv"))

# -------------------------------------------------------------------------
# 8. Table VIII: indirect associations through OLSE
# -------------------------------------------------------------------------

wanted_paths_viii <- c(
  "Post -> OLSE -> grade",
  "Mastery -> OLSE -> grade",
  "Social/vicarious -> OLSE -> grade",
  "Structural/compliance -> OLSE -> grade"
)

table_viii <- sem_primary_params |>
  dplyr::filter(path %in% wanted_paths_viii) |>
  dplyr::mutate(path_order = match(path, wanted_paths_viii)) |>
  dplyr::arrange(path_order) |>
  dplyr::distinct(path, .keep_all = TRUE) |>
  dplyr::transmute(
    indirect_association = path,
    estimate = est,
    std_error = se,
    p_value = pvalue
  )

safe_write_csv(table_viii, file.path(OUT_TABLES, "table_viii_indirect_associations.csv"))

# -------------------------------------------------------------------------
# 9. Appendix Table X: SEM sensitivity checks
# -------------------------------------------------------------------------

sem_sensitivity <- sem_sensitivity |>
  dplyr::mutate(
    path = path_label_sem(lhs, op, rhs, label)
  )

appendix_x_source <- sem_sensitivity |>
  dplyr::filter(
    (source_model == "Primary latent OLSE parcel SEM" & label == "ind_post") |
      (source_model == "Manifest OLSE path model" & label == "ind_post") |
      (source_model == "Overall redesign intensity SEM" & label == "ind_intensity")
  ) |>
  dplyr::mutate(
    model = dplyr::case_when(
      source_model == "Primary latent OLSE parcel SEM" ~ "Latent OLSE parcel SEM",
      source_model == "Manifest OLSE path model" ~ "Manifest OLSE path model",
      source_model == "Overall redesign intensity SEM" ~ "Redesign-intensity SEM",
      TRUE ~ source_model
    ),
    key_result = dplyr::case_when(
      source_model == "Primary latent OLSE parcel SEM" ~ "Post -> OLSE -> grade",
      source_model == "Manifest OLSE path model" ~ "Post -> OLSE -> grade",
      source_model == "Overall redesign intensity SEM" ~ "Intensity -> OLSE -> grade",
      TRUE ~ path
    ),
    model_order = match(model, c(
      "Latent OLSE parcel SEM",
      "Manifest OLSE path model",
      "Redesign-intensity SEM"
    ))
  ) |>
  dplyr::arrange(model_order)

table_x <- appendix_x_source |>
  dplyr::transmute(
    model = model,
    key_result = key_result,
    estimate = est,
    std_error = se,
    p_value = pvalue,
    main_finding = paste0(
      key_result,
      " = ", round3(est),
      ", p ", fmt_p(pvalue)
    )
  )

safe_write_csv(table_x, file.path(OUT_TABLES, "appendix_table_x_sem_sensitivity.csv"))

# Also write a comprehensive sensitivity-indirect table for reviewer convenience.
sem_sensitivity_indirects <- sem_sensitivity |>
  dplyr::filter(op == ":=", stringr::str_starts(label, "ind_")) |>
  dplyr::transmute(
    source_model = source_model,
    indirect_association = path,
    label = label,
    estimate = est,
    std_error = se,
    p_value = pvalue,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    std_all = std_all
  )

safe_write_csv(
  sem_sensitivity_indirects,
  file.path(OUT_TABLES, "appendix_sem_sensitivity_all_indirects.csv")
)

# -------------------------------------------------------------------------
# 10. Appendix Table XI: CFA comparison
# -------------------------------------------------------------------------

cfa_wide <- cfa_fit_comparison |>
  dplyr::filter(status == "ok") |>
  dplyr::select(model, fit_index, value) |>
  tidyr::pivot_wider(names_from = fit_index, values_from = value) |>
  janitor::clean_names()

get_col <- function(df, col) {
  if (col %in% names(df)) df[[col]] else rep(NA_real_, nrow(df))
}

appendix_xi <- cfa_wide |>
  dplyr::transmute(
    model = model,
    df = get_col(cfa_wide, "df_scaled"),
    p_value = get_col(cfa_wide, "pvalue_scaled"),
    cfi = get_col(cfa_wide, "cfi_scaled"),
    tli = get_col(cfa_wide, "tli_scaled"),
    rmsea = get_col(cfa_wide, "rmsea_scaled"),
    srmr = get_col(cfa_wide, "srmr")
  )

safe_write_csv(appendix_xi, file.path(OUT_TABLES, "appendix_table_xi_cfa_comparison.csv"))

# -------------------------------------------------------------------------
# 11. Optional Excel workbook with all paper tables
# -------------------------------------------------------------------------

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    list(
      table_iii_dataset_summary = table_iii,
      table_iv_fe_results = table_iv,
      table_v_rq1_robustness = table_v,
      table_vi_olse_reliability = table_vi,
      table_vii_sem_paths = table_vii,
      table_viii_indirects = table_viii,
      appendix_x_sem_sensitivity = table_x,
      appendix_xi_cfa_comparison = appendix_xi,
      appendix_all_sem_indirects = sem_sensitivity_indirects
    ),
    file.path(OUT_TABLES, "lens_paper_tables.xlsx")
  )
  message("Wrote: ", file.path(OUT_TABLES, "lens_paper_tables.xlsx"))
}

# -------------------------------------------------------------------------
# 12. Completion summary
# -------------------------------------------------------------------------

message("")
message("Paper table creation complete.")
message("Paper tables written to: ", OUT_TABLES)
message("")
message("Created:")
message("- table_iii_dataset_summary.csv")
message("- table_iv_fe_results.csv")
message("- table_v_rq1_robustness.csv")
message("- table_vi_olse_reliability.csv")
message("- table_vii_sem_paths.csv")
message("- table_viii_indirect_associations.csv")
message("- appendix_table_x_sem_sensitivity.csv")
message("- appendix_table_xi_cfa_comparison.csv")
message("- appendix_sem_sensitivity_all_indirects.csv")
message("- lens_paper_tables.xlsx, if writexl is installed")
