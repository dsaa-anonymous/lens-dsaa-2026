# 04_sem_mechanism_models.R
# Purpose: reproduce the RQ3 SEM mechanism-oriented models for the LENS
# DSAA 2026 reproducibility repository.
#
# Required prior steps:
#   Rscript scripts/01_prepare_data.R
#   Rscript scripts/03_olse_cfa_measurement.R
#
# Main inputs:
#   data/processed/olse_scored_for_sem.csv
#   data/processed/course_redesign_clean.csv
#
# Main outputs:
#   outputs/sem_models/sem_analysis_dataset_summary.csv
#   outputs/sem_models/sem_primary_fit.csv
#   outputs/sem_models/sem_primary_params.csv
#   outputs/sem_models/sem_manifest_fit.csv
#   outputs/sem_models/sem_manifest_params.csv
#   outputs/sem_models/sem_intensity_fit.csv
#   outputs/sem_models/sem_intensity_params.csv
#   outputs/sem_models/sem_cluster_one_at_a_time_fit.csv
#   outputs/sem_models/sem_cluster_one_at_a_time_params.csv
#   outputs/sem_models/sem_all_model_fit_tables.csv
#   outputs/sem_models/sem_all_model_parameters.csv
#   outputs/sem_models/sem_sensitivity_summary_for_paper.csv

source(file.path("R", "00_packages.R"))
source(file.path("R", "00_paths.R"))
source(file.path("R", "utils_io.R"))

# -------------------------------------------------------------------------
# Additional package checks for SEM workflow
# -------------------------------------------------------------------------

extra_packages <- c("lavaan", "semTools", "writexl")
missing_extra <- extra_packages[!vapply(extra_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_extra) > 0) {
  stop(
    "Missing required R packages for Script 04: ", paste(missing_extra, collapse = ", "), "\n",
    "Install them with renv::restore() if renv.lock exists, or run:\n",
    "install.packages(c(", paste(sprintf('"%s"', missing_extra), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(lavaan)
  library(semTools)
})

if (!exists("OUT_SEM", inherits = FALSE)) {
  OUT_SEM <- file.path(PROJECT_ROOT, "outputs", "sem_models")
}

if (!exists("OUT_SEM_EXPLORATORY", inherits = FALSE)) {
  OUT_SEM_EXPLORATORY <- file.path(OUT_SEM, "exploratory")
}

dir.create(OUT_SEM, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_SEM_EXPLORATORY, recursive = TRUE, showWarnings = FALSE)

message("Project root: ", PROJECT_ROOT)
message("Reading processed data from: ", DATA_PROCESSED)
message("Writing SEM outputs to: ", OUT_SEM)

# -------------------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------------------

safe_scale_local <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  sx <- stats::sd(x, na.rm = TRUE)
  if (is.na(sx) || sx == 0) return(rep(0, length(x)))
  as.numeric(scale(x))
}

mean_if_enough <- function(df, cols, min_prop = 0.50) {
  vals <- df[, cols, drop = FALSE]
  vals <- dplyr::mutate(vals, dplyr::across(dplyr::everything(), as.numeric))
  observed <- rowSums(!is.na(vals))
  required <- ceiling(length(cols) * min_prop)
  out <- rowMeans(vals, na.rm = TRUE)
  out[observed < required] <- NA_real_
  out
}

write_csv_local <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path, na = "")
  message("Wrote: ", path)
}

ensure_z <- function(df, var) {
  zvar <- paste0(var, "_z")
  if (var %in% names(df)) {
    if (!zvar %in% names(df) || all(is.na(df[[zvar]]))) {
      df[[zvar]] <- safe_scale_local(df[[var]])
    }
  }
  df
}

fit_sem_safe <- function(model_syntax, data, model_name, cluster_var = "course_id", estimator = "MLR") {
  fit <- tryCatch(
    lavaan::sem(
      model_syntax,
      data = data,
      estimator = estimator,
      missing = "fiml",
      cluster = cluster_var,
      std.lv = TRUE,
      fixed.x = FALSE
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(
      fit = NULL,
      fit_table = tibble::tibble(model = model_name, status = "error", error = fit$message),
      params = tibble::tibble(model = model_name, status = "error", error = fit$message)
    ))
  }

  did_converge <- tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) FALSE)

  if (!isTRUE(did_converge)) {
    params <- tryCatch(
      lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE) |>
        tibble::as_tibble() |>
        dplyr::mutate(model = model_name, status = "not_converged", .before = 1),
      error = function(e) tibble::tibble(model = model_name, status = "not_converged", error = e$message)
    )

    return(list(
      fit = fit,
      fit_table = tibble::tibble(
        model = model_name,
        status = "not_converged",
        converged = FALSE,
        error = "lavaan returned a fitted object, but the model did not converge."
      ),
      params = params
    ))
  }

  fit_table <- tryCatch(
    lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")) |>
      tibble::enframe(name = "fit_index", value = "value") |>
      tidyr::pivot_wider(names_from = fit_index, values_from = value) |>
      dplyr::mutate(model = model_name, status = "ok", converged = TRUE, .before = 1),
    error = function(e) tibble::tibble(model = model_name, status = "fit_measure_error", converged = TRUE, error = e$message)
  )

  params <- tryCatch(
    lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE) |>
      tibble::as_tibble() |>
      dplyr::mutate(model = model_name, status = "ok", .before = 1),
    error = function(e) tibble::tibble(model = model_name, status = "parameter_error", error = e$message)
  )

  list(fit = fit, fit_table = fit_table, params = params)
}

select_key_params <- function(params) {
  if (!all(c("op", "lhs", "rhs") %in% names(params))) return(params)

  params |>
    dplyr::filter(op %in% c("~", ":=", "=~")) |>
    dplyr::select(dplyr::any_of(c(
      "model", "status", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue",
      "ci.lower", "ci.upper", "std.all", "error"
    )))
}

to_plain_tibble <- function(x, model_name = NA_character_) {
  if (is.null(x)) {
    return(tibble::tibble(model = model_name, status = "missing"))
  }

  if (inherits(x, "lavaan.vector") || (is.atomic(x) && !is.null(names(x)))) {
    nm <- names(x)
    if (is.null(nm) || any(nm == "")) nm <- paste0("value_", seq_along(x))

    out <- tibble::tibble(
      fit_index = make.names(nm, unique = TRUE),
      value = suppressWarnings(as.numeric(unclass(x)))
    ) |>
      tidyr::pivot_wider(names_from = fit_index, values_from = value)

    out$model <- model_name
    out$status <- "ok"
    return(dplyr::relocate(out, model, status))
  }

  if (is.atomic(x)) {
    return(tibble::tibble(model = model_name, status = "atomic_vector", value = paste(as.character(x), collapse = "; ")))
  }

  out <- tryCatch(
    tibble::as_tibble(as.data.frame(x, stringsAsFactors = FALSE)),
    error = function(e) tibble::tibble(model = model_name, status = "conversion_error", error = e$message)
  )

  out[] <- lapply(out, function(col) {
    if (inherits(col, "lavaan.vector")) return(as.numeric(unclass(col)))
    if (is.factor(col)) return(as.character(col))
    if (is.list(col)) {
      return(vapply(
        col,
        function(z) {
          if (is.null(z)) return(NA_character_)
          paste(as.character(unlist(z)), collapse = "; ")
        },
        character(1)
      ))
    }
    if (is.atomic(col)) return(as.vector(col))
    as.character(col)
  })

  out <- tibble::as_tibble(out)

  if (!"model" %in% names(out)) out <- dplyr::mutate(out, model = model_name, .before = 1)
  if (!"status" %in% names(out)) out <- dplyr::mutate(out, status = "ok", .after = model)

  out
}

safe_bind_rows_named <- function(x) {
  nms <- names(x)
  if (is.null(nms)) nms <- paste0("object_", seq_along(x))
  cleaned <- lapply(seq_along(x), function(i) to_plain_tibble(x[[i]], nms[[i]]))
  dplyr::bind_rows(cleaned)
}

param_subset <- function(params, labels = character(), model_label = NULL) {
  if (!all(c("op", "label") %in% names(params))) return(tibble::tibble())

  out <- params |>
    dplyr::filter(op == ":=" | label %in% labels)

  if (!is.null(model_label) && nrow(out) > 0) {
    out <- out |> dplyr::mutate(source_model = model_label, .before = 1)
  }

  out
}

# -------------------------------------------------------------------------
# Read Script 03 SEM-ready data and redesign data
# -------------------------------------------------------------------------

scored_candidates <- c(
  file.path(DATA_PROCESSED, "olse_scored_for_sem.csv"),
  file.path(OUT_OLSE, "olse_scored_for_sem.csv"),
  file.path(OUT_OLSE, "project_soar_olse_scored_for_sem_v4.csv")
)

scored_path <- scored_candidates[file.exists(scored_candidates)][1]

if (is.na(scored_path) || length(scored_path) == 0) {
  stop(
    "Could not find SEM-ready OLSE scored file. Run first:\n",
    "Rscript scripts/03_olse_cfa_measurement.R",
    call. = FALSE
  )
}

redesign_candidates <- c(
  file.path(DATA_PROCESSED, "course_redesign_clean.csv"),
  file.path(PROJECT_ROOT, "data", "raw_public", "course_redesign.csv"),
  file.path(PROJECT_ROOT, "data", "raw_public", "Project_SOAR_course_redesign_realistic_v4.csv")
)

redesign_path <- redesign_candidates[file.exists(redesign_candidates)][1]

if (is.na(redesign_path) || length(redesign_path) == 0) {
  stop(
    "Could not find course redesign file. Run first:\n",
    "Rscript scripts/01_prepare_data.R",
    call. = FALSE
  )
}

message("SEM scored file: ", scored_path)
message("Course redesign file: ", redesign_path)

sem_student <- readr::read_csv(scored_path, show_col_types = FALSE) |>
  janitor::clean_names()

redesign <- readr::read_csv(redesign_path, show_col_types = FALSE) |>
  janitor::clean_names()

# -------------------------------------------------------------------------
# Prepare SEM analysis data
# -------------------------------------------------------------------------

q1_items <- paste0("q1_", 1:8)
q2_items <- paste0("q2_", 1:5)
q3_items <- paste0("q3_", 1:6)
q4_items <- paste0("q4_", 1:5)
q5_items <- paste0("q5_", 1:6)
olse_items <- c(q1_items, q2_items, q3_items, q4_items, q5_items)

# If someone bypassed Script 03 and the scored file still has raw OLSE items,
# compute missing parcel columns defensively.
if (!all(c("olse_q1_mean", "olse_q2_mean", "olse_q3_mean", "olse_q4_mean", "olse_q5_mean") %in% names(sem_student))) {
  if (all(olse_items %in% names(sem_student))) {
    sem_student <- sem_student |>
      dplyr::mutate(dplyr::across(dplyr::all_of(olse_items), ~ suppressWarnings(as.numeric(.x))))

    sem_student$olse_q1_mean <- mean_if_enough(sem_student, q1_items)
    sem_student$olse_q2_mean <- mean_if_enough(sem_student, q2_items)
    sem_student$olse_q3_mean <- mean_if_enough(sem_student, q3_items)
    sem_student$olse_q4_mean <- mean_if_enough(sem_student, q4_items)
    sem_student$olse_q5_mean <- mean_if_enough(sem_student, q5_items)
    sem_student$olse_overall_mean <- mean_if_enough(sem_student, olse_items)
  } else {
    stop("The SEM scored file is missing OLSE parcel columns and raw OLSE items.", call. = FALSE)
  }
}

for (v in c("mastery_redesign", "social_vicarious_redesign", "structural_compliance_redesign", "redesign_intensity")) {
  redesign <- ensure_z(redesign, v)
}

for (k in 1:8) {
  raw_v <- paste0("cluster_", k, "_delta")
  if (raw_v %in% names(redesign)) {
    redesign[[paste0(raw_v, "_z")]] <- safe_scale_local(redesign[[raw_v]])
  }
}

# Join only variables not already present in the scored file, avoiding .x/.y columns.
redesign_join_cols <- setdiff(names(redesign), names(sem_student))
redesign_join_cols <- c("course_id", redesign_join_cols)
redesign_join_cols <- unique(redesign_join_cols[redesign_join_cols %in% names(redesign)])

sem_data <- sem_student |>
  dplyr::left_join(
    redesign |> dplyr::select(dplyr::all_of(redesign_join_cols)),
    by = "course_id"
  ) |>
  dplyr::mutate(
    post_redesign = as.integer(post_redesign),
    redesign_status = factor(dplyr::if_else(post_redesign == 1, "post", "pre"), levels = c("pre", "post")),
    final_numeric_grade = suppressWarnings(as.numeric(final_numeric_grade)),
    olse_q1_mean = suppressWarnings(as.numeric(olse_q1_mean)),
    olse_q2_mean = suppressWarnings(as.numeric(olse_q2_mean)),
    olse_q3_mean = suppressWarnings(as.numeric(olse_q3_mean)),
    olse_q4_mean = suppressWarnings(as.numeric(olse_q4_mean)),
    olse_q5_mean = suppressWarnings(as.numeric(olse_q5_mean)),
    olse_overall_mean = suppressWarnings(as.numeric(olse_overall_mean)),
    course_term_id = dplyr::if_else(
      is.na(course_term_id) | course_term_id == "",
      paste(course_id, term, sep = "__"),
      as.character(course_term_id)
    )
  )

# Ensure standardized variables exist.
needed_z <- c(
  "olse_q1_mean", "olse_q2_mean", "olse_q3_mean", "olse_q4_mean", "olse_q5_mean",
  "olse_overall_mean", "final_numeric_grade",
  "mastery_redesign", "social_vicarious_redesign", "structural_compliance_redesign", "redesign_intensity"
)

for (v in needed_z) {
  zname <- paste0(v, "_z")
  if (v %in% names(sem_data)) {
    if (!zname %in% names(sem_data) || all(is.na(sem_data[[zname]]))) {
      sem_data[[zname]] <- safe_scale_local(sem_data[[v]])
    }
  }
}

# Preferred compact interaction names used by the SEM syntax.
sem_data <- sem_data |>
  dplyr::mutate(
    post_x_mastery_z = dplyr::case_when(
      "post_x_mastery_redesign_z" %in% names(sem_data) ~ .data$post_x_mastery_redesign_z,
      TRUE ~ post_redesign * mastery_redesign_z
    ),
    post_x_social_z = dplyr::case_when(
      "post_x_social_vicarious_redesign_z" %in% names(sem_data) ~ .data$post_x_social_vicarious_redesign_z,
      TRUE ~ post_redesign * social_vicarious_redesign_z
    ),
    post_x_structural_z = dplyr::case_when(
      "post_x_structural_compliance_redesign_z" %in% names(sem_data) ~ .data$post_x_structural_compliance_redesign_z,
      TRUE ~ post_redesign * structural_compliance_redesign_z
    ),
    post_x_intensity_z = dplyr::case_when(
      "post_x_redesign_intensity_z" %in% names(sem_data) ~ .data$post_x_redesign_intensity_z,
      TRUE ~ post_redesign * redesign_intensity_z
    )
  )

# Create one-at-a-time cluster interactions.
for (k in 1:8) {
  zvar <- paste0("cluster_", k, "_delta_z")
  xvar <- paste0("post_x_cluster_", k, "_z")
  if (zvar %in% names(sem_data)) {
    sem_data[[xvar]] <- sem_data$post_redesign * sem_data[[zvar]]
  }
}

required_sem_vars <- c(
  "course_id", "course_term_id", "post_redesign", "final_numeric_grade_z",
  "olse_q1_mean_z", "olse_q2_mean_z", "olse_q3_mean_z", "olse_q4_mean_z", "olse_q5_mean_z",
  "olse_overall_z", "post_x_mastery_z", "post_x_social_z", "post_x_structural_z", "post_x_intensity_z"
)

missing_required <- setdiff(required_sem_vars, names(sem_data))
if (length(missing_required) > 0) {
  stop("Missing required SEM variables: ", paste(missing_required, collapse = ", "), call. = FALSE)
}

analysis_summary <- sem_data |>
  dplyr::summarise(
    n_students = dplyr::n(),
    n_courses = dplyr::n_distinct(course_id),
    n_course_terms = dplyr::n_distinct(course_term_id),
    grade_mean = mean(final_numeric_grade, na.rm = TRUE),
    grade_sd = stats::sd(final_numeric_grade, na.rm = TRUE),
    olse_mean = mean(olse_overall_mean, na.rm = TRUE),
    olse_sd = stats::sd(olse_overall_mean, na.rm = TRUE),
    olse_grade_cor = stats::cor(olse_overall_mean, final_numeric_grade, use = "pairwise.complete.obs")
  )

write_csv_local(analysis_summary, file.path(OUT_SEM, "sem_analysis_dataset_summary.csv"))

message("SEM analysis rows: ", nrow(sem_data))
message("Courses: ", dplyr::n_distinct(sem_data$course_id))
message("Course-term clusters: ", dplyr::n_distinct(sem_data$course_term_id))

# -------------------------------------------------------------------------
# Primary SEM: latent OLSE from five domain parcels and redesign composites
# -------------------------------------------------------------------------

primary_sem_model <- '
  OLSE =~ olse_q1_mean_z + olse_q2_mean_z + olse_q3_mean_z + olse_q4_mean_z + olse_q5_mean_z

  OLSE ~ a_post*post_redesign +
         a_m*post_x_mastery_z +
         a_s*post_x_social_z +
         a_c*post_x_structural_z

  final_numeric_grade_z ~ b*OLSE +
                          c_post*post_redesign +
                          c_m*post_x_mastery_z +
                          c_s*post_x_social_z +
                          c_c*post_x_structural_z

  ind_post := a_post*b
  ind_mastery := a_m*b
  ind_social := a_s*b
  ind_structural := a_c*b
  total_post := c_post + ind_post
  total_mastery := c_m + ind_mastery
  total_social := c_s + ind_social
  total_structural := c_c + ind_structural
'

message("Fitting primary latent OLSE parcel SEM.")
primary_fit <- fit_sem_safe(primary_sem_model, sem_data, "primary_latent_parcel_sem_cluster_course", cluster_var = "course_id")
write_csv_local(primary_fit$fit_table, file.path(OUT_SEM, "sem_primary_fit.csv"))
write_csv_local(primary_fit$params, file.path(OUT_SEM, "sem_primary_params.csv"))

# -------------------------------------------------------------------------
# Sensitivity: indirect-only latent model
# -------------------------------------------------------------------------

indirect_only_model <- '
  OLSE =~ olse_q1_mean_z + olse_q2_mean_z + olse_q3_mean_z + olse_q4_mean_z + olse_q5_mean_z

  OLSE ~ a_post*post_redesign +
         a_m*post_x_mastery_z +
         a_s*post_x_social_z +
         a_c*post_x_structural_z

  final_numeric_grade_z ~ b*OLSE

  ind_post := a_post*b
  ind_mastery := a_m*b
  ind_social := a_s*b
  ind_structural := a_c*b
'

message("Fitting indirect-only latent OLSE parcel SEM.")
indirect_fit <- fit_sem_safe(indirect_only_model, sem_data, "indirect_only_latent_parcel_sem", cluster_var = "course_id")
write_csv_local(indirect_fit$fit_table, file.path(OUT_SEM, "sem_indirect_only_fit.csv"))
write_csv_local(indirect_fit$params, file.path(OUT_SEM, "sem_indirect_only_params.csv"))

# -------------------------------------------------------------------------
# Sensitivity: manifest OLSE path model
# -------------------------------------------------------------------------

manifest_sem_model <- '
  olse_overall_z ~ a_post*post_redesign +
                   a_m*post_x_mastery_z +
                   a_s*post_x_social_z +
                   a_c*post_x_structural_z

  final_numeric_grade_z ~ b*olse_overall_z +
                          c_post*post_redesign +
                          c_m*post_x_mastery_z +
                          c_s*post_x_social_z +
                          c_c*post_x_structural_z

  ind_post := a_post*b
  ind_mastery := a_m*b
  ind_social := a_s*b
  ind_structural := a_c*b
  total_post := c_post + ind_post
  total_mastery := c_m + ind_mastery
  total_social := c_s + ind_social
  total_structural := c_c + ind_structural
'

message("Fitting manifest OLSE path model.")
manifest_fit <- fit_sem_safe(manifest_sem_model, sem_data, "manifest_olse_path_sem_cluster_course", cluster_var = "course_id")
write_csv_local(manifest_fit$fit_table, file.path(OUT_SEM, "sem_manifest_fit.csv"))
write_csv_local(manifest_fit$params, file.path(OUT_SEM, "sem_manifest_params.csv"))

# -------------------------------------------------------------------------
# Sensitivity: overall redesign-intensity latent model
# -------------------------------------------------------------------------

intensity_sem_model <- '
  OLSE =~ olse_q1_mean_z + olse_q2_mean_z + olse_q3_mean_z + olse_q4_mean_z + olse_q5_mean_z

  OLSE ~ a_post*post_redesign + a_i*post_x_intensity_z

  final_numeric_grade_z ~ b*OLSE + c_post*post_redesign + c_i*post_x_intensity_z

  ind_post := a_post*b
  ind_intensity := a_i*b
  total_post := c_post + ind_post
  total_intensity := c_i + ind_intensity
'

message("Fitting overall redesign-intensity SEM.")
intensity_fit <- fit_sem_safe(intensity_sem_model, sem_data, "overall_intensity_latent_parcel_sem", cluster_var = "course_id")
write_csv_local(intensity_fit$fit_table, file.path(OUT_SEM, "sem_intensity_fit.csv"))
write_csv_local(intensity_fit$params, file.path(OUT_SEM, "sem_intensity_params.csv"))

# -------------------------------------------------------------------------
# Sensitivity: one-cluster-at-a-time latent models
# -------------------------------------------------------------------------

message("Fitting one-cluster-at-a-time SEM sensitivity models.")
cluster_results <- list()
cluster_fits <- list()

for (k in 1:8) {
  xvar <- paste0("post_x_cluster_", k, "_z")
  model_name <- paste0("cluster_", k, "_one_at_a_time")

  if (!xvar %in% names(sem_data) || all(is.na(sem_data[[xvar]])) || stats::sd(sem_data[[xvar]], na.rm = TRUE) == 0) {
    cluster_results[[model_name]] <- tibble::tibble(model = model_name, status = "not_estimable", error = paste("No estimable variation in", xvar))
    cluster_fits[[model_name]] <- tibble::tibble(model = model_name, status = "not_estimable", error = paste("No estimable variation in", xvar))
    next
  }

  cluster_model <- paste0('
    OLSE =~ olse_q1_mean_z + olse_q2_mean_z + olse_q3_mean_z + olse_q4_mean_z + olse_q5_mean_z

    OLSE ~ a_post*post_redesign + a_k*', xvar, '

    final_numeric_grade_z ~ b*OLSE + c_post*post_redesign + c_k*', xvar, '

    ind_post := a_post*b
    ind_cluster := a_k*b
    total_post := c_post + ind_post
    total_cluster := c_k + ind_cluster
  ')

  res <- fit_sem_safe(cluster_model, sem_data, model_name, cluster_var = "course_id")
  cluster_results[[model_name]] <- res$params
  cluster_fits[[model_name]] <- res$fit_table
}

cluster_params <- dplyr::bind_rows(cluster_results)
cluster_fit_table <- dplyr::bind_rows(cluster_fits)

write_csv_local(cluster_params, file.path(OUT_SEM, "sem_cluster_one_at_a_time_params.csv"))
write_csv_local(cluster_fit_table, file.path(OUT_SEM, "sem_cluster_one_at_a_time_fit.csv"))


# -------------------------------------------------------------------------
# Combined SEM output tables
# -------------------------------------------------------------------------

fit_tables <- list(
  primary_clustered_sem = primary_fit$fit_table,
  indirect_only_sem = indirect_fit$fit_table,
  manifest_olse_sem = manifest_fit$fit_table,
  overall_intensity_sem = intensity_fit$fit_table,
  cluster_one_at_a_time_sem = cluster_fit_table
)

param_tables <- list(
  primary_clustered_sem = primary_fit$params,
  indirect_only_sem = indirect_fit$params,
  manifest_olse_sem = manifest_fit$params,
  overall_intensity_sem = intensity_fit$params,
  cluster_one_at_a_time_sem = cluster_params
)

all_fit_tables <- safe_bind_rows_named(fit_tables)
all_params <- safe_bind_rows_named(param_tables)

write_csv_local(all_fit_tables, file.path(OUT_SEM, "sem_all_model_fit_tables.csv"))
write_csv_local(all_params, file.path(OUT_SEM, "sem_all_model_parameters.csv"))

# -------------------------------------------------------------------------
# Compact paper appendix sensitivity summary
# -------------------------------------------------------------------------

sem_sensitivity_summary <- dplyr::bind_rows(
  param_subset(primary_fit$params, labels = c("a_post", "a_m", "a_s", "a_c", "b", "c_post"), model_label = "Primary latent OLSE parcel SEM"),
  param_subset(manifest_fit$params, labels = c("a_post", "a_m", "a_s", "a_c", "b", "c_post"), model_label = "Manifest OLSE path model"),
  param_subset(intensity_fit$params, labels = c("a_post", "a_i", "b", "c_post"), model_label = "Overall redesign intensity SEM")
) |>
  dplyr::select(dplyr::any_of(c("source_model", "lhs", "op", "rhs", "label", "est", "se", "pvalue", "ci.lower", "ci.upper", "std.all", "status", "error")))

write_csv_local(sem_sensitivity_summary, file.path(OUT_SEM, "sem_sensitivity_summary_for_paper.csv"))

# Optional Excel workbook for convenience.
if (requireNamespace("writexl", quietly = TRUE)) {
  try(
    writexl::write_xlsx(
      list(
        analysis_summary = to_plain_tibble(analysis_summary, "analysis_summary"),
        all_fit_tables = all_fit_tables,
        all_params = all_params,
        primary_params = to_plain_tibble(primary_fit$params, "primary_clustered_sem"),
        indirect_only_params = to_plain_tibble(indirect_fit$params, "indirect_only_sem"),
        manifest_params = to_plain_tibble(manifest_fit$params, "manifest_olse_sem"),
        intensity_params = to_plain_tibble(intensity_fit$params, "overall_intensity_sem"),
        cluster_sensitivity_params = to_plain_tibble(cluster_params, "cluster_one_at_a_time_sem")
      ),
      file.path(OUT_SEM, "project_soar_mechanism_sem_outputs_v4.xlsx")
    ),
    silent = TRUE
  )
}

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

message("")
message("SEM analysis summary:")
print(analysis_summary)

message("")
message("Primary SEM fit:")
print(primary_fit$fit_table)

message("")
message("Primary SEM key parameters:")
print(select_key_params(primary_fit$params), n = 80)

message("")
message("SEM sensitivity summary:")
print(sem_sensitivity_summary, n = 80)

message("")
message("04_sem_mechanism_models.R completed successfully.")
message("SEM outputs written to: ", OUT_SEM)
