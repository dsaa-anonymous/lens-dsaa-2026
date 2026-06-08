# scripts/06_make_figures.R
# Generate reviewer-friendly figures for the LENS DSAA 2026 reproducibility repo.
#
# Run from repo root:
#   Rscript scripts/06_make_figures.R
#
# Expected prior steps:
#   Rscript scripts/01_prepare_data.R
#   Rscript scripts/02_fixed_effects_models.R
#   Rscript scripts/03_olse_cfa_measurement.R
#   Rscript scripts/04_sem_mechanism_models.R
#   Rscript scripts/05_make_paper_tables.R
#
# Figures written to:
#   figures/

options(stringsAsFactors = FALSE)

# -------------------------------------------------------------------------
# 1. Project-root detection
# -------------------------------------------------------------------------

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]

script_path <- if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)
} else {
  normalizePath("scripts/06_make_figures.R", mustWork = TRUE)
}

PROJECT_ROOT <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

source(file.path("R", "00_packages.R"))
source(file.path("R", "00_paths.R"))
source(file.path("R", "utils_io.R"))

# -------------------------------------------------------------------------
# 3. Paths
# -------------------------------------------------------------------------

# Paths are supplied by R/00_paths.R. By default they point to data/processed,
# outputs/, and figures/. Notebooks can redirect them with LENS_* environment
# variables so notebook-generated artifacts do not overwrite script outputs.
dir.create(OUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

message("Project root: ", PROJECT_ROOT)
message("Reading processed data from: ", DATA_PROCESSED)
message("Reading FE outputs from: ", OUT_FE)
message("Reading OLSE/CFA outputs from: ", OUT_OLSE)
message("Reading SEM outputs from: ", OUT_SEM)
message("Reading paper tables from: ", OUT_TABLES)
message("Figures directory: ", OUT_FIGURES)

# -------------------------------------------------------------------------
# 4. Helper functions
# -------------------------------------------------------------------------

read_required_csv <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Required input file not found: ", path,
      "\nRun the earlier pipeline scripts before running 06_make_figures.R."
    )
  }

  readr::read_csv(path, show_col_types = FALSE)
}

save_figure <- function(plot, filename_base, width = 8, height = 5) {
  png_path <- file.path(OUT_FIGURES, paste0(filename_base, ".png"))

  ggplot2::ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )

  message("Wrote: ", png_path)
}

clean_label <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_trim() |>
    stringr::str_to_sentence()
}

base_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "bold")
    )
}

# -------------------------------------------------------------------------
# 5. Figure 1: Treatment timing by course and term
# -------------------------------------------------------------------------

student_term <- read_required_csv(
  file.path(DATA_PROCESSED, "student_term_fe_analysis.csv")
)

required_timing_cols <- c("course_id", "term", "term_index", "post_redesign")

missing_timing_cols <- setdiff(required_timing_cols, names(student_term))

if (length(missing_timing_cols) > 0) {
  stop(
    "Cannot create Figure 1. Missing columns in student_term_fe_analysis.csv: ",
    paste(missing_timing_cols, collapse = ", ")
  )
}

treatment_timing <- student_term |>
  dplyr::distinct(course_id, term, term_index, post_redesign) |>
  dplyr::mutate(
    term_label = as.character(term),
    status = dplyr::case_when(
      post_redesign == 0 ~ "Pre-redesign",
      post_redesign == 1 ~ "Post-redesign",
      TRUE ~ "Unknown"
    )
  )

term_levels <- treatment_timing |>
  dplyr::arrange(term_index) |>
  dplyr::distinct(term_label) |>
  dplyr::pull(term_label)

course_levels <- treatment_timing |>
  dplyr::group_by(course_id) |>
  dplyr::summarise(
    first_post_index = suppressWarnings(min(term_index[post_redesign == 1], na.rm = TRUE)),
    first_term_index = min(term_index, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    first_post_index = ifelse(is.infinite(first_post_index), Inf, first_post_index)
  ) |>
  dplyr::arrange(first_post_index, first_term_index, course_id) |>
  dplyr::pull(course_id) |>
  as.character()

treatment_timing <- treatment_timing |>
  dplyr::mutate(
    term_label = factor(term_label, levels = term_levels),
    course_id = factor(as.character(course_id), levels = rev(course_levels)),
    status = factor(status, levels = c("Pre-redesign", "Post-redesign", "Unknown"))
  )

fig_01 <- ggplot2::ggplot(
  treatment_timing,
  ggplot2::aes(x = term_label, y = course_id, fill = status)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.4) +
  ggplot2::labs(
    title = "Treatment timing by course and term",
    subtitle = "Each tile represents whether a course-term observation is pre- or post-redesign.",
    x = "Term",
    y = "Course",
    fill = "Status"
  ) +
  base_theme() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

save_figure(fig_01, "fig_01_treatment_timing_by_course", width = 8.5, height = 6)

# -------------------------------------------------------------------------
# 6. Figure 2: Pre/post grade and OLSE descriptive summary
# -------------------------------------------------------------------------

desc_prepost <- read_required_csv(
  file.path(OUT_FE, "descriptive_prepost_summary.csv")
)

required_desc_cols <- c("post_redesign", "grade_mean", "olse_mean")
missing_desc_cols <- setdiff(required_desc_cols, names(desc_prepost))

if (length(missing_desc_cols) > 0) {
  stop(
    "Cannot create Figure 2. Missing columns in descriptive_prepost_summary.csv: ",
    paste(missing_desc_cols, collapse = ", ")
  )
}

desc_long <- desc_prepost |>
  dplyr::mutate(
    status = dplyr::case_when(
      post_redesign == 0 ~ "Pre-redesign",
      post_redesign == 1 ~ "Post-redesign",
      TRUE ~ as.character(post_redesign)
    )
  ) |>
  dplyr::select(status, grade_mean, olse_mean) |>
  tidyr::pivot_longer(
    cols = c(grade_mean, olse_mean),
    names_to = "measure",
    values_to = "mean_value"
  ) |>
  dplyr::mutate(
    measure = dplyr::case_when(
      measure == "grade_mean" ~ "Final numeric grade",
      measure == "olse_mean" ~ "OLSE mean",
      TRUE ~ clean_label(measure)
    ),
    status = factor(status, levels = c("Pre-redesign", "Post-redesign"))
  )

fig_02 <- ggplot2::ggplot(
  desc_long,
  ggplot2::aes(x = status, y = mean_value, fill = status)
) +
  ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
  ggplot2::facet_wrap(~ measure, scales = "free_y") +
  ggplot2::labs(
    title = "Pre/post descriptive summary",
    subtitle = "Mean final numeric grade and mean OLSE by redesign status.",
    x = NULL,
    y = "Mean"
  ) +
  base_theme()

save_figure(fig_02, "fig_02_prepost_grade_olse_summary", width = 7.5, height = 4.5)

# -------------------------------------------------------------------------
# 7. Figure 3: Leave-one-course-out FE robustness
# -------------------------------------------------------------------------

loo <- read_required_csv(
  file.path(OUT_FE, "leave_one_course_out_fe.csv")
)

required_loo_cols <- c("dropped_course", "estimate")
missing_loo_cols <- setdiff(required_loo_cols, names(loo))

if (length(missing_loo_cols) > 0) {
  stop(
    "Cannot create Figure 3. Missing columns in leave_one_course_out_fe.csv: ",
    paste(missing_loo_cols, collapse = ", ")
  )
}

if (!("conf.low" %in% names(loo)) || !("conf.high" %in% names(loo))) {
  if ("std.error" %in% names(loo)) {
    loo <- loo |>
      dplyr::mutate(
        conf.low = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error
      )
  } else {
    loo <- loo |>
      dplyr::mutate(
        conf.low = NA_real_,
        conf.high = NA_real_
      )
  }
}

primary_fe <- read_required_csv(
  file.path(OUT_FE, "project_soar_fe_grade_coefficients.csv")
)

primary_estimate <- primary_fe |>
  dplyr::filter(
    model == "Course + term FE",
    term == "post_redesign"
  ) |>
  dplyr::slice(1) |>
  dplyr::pull(estimate)

if (length(primary_estimate) == 0) {
  primary_estimate <- NA_real_
}

loo_plot_data <- loo |>
  dplyr::mutate(
    dropped_course = factor(
      as.character(dropped_course),
      levels = as.character(dropped_course[order(estimate)])
    )
  )

fig_03 <- ggplot2::ggplot(
  loo_plot_data,
  ggplot2::aes(x = estimate, y = dropped_course)
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  {
    if (!is.na(primary_estimate)) {
      ggplot2::geom_vline(
        xintercept = primary_estimate,
        linetype = "solid",
        linewidth = 0.5
      )
    }
  } +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    na.rm = TRUE
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(
    title = "Leave-one-course-out robustness",
    subtitle = "Post-redesign fixed-effects grade estimate after dropping each course.",
    x = "Estimated post-redesign grade association",
    y = "Dropped course"
  ) +
  base_theme()

save_figure(fig_03, "fig_03_leave_one_course_out_fe", width = 8, height = 6)

# -------------------------------------------------------------------------
# 8. Figure 4: SEM indirect associations through OLSE
# -------------------------------------------------------------------------

table_viii_path <- file.path(OUT_TABLES, "table_viii_indirect_associations.csv")

if (file.exists(table_viii_path)) {
  indirects <- readr::read_csv(table_viii_path, show_col_types = FALSE)
} else {
  message("table_viii_indirect_associations.csv not found. Falling back to sem_primary_params.csv.")
  indirects <- read_required_csv(file.path(OUT_SEM, "sem_primary_params.csv"))
}

# Try to standardize common possible column names from Script 05 or lavaan output.
names_lower <- tolower(names(indirects))

path_col <- names(indirects)[
  match(TRUE, names_lower %in% c("indirect_association", "association", "path", "label", "lhs"), nomatch = 0)
]

estimate_col <- names(indirects)[
  match(TRUE, names_lower %in% c("estimate", "est", "std_all", "std.all"), nomatch = 0)
]

se_col <- names(indirects)[
  match(TRUE, names_lower %in% c("std_error", "std.error", "se"), nomatch = 0)
]

p_col <- names(indirects)[
  match(TRUE, names_lower %in% c("p_value", "p.value", "pvalue"), nomatch = 0)
]

if (length(path_col) == 0 || length(estimate_col) == 0) {
  stop(
    "Cannot create Figure 4. Could not identify path and estimate columns in indirect-association file.\n",
    "Available columns: ", paste(names(indirects), collapse = ", ")
  )
}

indirect_plot_data <- indirects |>
  dplyr::mutate(
    figure_path = .data[[path_col]],
    figure_estimate = as.numeric(.data[[estimate_col]])
  )

if (!is.null(se_col) && length(se_col) > 0 && se_col %in% names(indirect_plot_data)) {
  indirect_plot_data <- indirect_plot_data |>
    dplyr::mutate(
      figure_se = as.numeric(.data[[se_col]]),
      conf.low = figure_estimate - 1.96 * figure_se,
      conf.high = figure_estimate + 1.96 * figure_se
    )
} else {
  indirect_plot_data <- indirect_plot_data |>
    dplyr::mutate(
      figure_se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_
    )
}

# If the fallback lavaan file includes many rows, retain only indirect-style rows when possible.
if ("op" %in% names(indirect_plot_data)) {
  indirect_plot_data <- indirect_plot_data |>
    dplyr::filter(op == ":=" | stringr::str_detect(tolower(as.character(figure_path)), "indirect|olse"))
}

indirect_plot_data <- indirect_plot_data |>
  dplyr::filter(!is.na(figure_estimate)) |>
  dplyr::mutate(
    figure_path = clean_label(figure_path),
    figure_path = factor(
      figure_path,
      levels = figure_path[order(figure_estimate)]
    )
  )

if (nrow(indirect_plot_data) == 0) {
  stop("Cannot create Figure 4. No indirect-association rows available after filtering.")
}

fig_04 <- ggplot2::ggplot(
  indirect_plot_data,
  ggplot2::aes(x = figure_estimate, y = figure_path)
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    na.rm = TRUE
  ) +
  ggplot2::geom_point(size = 2.3) +
  ggplot2::labs(
    title = "SEM indirect associations through OLSE",
    subtitle = "Estimated redesign-to-grade indirect associations through OLSE.",
    x = "Indirect association estimate",
    y = NULL
  ) +
  base_theme()

save_figure(fig_04, "fig_04_sem_indirect_associations", width = 8, height = 5)

# -------------------------------------------------------------------------
# 9. Write figure inventory
# -------------------------------------------------------------------------

figure_inventory <- tibble::tibble(
  figure = c(
    "fig_01_treatment_timing_by_course",
    "fig_02_prepost_grade_olse_summary",
    "fig_03_leave_one_course_out_fe",
    "fig_04_sem_indirect_associations"
  ),
  png = file.path(basename(OUT_FIGURES), paste0(figure, ".png")),
  source_files = c(
    "data/processed/student_term_fe_analysis.csv",
    "outputs/fe_models/descriptive_prepost_summary.csv",
    "outputs/fe_models/leave_one_course_out_fe.csv; outputs/fe_models/project_soar_fe_grade_coefficients.csv",
    "outputs/paper_tables/table_viii_indirect_associations.csv"
  )
)

readr::write_csv(
  figure_inventory,
  file.path(OUT_FIGURES, "figure_inventory.csv"),
  na = ""
)

message("Wrote: ", file.path(OUT_FIGURES, "figure_inventory.csv"))
message("")
message("06_make_figures.R completed successfully.")