# Shared input/output and validation helpers for the LENS reproducibility repo.

term_to_index <- function(term_vec) {
  term_vec <- as.character(term_vec)
  yr <- suppressWarnings(as.integer(stringr::str_extract(term_vec, "\\d{4}")))
  sem <- stringr::str_to_upper(stringr::str_extract(term_vec, "SP|SPRING|SU|SUMMER|F|FA|FALL"))
  sem_num <- dplyr::case_when(
    sem %in% c("SP", "SPRING") ~ 1L,
    sem %in% c("SU", "SUMMER") ~ 2L,
    sem %in% c("F", "FA", "FALL") ~ 3L,
    TRUE ~ 9L
  )
  yr * 10L + sem_num
}

grade_points <- c(
  "A" = 4.0, "A-" = 3.7, "B+" = 3.3, "B" = 3.0, "B-" = 2.7,
  "C+" = 2.3, "C" = 2.0, "C-" = 1.7, "D+" = 1.3,
  "D" = 1.0, "D-" = 0.7, "F" = 0.0
)

point_to_letter <- function(x) {
  names(grade_points)[vapply(x, function(v) {
    if (is.na(v)) return(NA_integer_)
    which.min(abs(v - grade_points))
  }, integer(1))]
}

safe_scale <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  as.numeric(scale(x))
}

safe_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path, na = "")
  message("Wrote: ", path)
}

read_public_data <- function() {
  files <- list(
    student_course = file.path(DATA_RAW, "student_course.csv"),
    course_term = file.path(DATA_RAW, "course_term.csv"),
    course_redesign = file.path(DATA_RAW, "course_redesign.csv")
  )

  missing_files <- names(files)[!file.exists(unlist(files))]
  if (length(missing_files) > 0) {
    stop(
      "Missing public data files in data/raw_public/: ",
      paste(paste0(missing_files, " -> ", unlist(files)[missing_files]), collapse = "; "),
      call. = FALSE
    )
  }

  list(
    student_course = readr::read_csv(files$student_course, show_col_types = FALSE) |>
      janitor::clean_names(),
    course_term = readr::read_csv(files$course_term, show_col_types = FALSE) |>
      janitor::clean_names(),
    course_redesign = readr::read_csv(files$course_redesign, show_col_types = FALSE) |>
      janitor::clean_names()
  )
}

validate_inputs <- function(student_course, course_term, course_redesign) {
  required_student <- c(
    "student_id", "course_id", "term", "section_id", "instructor_id",
    "post_redesign", "redesign_status", "final_numeric_grade",
    "final_grade_letter_sampled", "olse_overall_mean"
  )
  required_course_term <- c(
    "course_id", "term", "section_id", "instructor_id", "post_redesign", "redesign_status"
  )
  required_redesign <- c(
    "course_id", "mastery_redesign", "social_vicarious_redesign",
    "structural_compliance_redesign", "redesign_intensity"
  )
  olse_items <- c(
    paste0("q1_", 1:8),
    paste0("q2_", 1:5),
    paste0("q3_", 1:6),
    paste0("q4_", 1:5),
    paste0("q5_", 1:6)
  )

  missing_student <- setdiff(c(required_student, olse_items), names(student_course))
  missing_course_term <- setdiff(required_course_term, names(course_term))
  missing_redesign <- setdiff(required_redesign, names(course_redesign))

  if (length(missing_student) > 0) stop("Missing student_course columns: ", paste(missing_student, collapse = ", "), call. = FALSE)
  if (length(missing_course_term) > 0) stop("Missing course_term columns: ", paste(missing_course_term, collapse = ", "), call. = FALSE)
  if (length(missing_redesign) > 0) stop("Missing course_redesign columns: ", paste(missing_redesign, collapse = ", "), call. = FALSE)

  # Strict checks for the current DSAA public analysis files.
  stopifnot(nrow(student_course) == 1568)
  stopifnot(dplyr::n_distinct(student_course$course_id) == 19)
  stopifnot(nrow(course_term) == 268)
  stopifnot(nrow(course_redesign) == 19)

  invisible(TRUE)
}
