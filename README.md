# LENS: Linked Analytics Workflow for Evaluating Course Redesign

This repository contains the reproducibility package for an IEEE DSAA conference paper on **LENS**, a linked institutional analytics workflow for evaluating online course redesign using fixed-effects models, OLSE measurement modeling, and structural equation modeling (SEM).

The repo is organized so that reviewers can reproduce the paper results in two ways:

1. **Canonical script pipeline**: run the numbered R scripts in `scripts/` from the repository root.
2. **Reviewer-facing notebook pipeline**: run the notebooks in `notebooks/`, which call the same R scripts but write artifacts to notebook-specific folders for comparison.

The scripts are the source of truth. The notebooks are literate wrappers around the scripts to make the workflow easier to inspect.

The notebook pipeline writes generated artifacts to outputs_notebooks/, figures_notebooks/, and outputs_comparison/ when run.



## Repository at a glance

```text
.
├── R/                         # Shared setup, path, and validation helpers
├── data/
│   ├── raw_public/            # Public/de-identified analysis CSV inputs
│   ├── processed/             # Script-generated processed data
│   └── processed_notebooks/   # Notebook-generated processed data
├── scripts/                   # Canonical numbered R pipeline
├── notebooks/                 # Reviewer-facing R notebooks
├── outputs/                   # Script-generated model/table outputs
├── outputs_notebooks/         # Notebook-generated model/table outputs
├── outputs_comparison/        # Script-vs-notebook comparison manifests
├── figures/                   # Script-generated figures
├── figures_notebooks/         # Notebook-generated figures
└── paper/                     # Paper PDF and table traceability notes
```



## What the workflow reproduces

The pipeline reproduces three analytic layers from the paper:

| Paper component | Question | Main scripts | Main outputs |
|---|---|---|---|
| RQ1 fixed-effects grade models | Are post-redesign offerings associated with higher final numeric grades after course and term adjustment? | `01_prepare_data.R`, `02_fixed_effects_models.R` | `outputs/fe_models/`, `outputs/paper_tables/table_iv_fe_results.csv`, `outputs/paper_tables/table_v_rq1_robustness.csv` |
| RQ2 OLSE measurement checks | Does the OLSE survey provide a reliable measurement structure for mechanism-oriented modeling? | `03_olse_cfa_measurement.R` | `outputs/olse_measurement/`, `outputs/paper_tables/table_vi_olse_reliability.csv`, `outputs/paper_tables/appendix_table_xi_cfa_comparison.csv` |
| RQ3 SEM mechanism models | Are redesign variables associated with grades through OLSE-consistent pathways? | `04_sem_mechanism_model.R` | `outputs/sem_models/`, `outputs/paper_tables/table_vii_sem_paths.csv`, `outputs/paper_tables/table_viii_indirect_associations.csv` |
| Paper tables and figures | Generate publication-facing tables and figures from the analysis outputs | `05_make_paper_tables.R`, `06_make_figures.R` | `outputs/paper_tables/`, `figures/` |



## Input data

The public analysis inputs are in `data/raw_public/`:

| File | Unit | Purpose |
|---|---|---|
| `student_course.csv` | Student-course observation | Final numeric grade, OLSE item responses, OLSE domain means, course/term/section identifiers, redesign status |
| `course_term.csv` | Course-section-term row | Course offering metadata, enrollment summaries, average grade/OLSE fields, redesign status |
| `course_redesign.csv` | Course-level redesign record | Pre/post course-review scores, eight redesign-cluster deltas, three redesign composites, overall redesign intensity |

The analysis file contains 1,568 student-course observations across 19 courses, 67 course-term cells, and 268 section-term rows.

Identifiers in the public release are de-identified. The repository is intended for reproducing aggregate/model outputs, not for individual-level prediction or decision-making.



## Software requirements

The analysis is written in R. The main packages used by the scripts include:

```r
c(
  "readr", "dplyr", "tidyr", "stringr", "purrr", "tibble", "janitor",
  "broom", "fixest", "modelsummary", "clubSandwich", "writexl",
  "ggplot2", "psych", "lavaan", "semTools"
)
```



## Reproduce results with the canonical script pipeline

Run all scripts from the repository root in numeric order:

```bash
Rscript scripts/01_prepare_data.R
Rscript scripts/02_fixed_effects_models.R
Rscript scripts/03_olse_cfa_measurement.R
Rscript scripts/04_sem_mechanism_model.R
Rscript scripts/04b_sem_power_analysis.R
Rscript scripts/04c_baseline_ablation_comparisons.R
Rscript scripts/05_make_paper_tables.R
Rscript scripts/06_make_figures.R
```

Expected script-generated folders:

```text
data/processed/
outputs/fe_models/
outputs/olse_measurement/
outputs/sem_models/
outputs/paper_tables/
figures/
```



## Reproduce results with notebooks

The notebook workflow is designed for reviewers who prefer a guided, inspectable run.

Run:

```text
notebooks/00_run_all_pipeline_with_comparison.ipynb
```

This notebook calls the same six scripts but redirects outputs to notebook-specific folders:

```text
data/processed_notebooks/
outputs_notebooks/
figures_notebooks/
```

It then writes a comparison manifest to:

```text
outputs_comparison/00_full_pipeline_comparison.csv
```

The modular notebooks can also be run in order:

```text
01_prepare_data.ipynb
02_fixed_effects_models.ipynb
03_olse_cfa_measurement.ipynb
04_sem_mechanism_model.ipynb
```

See `notebooks/README.md` for details.



## Script-vs-notebook reproducibility check

The repository keeps script outputs and notebook outputs separate so reviewers can compare them side by side.

| Output type | Script pipeline | Notebook pipeline |
|---|---|---|
| Processed data | `data/processed/` | `data/processed_notebooks/` |
| Model/table outputs | `outputs/` | `outputs_notebooks/` |
| Figures | `figures/` | `figures_notebooks/` |
| Comparison manifest | - | `outputs_comparison/00_full_pipeline_comparison.csv` |

Interpretation of the manifest:

- `match`: exact byte-level match.
- `different`: files differ by MD5 checksum. For CSVs, this may indicate a substantive difference and should be inspected. For XLSX/HTML outputs, differences can arise from timestamps or volatile generated IDs even when the underlying tables match.
- `missing_*`: one side of the comparison did not produce the expected file.

In the final checked run, the substantive CSV/table outputs matched; remaining byte-level differences were attributable to generated workbook metadata, volatile HTML identifiers, the expected `figures/` versus `figures_notebooks/` path difference in the figure inventory, and one extra script-side identifier column in a processed SEM helper file.



## Paper table traceability

Generated paper-facing tables are stored in:

```text
outputs/paper_tables/
```

The most important files are:

| Paper table | Generated source |
|---|---|
| Table III: dataset summary | `outputs/paper_tables/table_iii_dataset_summary.csv` |
| Table IV: fixed-effects grade model results | `outputs/paper_tables/table_iv_fe_results.csv` |
| Table V: RQ1 robustness checks | `outputs/paper_tables/table_v_rq1_robustness.csv` |
| Table VI: OLSE reliability estimates | `outputs/paper_tables/table_vi_olse_reliability.csv` |
| Table VII: SEM structural paths | `outputs/paper_tables/table_vii_sem_paths.csv` |
| Table VIII: indirect associations through OLSE | `outputs/paper_tables/table_viii_indirect_associations.csv` |
| Appendix Table X: SEM sensitivity checks | `outputs/paper_tables/appendix_table_x_sem_sensitivity.csv` |
| Appendix Table XI: OLSE CFA model comparison | `outputs/paper_tables/appendix_table_xi_cfa_comparison.csv` |

See `paper/table_traceability.md` for a reviewer-facing crosswalk between paper tables, scripts, and generated files.



## Key result files for reviewers

Reviewers who want to check the main numeric claims can start with these files:

```text
outputs/paper_tables/table_iii_dataset_summary.csv
outputs/paper_tables/table_iv_fe_results.csv
outputs/paper_tables/table_v_rq1_robustness.csv
outputs/paper_tables/table_vi_olse_reliability.csv
outputs/paper_tables/table_vii_sem_paths.csv
outputs/paper_tables/table_viii_indirect_associations.csv
outputs/paper_tables/appendix_table_x_sem_sensitivity.csv
outputs/paper_tables/appendix_table_xi_cfa_comparison.csv
outputs_comparison/00_full_pipeline_comparison.csv
```



## Privacy and intended use

This repository is for reproducible institutional research and paper review. The public files use de-identified identifiers and should be used only to reproduce aggregate summaries, models, tables, and figures. The workflow should not be used for individual-level student prediction, intervention automation, or re-identification.



## Citation

If citing this repository, cite the associated IEEE DSAA paper and include the GitHub release/commit hash used for reproduction.
