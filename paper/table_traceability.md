# Paper table and figure traceability

This file maps paper tables and figures to the generated outputs in the reproducibility package. It is intended to help reviewers verify that the manuscript values can be traced to executable code.

---

## How to use this file

1. Run the canonical script pipeline from the repository root.
2. Confirm that `outputs/paper_tables/` and `figures/` are regenerated.
3. Compare the paper values against the source files listed below.
4. Optionally run `notebooks/00_run_all_pipeline_with_comparison.ipynb` to generate notebook-side outputs and `outputs_comparison/00_full_pipeline_comparison.csv`.

---

## Main paper tables

| Paper table | Description | Generated source | Script |
|---|---|---|---|
| Table I | Linked data layers used in the LENS workflow | Authored in paper text | Not generated |
| Table II | Mapping from Legon clusters to redesign composites | Authored in paper text | Not generated |
| Table III | Dataset summary and pre/post descriptive statistics | `outputs/paper_tables/table_iii_dataset_summary.csv` | `scripts/05_make_paper_tables.R` |
| Table IV | Fixed-effects grade model results | `outputs/paper_tables/table_iv_fe_results.csv` | `scripts/05_make_paper_tables.R` |
| Table V | RQ1 robustness checks | `outputs/paper_tables/table_v_rq1_robustness.csv` | `scripts/05_make_paper_tables.R` |
| Table VI | OLSE reliability estimates | `outputs/paper_tables/table_vi_olse_reliability.csv` | `scripts/05_make_paper_tables.R` |
| Table VII | Primary SEM structural paths | `outputs/paper_tables/table_vii_sem_paths.csv` | `scripts/05_make_paper_tables.R` |
| Table VIII | Indirect associations through OLSE | `outputs/paper_tables/table_viii_indirect_associations.csv` | `scripts/05_make_paper_tables.R` |
| Appendix Table X | SEM sensitivity checks | `outputs/paper_tables/appendix_table_x_sem_sensitivity.csv` | `scripts/05_make_paper_tables.R` |
| Appendix Table XI | OLSE CFA model comparison | `outputs/paper_tables/appendix_table_xi_cfa_comparison.csv` plus chi-square values from `outputs/olse_measurement/olse_cfa_fit_comparison.csv` | `scripts/03_olse_cfa_measurement.R`, `scripts/05_make_paper_tables.R` |

---

## Key current values to verify in the paper

### Table III: dataset summary

Source: `outputs/paper_tables/table_iii_dataset_summary.csv`

| Quantity | Current generated value |
|---|---:|
| Student-course observations | 1,568 |
| Courses | 19 |
| Course-term cells | 67 |
| Section-term rows | 268 |
| Pre-redesign observations | 942 |
| Post-redesign observations | 626 |
| Mean pre-redesign grade | 2.456 |
| Mean post-redesign grade | 2.715 |
| Mean pre-redesign OLSE | 3.429 |
| Mean post-redesign OLSE | 3.597 |

Important manuscript check: use **67 course-term cells**, not 76.

### Table IV: fixed-effects grade model results

Source: `outputs/paper_tables/table_iv_fe_results.csv`

| Model | Est. | SE | p |
|---|---:|---:|---:|
| Course + term FE | 0.380 | 0.118 | 0.0048 |
| Grade z-score outcome | 0.478 | 0.149 | 0.0048 |
| Course + term + instructor | 0.387 | 0.119 | 0.0044 |
| Course-term aggregate | 0.380 | 0.123 | 0.0062 |

### Table V: RQ1 robustness checks

Source: `outputs/paper_tables/table_v_rq1_robustness.csv`

| Check | Current generated value |
|---|---|
| CR2/Satterthwaite | beta = 0.380, SE = 0.124, df = 15.44, p = 0.0075 |
| Leave-one-course-out | All estimates positive; beta range = 0.337-0.438 |
| Instructor-adjusted FE | beta = 0.387, SE = 0.119, p = 0.0044 |
| Course-term aggregate | beta = 0.380, SE = 0.123, p = 0.0062 |

### Table VI: OLSE reliability estimates

Source: `outputs/paper_tables/table_vi_olse_reliability.csv`

| Scale | Items | alpha |
|---|---:|---:|
| OLSE Domain Q1 | 8 | 0.785 |
| OLSE Domain Q2 | 5 | 0.706 |
| OLSE Domain Q3 | 6 | 0.738 |
| OLSE Domain Q4 | 5 | 0.695 |
| OLSE Domain Q5 | 6 | 0.743 |
| Overall OLSE scale | 30 | 0.932 |

### Table VII: primary SEM structural paths

Source: `outputs/paper_tables/table_vii_sem_paths.csv`

| Path | Est. | SE | p |
|---|---:|---:|---:|
| OLSE <- post-redesign | 0.421 | 0.080 | < .001 |
| OLSE <- post x mastery | 0.228 | 0.081 | 0.005 |
| OLSE <- post x social/vicarious | 0.445 | 0.055 | < .001 |
| OLSE <- post x structural/compliance | 0.323 | 0.070 | < .001 |
| Grade <- OLSE | 0.373 | 0.032 | < .001 |
| Grade <- post-redesign | 0.197 | 0.049 | < .001 |

Important manuscript check: the direct grade path should be **0.197**, not 0.291.

### Table VIII: indirect associations through OLSE

Source: `outputs/paper_tables/table_viii_indirect_associations.csv`

| Indirect association | Est. | SE | p |
|---|---:|---:|---:|
| Post -> OLSE -> grade | 0.157 | 0.034 | < .001 |
| Mastery -> OLSE -> grade | 0.085 | 0.034 | 0.012 |
| Social/vicarious -> OLSE -> grade | 0.166 | 0.022 | < .001 |
| Structural/compliance -> OLSE -> grade | 0.120 | 0.028 | < .001 |

---

## Appendix tables

### Appendix Table X: SEM sensitivity checks

Source: `outputs/paper_tables/appendix_table_x_sem_sensitivity.csv`

| Model | Key result |
|---|---|
| Latent OLSE parcel SEM | Post -> OLSE -> grade = 0.157, p < .001 |
| Manifest OLSE path model | Post -> OLSE -> grade = 0.145, p < .001 |
| Redesign-intensity SEM | Intensity -> OLSE -> grade = 0.089, p = 0.0048 |

### Appendix Table XI: OLSE CFA model comparison

Sources:

- `outputs/paper_tables/appendix_table_xi_cfa_comparison.csv`
- `outputs/olse_measurement/olse_cfa_fit_comparison.csv` for scaled chi-square values

| Model | chi-square | df | p | CFI | TLI | RMSEA | SRMR |
|---|---:|---:|---:|---:|---:|---:|---:|
| Five-domain correlated CFA | 442.746 | 395 | 0.049 | 0.998 | 0.998 | 0.009 | 0.019 |
| Higher-order CFA | 445.796 | 400 | 0.057 | 0.998 | 0.998 | 0.009 | 0.019 |
| Single-factor CFA | 455.155 | 405 | 0.043 | 0.998 | 0.998 | 0.009 | 0.020 |

---

## Figures

| Paper figure | Generated source | Script |
|---|---|---|
| LENS workflow diagram | `figures/linked_data_architecture` if maintained as LaTeX/TikZ in the paper source | Paper source |
| Treatment timing by course | `figures/fig_01_treatment_timing_by_course.png` | `scripts/06_make_figures.R` |
| Pre/post grade and OLSE summary | `figures/fig_02_prepost_grade_olse_summary.png` | `scripts/06_make_figures.R` |
| Leave-one-course-out FE robustness | `figures/fig_03_leave_one_course_out_fe.png` | `scripts/06_make_figures.R` |
| SEM indirect associations | `figures/fig_04_sem_indirect_associations.png` | `scripts/06_make_figures.R` |

---
