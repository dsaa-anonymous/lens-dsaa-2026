# Notebooks

The notebooks provide a reviewer-friendly way to reproduce the LENS DSAA analysis pipeline while keeping the existing `scripts/` directory as the canonical source of truth.

The notebooks do not duplicate model logic. They call the same R scripts used by the command-line pipeline.

---

## Notebook outputs do not overwrite script outputs

The command-line R scripts write to the canonical locations by default:

```text
data/processed/
outputs/
figures/
```

The notebooks set environment variables before sourcing the same scripts, so notebook-generated artifacts are written separately:

```text
data/processed_notebooks/
outputs_notebooks/
figures_notebooks/
```

Comparison manifests are written to:

```text
outputs_comparison/
```

This design lets reviewers compare command-line script outputs against notebook outputs without overwriting either set.

---

## Recommended reviewer path

Start here:

```text
00_run_all_pipeline_with_comparison.ipynb
```

This notebook runs the full six-script pipeline and writes:

```text
outputs_comparison/00_full_pipeline_comparison.csv
```

The comparison manifest records script paths, notebook paths, MD5 hashes, and match/difference status for expected artifacts.

---

## Notebook inventory

| Notebook | Purpose | Main outputs |
|---|---|---|
| `00_run_all_pipeline_with_comparison.ipynb` | End-to-end reproduction and output comparison | `data/processed_notebooks/`, `outputs_notebooks/`, `figures_notebooks/`, `outputs_comparison/00_full_pipeline_comparison.csv` |
| `01_prepare_data.ipynb` | Validate raw public inputs and create processed analysis files | `data/processed_notebooks/` |
| `02_fixed_effects_models.ipynb` | Reproduce RQ1 fixed-effects grade models and robustness checks | `outputs_notebooks/fe_models/` |
| `03_olse_cfa_measurement.ipynb` | Reproduce RQ2 OLSE reliability, CFA, measurement comparability, and SEM-ready scoring | `outputs_notebooks/olse_measurement/`, `data/processed_notebooks/olse_scored_for_sem.csv` |
| `04_sem_mechanism_model.ipynb` | Reproduce RQ3 SEM mechanism models and sensitivity checks | `outputs_notebooks/sem_models/` |

There is intentionally no separate paper-tables-and-figures notebook. The full-pipeline notebook runs `scripts/05_make_paper_tables.R` and `scripts/06_make_figures.R` as the downstream publication-output stage.

---

## How to run

1. Open the repository root in RStudio, VS Code, Jupyter, or another environment with an R kernel.
2. Make sure the required R packages are installed. See the root `README.md` for the package list.
3. Run `00_run_all_pipeline_with_comparison.ipynb` from top to bottom.

The notebooks try to locate the project root automatically, so they can be launched from either the root directory or from inside `notebooks/`.

---

## Interpreting comparison results

Open:

```text
outputs_comparison/00_full_pipeline_comparison.csv
```

The most important column is `status`:

| Status | Meaning |
|---|---|
| `match` | Script and notebook files are exact byte-level matches |
| `different` | Files differ by MD5 checksum and should be interpreted by file type |
| `missing_script` | Expected script-side output is missing |
| `missing_notebook` | Expected notebook-side output is missing |

CSV differences are usually the most important to inspect. XLSX and HTML outputs can differ because of timestamps or generated IDs even when the underlying table values match. Figure inventory files are expected to differ in path strings because they point to `figures/` versus `figures_notebooks/`.

---

## Design rationale

For conference-review reproducibility, the safest pattern is:

- Keep scripts as the authoritative executable pipeline.
- Use notebooks as transparent wrappers for review, not as a second implementation.
- Write notebook artifacts to separate folders.
- Produce a comparison manifest that makes output drift visible.

This avoids script/notebook divergence while still giving reviewers a guided execution path.
