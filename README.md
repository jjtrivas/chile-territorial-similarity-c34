# Computational Integration of Socio-Environmental and Oncological Data Through a Territorial Similarity Network in Chilean Communes

[![R](https://img.shields.io/badge/analysis-R-blue)](https://www.r-project.org/)

**Territorial similarity networks · Chile · Lung cancer (ICD-10 C34)**  
**V Reunión Anual de la Sociedad Chilena de Bioinformática — November 2026**  
**Repository status:** Audited outputs and a *processed-data* workflow provided for independent local testing. Full reconstruction from official raw sources is **in progress**.

## Overview

Exploratory ecological analysis of **18 Chilean communes** with socio-environmental, regulatory, social/housing, demographic and healthcare-access indicators. Three network specifications contrast the inclusion of crude or age-adjusted C34 measures with a non-oncological reference network. The networks express **multidimensional similarity**, not geographical distance, disease transmission, causal relationships or individual cancer risk.

## Network specifications and archived results

| Network | Specification | Nodes | Edges | Components | Isolates | Louvain modularity (descriptive) |
|---|---|---:|---:|---:|---:|---:|
| A | Includes crude C34 | 18 | 61 | 4 | 2 | 0.2161308 |
| B | Excludes C34 | 18 | 61 | 4 | 2 | 0.2840162 |
| A_ajustada | Includes age-adjusted C34 | 18 | 61 | 4 | 2 | 0.2150323 |

The reported dataset contains **1,864 C34 cases** over **14,916,395 person-years** across the selected communes and their registry coverage windows. Six C34 correspondence contrasts are archived in `results/Tabla_correspondencia_C34.csv`; **none is significant after Benjamini–Hochberg correction at 0.05**. These are ecological, descriptive/exploratory results, **not causal tests**.

## Contents

- `data/processed/` — Eleven RDS objects preserving derived commune-level tables, historic population data, saved networks, and sensitivity outputs. **Not source microdata**.
- `results/` — Final reference tables exported for the manuscript and original session information.
- `figures/` — Figure 1 (`PDF` vector and `PNG`) generated from the archived graphs.
- `scripts/` — Validation, network export, Figure 1 regeneration and a prepublication test **from saved RDS objects**.
- `legacy/` — Six original working scripts preserved for traceability. They are **not** a clean end-to-end executable workflow and may reference local paths or session objects.
- `docs/` — Methodological notes, source inventory under completion, legacy descriptions, and archived original checksums.

## Quick start: reproducibility from archived processed objects

Original recorded environment: **R 4.4.3** with **igraph 2.2.2** (these versions are not yet pinned in a complete lockfile; install `igraph` once with `install.packages("igraph")`). Open this folder as an RStudio project (`chile-territorial-similarity-c34.Rproj`) so the working directory is the repository root, and run:

```r
source("scripts/03_prepublicacion.R")
```

The prepublication script invokes three steps: auditing core archived metrics, exporting edge lists and distance/similarity matrices under `exports/`, and regenerating **Figure 1 v3** in `exports/figures/`, preserving the original published figure under `figures/`. None of the three scripts modifies original `.rds` inputs. The figure-generating code was adapted from the original approved script with relative paths. **Before uploading**, run `source("scripts/03_prepublicacion.R")` in a clean RStudio session and confirm `LISTO_PARA_SUBIR` appears. The revised scripts have not yet been independently executed in R in the package-building environment.

**Scope:** This workflow starts from preserved derived `.rds` objects, not the original institutional source files. See [reproducibility status](docs/REPRODUCIBILIDAD.md). It does not reproduce raw-data extraction or all original permutations yet. Those are tracked as pending work rather than presented as complete.

## Sources, reuse and transparency

The analysis uses official Chilean institutional datasets: cancer registry records, population estimates, environmental regulatory declarations, municipal poverty statistics, healthcare establishment registries and housing/census indicators. The exact URLs, source-file versions, permissions for redistribution, and transformation scripts still need to be cataloged in [source provenance](docs/FUENTES_Y_LICENCIAS.md).

Original individual-level population cancer registry data are **not included**. Public download access does **not** automatically establish unrestricted republication rights or waive privacy safeguards. Only the archived derived/aggregated research outputs have been included in this initial release.

## Methodological notes

- Different cancer registry coverage periods across communes (within 1998–2019) and other covariate reference years; the analysis is multitemporal.
- Regulatory-zone status is **not** a quantitative exposure measurement.
- The age-adjusted rates use an internal pooled standard population; see methodology and original research scripts for details.
- The figure layout is **schematic**, shared between panels for comparison, not a map of Chile.
- The legacy `renv.lock` recorded only `renv`, not all dependencies. It is kept as `docs/renv_lock_original_incompleto.json` **for traceability only**, not as a fully reproducible lockfile.

## Attribution and citation

Research project maintained by [@jjtrivas](https://github.com/jjtrivas). Full author affiliations and a citation file will be added after confirmation with the coauthors. Please cite the specific release/version of this repository when made available; GitHub's changing default branch is not an immutable archive.
