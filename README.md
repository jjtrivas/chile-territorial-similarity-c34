# Computational Integration of Socio-Environmental and Oncological Data Through a Territorial Similarity Network in Chilean Communes

[![R](https://img.shields.io/badge/analysis-R-blue)](https://www.r-project.org/)

**Territorial similarity networks · Chile · Lung cancer (ICD-10 C34)**  
**V Reunión Anual de la Sociedad Chilena de Bioinformática — November 2026**  
**Reproducibility status (17 November 2026):** All three networks recalculated and verified against archived outputs using the final processed matrix; selected calculations independently checked against institutional source files. **Full source-to-matrix reconstruction remains incomplete.**

**Navigation:** [Overview](#overview) · [Network results](#network-specifications-and-results) · [Reproducibility](#reproducibility-and-validation) · [Run the checks](#how-to-run-the-checks) · [Sources and limitations](#data-sources-privacy-and-limitations)

## Overview

This repository accompanies an exploratory ecological analysis of **18 Chilean communes** combining environmental-regulatory, social/housing, demographic, healthcare-infrastructure and lung-cancer indicators. Three network specifications examine how including crude or age-adjusted C34 rates changes the territorial similarity structure relative to a network that excludes C34.

The edges represent **multidimensional similarity** between communes. Matching edge counts across models do not necessarily imply that the models have the same edges. They do **not** represent geographic proximity, cancer transmission, causal associations, or individual cancer risk. The network visualizations use a schematic layout, not a geographical map.

## Network specifications and results

| Network | Specification | Nodes | Edges | Components | Isolates | Louvain modularity (descriptive) |
|---|---|---:|---:|---:|---:|---:|
| **A** | Includes crude C34 rate | 18 | 61 | 4 | 2 | 0.2161308 |
| **B** | Excludes C34 from the similarity calculation | 18 | 61 | 4 | 2 | 0.2840162 |
| **A_ajustada** | Includes age-adjusted C34 rate | 18 | 61 | 4 | 2 | 0.2150323 |

Across the selected commune-specific cancer-registry coverage windows, the archived study dataset contains **1,864 C34 records** and **14,916,395 person-years**. Six C34 correspondence contrasts are reported in [`results/Tabla_correspondencia_C34.csv`](results/Tabla_correspondencia_C34.csv); **none remains significant after Benjamini–Hochberg adjustment at 0.05**. Results should be interpreted descriptively and exploratorily.

### Similarity calculation

The final specifications use standardized Euclidean distances, normalized within each variable block and then averaged with equal block weights. The main network threshold is the **60th percentile of pairwise similarity**. The final processed-matrix reconstruction follows these blocks:

- **Environmental-regulatory:** number of zone declarations and presence of a latent-zone declaration.
- **Social/housing:** multidimensional poverty (2017) and household crowding (2024).
- **Demographic/healthcare:** historical percentage of residents aged 65+, log-transformed public primary-care facilities per 10,000 residents, and public high-complexity hospital indicator.
- **Oncological:** log-transformed crude C34 rate in network A or log-transformed age-adjusted C34 rate in network A_ajustada; this block is absent in B.

For exact calculations and variable names, see [`scripts/06_reconstruir_redes_desde_matriz.R`](scripts/06_reconstruir_redes_desde_matriz.R). The historical age structure used in the final network is **not** the single-year 2020 percentage.

## Reproducibility and validation

The following checks were **successfully executed locally in RStudio** on 17 November 2026. This describes verified procedures, not a claim that the entire project can already be rebuilt from raw official data.

| Check | Verification result | Script / output |
|---|---|---|
| C34 case counts from the original cancer registry | **1,864 records; 18/18 communes matched** | [`scripts/04_verificar_c34_rpc_original.R`](scripts/04_verificar_c34_rpc_original.R); [`results/Tabla_verificacion_C34_RPC_original.csv`](results/Tabla_verificacion_C34_RPC_original.csv) |
| INE population estimates | **14,916,395 person-years; 2,380 commune-year-age records; 18/18 communes matched** | [`scripts/05_ine_y_control_redes_local.R`](scripts/05_ine_y_control_redes_local.R); [`results/Tabla_verificacion_poblacion_INE.csv`](results/Tabla_verificacion_poblacion_INE.csv) |
| Crude and age-adjusted C34 rates | **18/18 communes matched** | [`scripts/05_ine_y_control_redes_local.R`](scripts/05_ine_y_control_redes_local.R); [`results/Tabla_verificacion_tasas_ajustadas_C34.csv`](results/Tabla_verificacion_tasas_ajustadas_C34.csv) |
| Multidimensional poverty, 2017 | **18/18 communes matched** against the original spreadsheet | Local audit; standalone portable script not yet released |
| Crowding, 2024 | **18/18 communes matched** against the census spreadsheet | Local audit; standalone portable script not yet released |
| Primary-care and high-complexity hospital indicators | **18/18 communes matched** against the healthcare establishment dataset | Local audit; standalone portable script not yet released |
| Networks from final processed matrix | **3/3 networks: all 61 edges identical to archived networks**; maximum distance-matrix difference approximately `1.11e-16` | [`scripts/06_reconstruir_redes_desde_matriz.R`](scripts/06_reconstruir_redes_desde_matriz.R); [`results/06_control_redes_desde_matriz.csv`](results/06_control_redes_desde_matriz.csv) |
| Saved-object audit and Figure 1 | Archived network structure, selected tables and original checksums checked; Figure 1 regenerated | [`scripts/03_prepublicacion.R`](scripts/03_prepublicacion.R) |

**Important distinction:** script `06` recomputes the **distance matrices and edges from the saved final analytical matrix**, rather than copying the saved graphs. However, it does **not** recreate the complete analytical matrix from every institutional source file. Source-level reconstruction remains partial, and historical audit scripts have not all been converted into a portable end-to-end pipeline.

The environmental regulation/decree inventory has been documented, but its **full legal-document and spatial-boundary audit remains pending**. The corresponding variables are therefore not described as independently validated against every decree and polygon.

## Repository structure

| Path | Contents |
|---|---|
| [`data/processed/`](data/processed/) | 11 archived RDS objects, including the analytical matrix, age-stratified population references and saved networks; **not individual-level registry files** |
| [`scripts/`](scripts/) | Portable verification, export, figure-generation and network-reconstruction scripts |
| [`results/`](results/) | Main analytical results, session information and the aggregate validation tables |
| [`figures/`](figures/) | Final Figure 1 as PDF and PNG |
| [`docs/`](docs/) | Source/provenance notes, original checksums, methodological notes and remaining reproducibility tasks |
| [`legacy/`](legacy/) | Original working scripts for traceability; not guaranteed to run end-to-end without original local sources or session context |
| `exports/` | **Locally generated** tables and figures produced by running the scripts; not necessary as an additional source-data folder |

## How to run the checks

### 1. Validate archived processed objects and regenerate the figure

Use **R 4.4.3** (the recorded analysis version) and open `chile-territorial-similarity-c34.Rproj` in RStudio so relative paths resolve from the repository root. Install the packages required for the scripts as needed:

```r
install.packages(c("igraph", "haven", "readxl"))
```

Then run:

```r
source("scripts/03_prepublicacion.R")
```

This creates derived outputs under `exports/` and regenerates Figure 1 under `exports/figures/`. The local check completed with `LISTO_PARA_SUBIR`. It does not overwrite the archived RDS inputs.

### 2. Rebuild all three networks from the processed matrix

```r
source("scripts/06_reconstruir_redes_desde_matriz.R")
```

Successful local result: `REDES_RECONSTRUIDAS_DESDE_MATRIZ - 3/3 REDES`. The output comparison CSV is generated under `exports/verification/`.

### 3. Optional: recheck C34, population and rates from local institutional files

**The original C34 registry file and INE workbooks are not distributed in this repository.** Researchers with legitimate access must save their own source copies locally and set the following environment variables to those files or directories; these examples are placeholders, **not** paths to be committed to GitHub:

```r
Sys.setenv(RPC_SAV_PATH = "C:/path/to/private/BBDD_RPC_1998-2019_ID.sav")
source("scripts/04_verificar_c34_rpc_original.R")

Sys.setenv(
  FUENTES_RAIZ = "C:/path/to/INE/DatCom",
  RPC_SAV_PATH = "C:/path/to/private/BBDD_RPC_1998-2019_ID.sav"
)
source("scripts/05_ine_y_control_redes_local.R")
```

The population script expects the relevant `SalComUsuarios-XXTok.xls` workbooks with the original `T5` layout, and the cancer-rate script expects the validated fields in the original RPC extract. These checks compare the reconstructed values with archived reference objects and write **aggregate** verification tables. They are not a public raw-data download pipeline.

## Data sources, privacy and limitations

The analysis draws on Chilean institutional sources for cancer registry records, INE population estimates, environmental zone declarations, municipal poverty measures, healthcare establishments and census/housing variables. Source versions, redistribution permissions and links are being documented in [`docs/FUENTES_Y_LICENCIAS.md`](docs/FUENTES_Y_LICENCIAS.md). Public availability does not necessarily imply unrestricted redistribution rights.

- Commune-specific cancer registry windows differ within **1998–2019**.
- Environmental-regulatory, poverty, housing, demographic and healthcare indicators originate from **different reference years**; they are not necessarily contemporaneous with cancer diagnoses.
- The 2026 healthcare-infrastructure inventory does **not** measure healthcare access at the time of each cancer diagnosis.
- Regulatory-zone designation does **not** quantify individual pollution exposure or prove uniform exposure across an entire commune.
- Age-adjusted C34 rates use an internally pooled reference population; consult the analytical methods before comparing these estimates with externally standardized rates.
- The study is ecological and multitemporal. Results must not be interpreted as individual or causal effects.
- The six cancer correspondence tests are exploratory; their adjusted results are not statistically significant at the stated threshold.
- The original archived `renv.lock` is incomplete and is preserved as [`docs/renv_lock_original_incompleto.json`](docs/renv_lock_original_incompleto.json), **not** presented as a fully pinned dependency lockfile.

**Privacy:** No individual-level cancer registry records or person-level identifiers are supplied here. Do not upload the original `.sav`, `.zip` registry extract, private source directories or identifiers. Aggregate outputs should be reviewed for disclosure risk and redistribution conditions before release.

## Attribution and citation

Maintained by [@jjtrivas](https://github.com/jjtrivas). Full author affiliations and an archival citation will be added after coauthor confirmation. For formal citations, use a fixed repository release/commit rather than the changing default branch whenever possible.
