# DC Historic District Analysis

Code, data, and rendered output for an empirical project examining whether historic districts (HDs) in Washington, DC are associated with neighborhood socioeconomic and demographic change. The work informs a series of [Greater Greater Washington](https://ggwash.org/) pieces.

**Authors:** Pete Rodrigue & Ricardo Sheler (main analysis); Pete Rodrigue (exclusionary-zoning appendix)

**Main report** (rendered from `analysis.Rmd`):
https://pete-rodrigue.github.io/hd_analysis/

**Exclusionary-zoning companion piece** — *"Large-lot zoning is a relic of Jim Crow. It's time to abolish it"* (rendered from `exclusionary zoning analysis.Rmd`):
https://raw.githack.com/pete-rodrigue/hd_analysis/main/exclusionary-zoning-analysis.html

Contact: edwardpierrerodrigue [at] gmail [dot] com.

---

## What's in the analysis

The main report combines descriptive statistics, spatial analysis, and causal-inference methods:

- **Descriptive comparisons** of demographics, income, and rent burden inside vs. outside HDs.
- **Housing, demographic, solar, and energy-efficiency** comparisons between HD and non-HD areas, both at a point in time and over time, going back to 1940.
- **Historical overlay analysis** showing how 1930s FHA redlining grades and 20th-century racially-restrictive covenants spatially relate to present-day zoning.

---

## Reproducing the analysis

1. Clone the repo. Note: the repo is large because DC Open Data and NHGIS files are committed.
2. Open `analysis.Rmd` in RStudio.
3. Knit. The document sources `supporting_functions.R` automatically and writes `analysis.html`.
4. A GitHub Action in `.github/workflows/` renames `analysis.html` to `index.html` on push, so GitHub Pages serves the latest version at the URL above.

To knit the exclusionary-zoning piece instead, open `exclusionary zoning analysis.Rmd` and knit; it writes `exclusionary-zoning-analysis.html`.

**Environment:** the main analysis was run on R 4.5.1 (Windows). Core packages (with the versions used): `sf 1.0-21`, `dplyr 1.1.4`, `ggplot2 3.5.2`, `leaflet 2.2.2`, `plotly 4.11.0`, `did 2.1.2`, `fixest 0.12.1`, `HonestDiD 0.2.6`, `here 1.0.2`. A full `sessionInfo()` capture lives at the top of `analysis.Rmd`.

**Data you'll need to download separately** (gitignored, too large to commit):
- `building_permits/` — DC Open Data building-permit CSVs. Only `Building_Permits_in_2024.csv` and `solar_all_years_geocoded.csv` are actually read by `analysis.Rmd`; the other year-by-year files were used to build the geocoded solar file.
- `dc_residential_data/` — only `DC_Properties.csv` is read by `analysis.Rmd`.

Download both from https://opendata.dc.gov/ and place them at the paths above.

---

## Top-level scripts

| File | Purpose |
|---|---|
| `analysis.Rmd` | Main Rmarkdown document. Produces the full report. Heavily commented — see inline comments for methodology notes. |
| `exclusionary zoning analysis.Rmd` | Companion document focused on exclusionary-zoning evidence. Produces `exclusionary-zoning-analysis.html` and (via commented-out `saveWidget` calls) the two files in `stand_alone_maps/`. |
| `supporting_functions.R` | Helper functions sourced by both Rmd files: data loading, crosswalk construction, block-matching, DiD model wrappers, and a method implementation of Rambachan–Roth honest-DiD for `AGGTEobj` results. |
| `map_pct_white_by_tract_1940.R` | Standalone script. Pulls 1940 tract-level demographics live from the DC GIS REST API (no local data required) and builds a Leaflet map of percent-white by tract. |
| `scrape_urban_turf.py` | Selenium scraper (headless Chrome) that pulls the DC apartment/condo development pipeline from [dc.urbanturf.com](https://dc.urbanturf.com/pipeline) and writes `urbanturf_projects.csv`. |

---

## Rendered output

| File | What it is |
|---|---|
| `index.html` | The main report, served by GitHub Pages. Renamed from `analysis.html` by the workflow. |
| `exclusionary-zoning-analysis.html` | The rendered exclusionary-zoning companion piece. Viewable via raw.githack. |
| `stand_alone_maps/standalone_appendix_map.html` | Self-contained Leaflet map (FHA grades, discriminatory covenants, 1940 demographics, modern R-zones, home prices). Designed to be iframe-embedded elsewhere. |
| `stand_alone_maps/standalone_appendix_sankey.html` | Self-contained Plotly Sankey diagram showing flow of acres across FHA grades and modern zoning categories. |
| `stand_alone_maps/standalone_appendix_sankey_files/` | JS/CSS dependencies bundled with the Sankey file (htmlwidgets artifacts from `saveWidget`). |
| `exclusionary_zoning_plots/` | PNG exports (`v1.png`, `v2.png`) of a key figure from the exclusionary-zoning piece. |

---

## Data folders

### Census and ACS data (IPUMS NHGIS)

Most demographic data comes from [IPUMS NHGIS](https://www.nhgis.org/):

> Jonathan Schroeder, David Van Riper, Steven Manson, Katherine Knowles, Tracy Kugler, Finn Roberts, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 20.0. Minneapolis, MN: IPUMS. 2025. http://doi.org/10.18128/D050.V20.0.

| Folder | Contents |
|---|---|
| `block_data/` | 1970–2020 block-level CSVs (race, total population). Contains a `1990_2020_housing_unit_data/` subfolder (housing-unit counts) and a `block_xwalks_1990_to_2020/` subfolder with NHGIS-provided block crosswalks (retained for forkers; the main analysis builds its own crosswalks). |
| `block_group_data/` | 2006–2010 and 2019–2023 5-year ACS block-group tables, plus a `2010_2020_xwalk/` subfolder. Used in descriptive statistics. |
| `block_group_shapes/` | 2010 and 2023 block-group shapefiles. |
| `block_shapes/` | Block shapefiles for each decennial 1940–2020. Boundaries shift across years, which is what motivates the custom crosswalk work in `xwalks/`. |
| `tract_data/` | Tract-level shapefiles 1940–2020. Not used by `analysis.Rmd`; retained for `map_pct_white_by_tract_1940.R` and anyone forking. |
| `vacancy_data/` | 2019 5-year ACS block-group shapefile and CSV with vacancy variables. |

### 1940 and 1950 block data (GWU / Prologue DC)

The 1940 and 1950 block shapefiles (`block_shapes/1940_block_shapefiles/` and `block_shapes/1950_block_shapefiles/`) were digitized and geocoded by Leah Brooks and colleagues at the George Washington Center for Washington Area Studies, in collaboration with Prologue DC. Source: https://blogs.gwu.edu/centerforwashingtonareastudies/resources/

### DC Open Data

| Folder / file | Contents | Source |
|---|---|---|
| `Affordable_Housing/` | Locations of affordable-housing projects. | https://opendata.dc.gov/datasets/DCGIS::affordable-housing/about |
| `Historic_Districts/` | DC historic-district boundaries. | https://opendata.dc.gov/datasets/DCGIS::historic-districts/about |
| `Wards_from_2022/` | Ward boundaries (2022). | https://opendata.dc.gov/datasets/DCGIS::wards-from-2022/about |
| `zoning/` | DC's 2016 zoning-regulation boundaries. | https://opendata.dc.gov/datasets/DCGIS::zoning-boundaries-zoning-regulations-of-2016/about |
| `Waterbodies_2021/` | Waterbody shapefile, used for map basemaps. | DC Open Data |
| `Building_Footprints/` | Building-footprint shapefile for all DC structures. | DC Open Data |
| `building_energy_data/` | `Building_Energy_Benchmarking.csv` (used in the main report's energy-efficiency section). Also contains a `Building_Energy_Performance/` shapefile and `energy_star_buildings_geocoded.csv` — these two are not currently referenced by either Rmd. | DC Open Data |
| `building_heights/dc_buildings.csv` | Per-building height information. Not currently referenced by either Rmd; retained in case future revisions use it. | DC Open Data |
| `building_permits/` | *(gitignored)* 2009–2024 permit CSVs plus solar extracts. Only `Building_Permits_in_2024.csv` and `solar_all_years_geocoded.csv` are read. | DC Open Data |
| `dc_residential_data/` | *(gitignored)* Property and address-point data. Only `DC_Properties.csv` is read by the main analysis. | DC Open Data |
| `Record_Lots/` | Record-lot (tax-lot) shapefile. Used by the exclusionary-zoning piece. | DC Open Data |
| `Computer_Assisted_Mass_Appraisal_-_Residential.csv` | CAMA residential-appraisal file (property characteristics, assessed values). Used by the exclusionary-zoning piece. | DC Open Data |

### Historic / discriminatory-policy data

| Folder | Contents |
|---|---|
| `discriminatory_policies/fha_map/` | 1930s Federal Housing Administration redlining map (georeferenced TIFF plus derived shapefile). Source: [DC Policy Center](https://www.dcpolicycenter.org/publications/mapping-segregation-fha/), originally National Archives. |
| `discriminatory_policies/dc_discriminatory_covenants/` | Shapefile of racially restrictive covenants, derived from an image provided by [Mapping Segregation DC](https://mappingsegregationdc.org/). The Mapping Segregation team generously provided feedback and a random sample of properties. |

### Historic-district ancillary data

| Folder / file | Contents |
|---|---|
| `hd_data/data.csv` | Hand-assembled historic-district designation dates, compiled from the DC Historic Preservation Office website and Wikipedia. A local copy of the [Google Sheet](https://docs.google.com/spreadsheets/d/1Ajl1iAS0NRB7vk_UFDveeWzGkwf3tuiDo-zV9_wtzRM/) that the Rmd actually reads at run-time. |
| `potential_new_hds/` | KML files listing potential future historic districts: 2020 DC Comp Plan / Ward Heritage Guide mentions, past community-considered designations, current conversations, pending applications, and the DC Preservation League's Endangered Places list. |
| `Literature/` | PDFs of key papers cited in the report's literature-review section. |

### Derived / intermediate data

| Folder | Contents |
|---|---|
| `timeseries/` | Saved RData files (`ts_pop_density.RData`, `ts_pop_density_no2020_screen.RData`) caching pre-built block-level time series. Lets you re-knit quickly without rebuilding the time series from scratch. |
| `xwalks/` | Saved RData crosswalks (`inhd_xwalk_list.RData`, `nohd_xwalk_list.RData`) — pre-computed block-to-block crosswalks used in the main analysis. Regenerated if you set `RUN_EVERYTHING_FROM_SCRATCH = TRUE` at the top of `analysis.Rmd`. |

### Google Sheets (live data, read at knit time)

Several datasets are pulled live at knit time rather than from local files:

- [Historic district designation dates](https://docs.google.com/spreadsheets/d/1Ajl1iAS0NRB7vk_UFDveeWzGkwf3tuiDo-zV9_wtzRM/)
- [Affordable housing projects](https://docs.google.com/spreadsheets/d/1vVJ_AshZVdxJpXrbSutCfHxK3tfgoi1Q-fIus-hm7RA/) — downloaded from Open Data DC and hand-labeled as "creating new affordable units" vs. "preserving existing units."
- [Urban Turf apartment-pipeline scrape](https://docs.google.com/spreadsheets/d/1Y6aoKym6u7dMvHhZEkFeRAyowqTzstwRuAKzk2XLg9A/) — produced by `scrape_urban_turf.py` and geocoded.

Local CSV fallbacks are commented out in the Rmd.

### Other external data

The "comparable cities" statistics in the report summary come from a [GGWash post](https://ggwash.org/view/78627/dc-has-more-historic-buildings-than-boston-chicago-and-philadelphia-combined-why-2) citing the National Trust for Historic Preservation.

---

## Supporting files and config

| File / folder | Purpose |
|---|---|
| `.github/workflows/` | GitHub Action that renames `analysis.html` → `index.html` on each push so GitHub Pages picks up the latest version automatically. |
| `images/` | Static images referenced by the Rmarkdown documents. |
| `.gitignore` | Files excluded from version control — notably the full-US 1970 block CSV, the `building_permits/` folder, and the `dc_residential_data/` folder. |
| `.gitattributes` | Git attribute config. |
| `LICENSE` | MIT license. |
| `README.md` | This file. |

---

## Notes for forkers

- The repo is large because most raw DC Open Data and NHGIS exports are committed directly. If you fork, consider replacing the committed raw data with a download script.
- Block boundaries shift across decennial years. The `xwalks/` folder has pre-computed block-to-block crosswalks that the main analysis uses; see the relevant chunks in `analysis.Rmd` for how they're built. Setting `RUN_EVERYTHING_FROM_SCRATCH = TRUE` at the top of the Rmd forces them to be regenerated.
- The block-matching code pairs each HD block with the `n` nearest non-HD blocks based on a configurable set of matching variables (race composition, density, etc.). See `create_matches()` in `supporting_functions.R`.
- Questions, corrections, or fork ideas: edwardpierrerodrigue [at] gmail [dot] com.
