# DSR Calculator

`episerver_dsr()` opens a Shiny app that calculates crude rates and directly age-standardised rates (DSRs) from EpiServer's ED and APC data. It gives confidence intervals, maps, funnel plots and R code that reproduces the tables. `dsr_calculate()` and `dsr_age_specific()` do the arithmetic and are exported for scripts; the app's Copy Code calls them. Analysts open the app from Addins > Open Menu, or by calling `episerver_dsr()`.

## Status

| Pull request | Content |
|---|---|
| #3 | First version: datasets, years and pooling, populations, levels, standards, suppression, diagnosis codes, the Tables and Map tabs, and Copy Code |
| #4 | Age ranges (truncated DSRs), an age-specific map, the three-year default and ASGS 2021 labels |
| #5 | Explore tab (ggiraph map and funnel plot, linked), funnel methods and the Help tab |
| #6 | Sidebar layout in the browser, keyboard-only highlights, the AIHW minimums (n.p.), Counts by age, validation against PHEindicatormethods and epitools, and these notes |

The pull request descriptions give the detail of each change and what to check against EpiServer.

## Where the code is

| File | Holds |
|---|---|
| `R/dsr_calculate.R` | `dsr_calculate()` and `dsr_age_specific()`; strata (`.dsr_strata()`), exact Poisson limits (`.pois_lower()`, `.pois_upper()`), suppression and argument checks |
| `R/episerver_dsr.R` | `episerver_dsr()` and `episerver_dsr_stop()`; the definitions (`.dsr_asgs`, `.dsr_datasets`, `.dsr_scopes`, `.dsr_surrounds`, `.dsr_levels`, `.dsr_age_labels`); validated settings (`.dsr_spec()`); SQL builders (`.dsr_sql_*()`); the code generator (`.dsr_code()`); tables (`.dsr_datatable()`, `.dsr_counts*()`, `.dsr_tsv()`); map controls; the app factory `episerver_dsr_app()` |
| `R/dsr_map.R` | ABS boundaries (`.dsr_boundaries()`, `.dsr_fetch_json()`, the cache) and the static map (`.dsr_map_plot()`) |
| `R/dsr_funnel.R` | Funnel statistics (`.dsr_funnel()`, limits, overdispersion) and the funnel plot |
| `R/dsr_help.R` | The Help tab, built from the definitions so that it follows the code |
| `R/epi_app.R` | Infrastructure shared with the other apps (see `shared-apps.md`) |

| Test file | Covers |
|---|---|
| `test-dsr_calculate.R` | The arithmetic and the argument checks |
| `test-dsr_validation.R` | Every rate against PHEindicatormethods, epitools and `stats::poisson.test()`; skips without the two packages |
| `test-dsr_funnel.R`, `test-dsr_map.R`, `test-dsr_help.R` | The funnel statistics, the boundaries and map, and the Help tab |
| `test-episerver_dsr.R` | The app through `shiny::testServer()` with a fake EpiServer (`fake_episerver()` in the file), the SQL builders, and the code generator, whose output is run and compared with the app's tables |
| `test-episerver_dsr_live.R` | Every query against EpiServer; skipped offline |

## Decisions

### Arithmetic

- **Intervals.**
  - Crude and age-specific rates have exact Poisson (Garwood) intervals.
  - DSRs have Dobson et al. (1991) intervals, with exact limits for the total count.
  - OHID's PHEindicatormethods takes the count's limits from Byar's approximation instead, so the validation tests swap the limits before comparing.
- **Weights** are the standard population's shares over the age groups in the rates, so the weights of a truncated DSR (an age range) sum to 1 within the range.
- **Persons** include sex not stated and other sex; males and females do not.
- **Undefined DSRs.** A DSR is undefined when an age group has events but no population, and `DSRNote` says so.
- **Minimums.** The AIHW (2011) principles, which the ABS applies in *Deaths, Australia, 2010*, Appendix 1, give two:
  - Principle 3: no DSR from fewer than 20 events.
  - Principle 2: no DSR when the population is under 30 in any age group.
  - `dsr_calculate()` defaults both to 0, so existing scripts give the same results. The app defaults to 20 and 30 and passes them in Copy Code.
  - Exactly 20 events and exactly 30 people pass. Principle 3 says "less than 20" but later "greater than 20"; the first reading is used.
  - Pooled populations are summed over the years.
  - The principles set no rule for age groups with no events.

### Data

- **Datasets.** The fields, the principal-diagnosis rule and the ICD-10 conditions of each dataset are in `.dsr_datasets`, with the reasons in its comment.
  - ED always searches `EDShortListCode` and `Diagnosis1` to `Diagnosis3`, because no field holds the principal diagnosis in every year.
- **Areas.** The ERP5 population tables hold ASGS 2021 areas, so events use `geo_SA3_2021` and `geo_SA2_2021`. The edition is set once, in `.dsr_asgs`.
- **ACT and surrounds** are the ACT plus the four NSW SA3s that border it (`.dsr_surrounds`), with their populations from the `_AUS` tables.
- **Excluded events.** Events with no age, no residence, or an area missing from the population table are left out of the rates and reported in the info bar.
- **Years.** The range offered is the years covered by both the events and the populations. The default is the latest complete year and the two before it.
- **Connections.** Every query goes through `.epi_connection()`, so through `episerver_connect()`. Every value put into SQL passes through `.dsr_spec()`.

### Interface

- **Layout.** bslib, not shinydashboard (see `shared-apps.md`).
  - The sidebar has the sections Events, Population, Time periods and Rate details, with Calculate and Copy Code held at its foot.
  - The sidebar cannot be resized, as the resize handle covered its scrollbar.
- **Opening.** The app opens in the web browser by default, for space.
- **Tables.** The Tables tab switches between Rates, Age-specific rates and Counts by age, with one Copy table button for the table shown.
  - Copied tables use en dashes in age groups and periods, so Excel keeps them as text.
- **Ages on maps.** Maps and the funnel plot state the ages of the value shown in the title, axis labels, tooltips, click text and file names. Subtitles never state ages, so a map cannot give two age ranges.
- **Not presented.** A DSR below the minimums reads n.p., and pointing at it gives the reason. The info bar counts n.p. rows by reason and names the remedies (pool years, choose a larger area).

### Funnel plots

- **DSRs** follow OHID's Fingertips method. Each DSR is plotted against the population derived as events / DSR, and the average is total events over total derived population. Areas whose DSR is not presented are left out, replacing OHID's rule of fewer than 10 events.
- **Crude and age-specific rates** are plotted against population.
- **Events.** Observed / expected events are plotted against expected events, with the expected counts from the age-specific rates of the areas plotted (indirect standardisation).
- **Limits and overdispersion.**
  - The limits are exact Poisson limits at 95% and 99.8%.
  - Spiegelhalter's (2005) allowance for overdispersion is on by default.
  - The method was checked against PHEindicatormethods and FunnelPlotR in pull request #5.
- **Suppressed and n.p. areas** are left out of the average, the overdispersion estimate and the limits. The average is printed, so including a suppressed area would let its events be recovered by subtraction.
- **Colours,** in order: coral `#F15F50` (above the 99.8% limit), yellow `#C6B30B` (above the 95% limit), grey `#CBCBC6` (within the limits), green `#90A630` (below the 95% limit) and blue `#008FC5` (below the 99.8% limit). The maintainer chose green and blue in pull request #6; see Open items.

### Suppression

- **Default.** The app suppresses rows with fewer than 5 events. Each row is judged on its own count, and populations are never suppressed.
- **What it blanks.** In the tables and maps, a suppressed row is calculated and then blanked, so suppression changes no other row.

### Maps

- **Boundaries** come from the ABS ASGS 2021 ArcGIS services (`geo.abs.gov.au`), simplified on the server (`.dsr_boundary_offset()`). They are cached as RDS files in `tools::R_user_dir("actepir", "cache")`.
- **Libraries.** The Map tab draws a static ggplot2 map that can be copied or saved as PNG or PDF. The Explore tab uses ggiraph: SVG with tooltips and selection, with no map tiles. Leaflet was rejected: it adds about 19 packages, including sf, terra and raster, and its tiles would go through the proxy.

## Open items

- **Funnel colours and colour vision (waiting on the maintainer).** On the map, colour is the only cue for an area's position.
  - In normal vision, yellow (above 95%) and green (below 95%) differ by OKLab ΔE 8.8; pairs below about 10 are hard to tell apart.
  - With deuteranopia, coral and green differ by 1.9.
  - Proposed alternative, from the brand's district colours: dark red `#93171B`, coral `#F15F50`, grey `#CBCBC6`, cyan `#0AB4BA`, dark blue `#004868`. Every pair differs by 10 or more under the four vision types tested.
- **Principle 4: age groups.**
  - The principles use five-year groups from 0-4 to 75+, and allow ten-year groups to overcome small numbers. The calculator uses 18 groups to 85+.
  - A 75+ top group would raise the oldest populations and avoid many Principle 2 failures. It changes the DSRs, so it should be a visible option, and the standard population would be collapsed the same way.
- **Complementary suppression (waiting on the maintainer for a rule).**
  - A single suppressed age group can be worked out as the row total minus the other age groups. The Rates and Age-specific rates tables together allow this, and Counts by age shows the figures side by side.
  - Males can be estimated as persons minus females, out by the events with sex not stated.
  - The ACT Health release rules should set the method.
- **Consistent time series.** The minimums apply row by row, so a series can have n.p. years. The principles ask for one method across a series. A warning when a series has gaps would help; pooling stays the analyst's choice.
- **Counts by age width.** The table fits a 1920-pixel window, but at 1440 pixels it scrolls sideways.
- **R CMD check notes,** also on `main`:
  - "Namespace in Imports field not imported from: 'labelled'";
  - the examples write two files to the sass cache.

## How to check a change

**Offline:**
- Run the three commands in CLAUDE.md. With no EpiServer, 19 tests skip: 17 that need EpiServer, 1 that needs an ODBC driver and 1 that needs the ABS service.
- `test-dsr_validation.R` needs `PHEindicatormethods` and `epitools`, and skips without them.
- For the app in a browser, run `Rscript dev/harness/run_app.R dev/harness/dsr_calculator.js <output dir>`. Compare the output with the results recorded in `dev/harness/README.md`, and look at the screenshots.
- For the live tests, run `Rscript dev/harness/dryrun_live.R`.
- When a SQL builder changes, update `dev/harness/fake_episerver.R`, and `dev/harness/fake_live.R` if the live tests send the query. Add or update a live test too (CLAUDE.md).

**Against EpiServer:** the maintainer runs `testthat::test_local()`, where `test-episerver_dsr_live.R` must run rather than skip. They then use the app as described in section 3 of the pull request.
