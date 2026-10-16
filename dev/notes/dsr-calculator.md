# DSR Calculator

`episerver_dsr()` opens a Shiny app that calculates crude rates and directly age-standardised rates (DSRs) from EpiServer's ED and APC data. It gives confidence intervals, an interactive map and funnel plot, and R code that reproduces the tables. `dsr_calculate()` and `dsr_age_specific()` do the arithmetic and are exported for scripts; the app's Copy Code calls them. Analysts open the app from Addins > Open DSR Calculator, or by calling `episerver_dsr()`.

## Status

| Pull request | Content |
|---|---|
| #3 | First version: datasets, years and pooling, populations, levels, standards, suppression, diagnosis codes, the Tables and Map tabs, and Copy Code |
| #4 | Age ranges (truncated DSRs), an age-specific map, the three-year default and ASGS 2021 labels |
| #5 | Explore tab (ggiraph map and funnel plot, linked), funnel methods and the Help tab |
| #6 | Sidebar layout in the browser, keyboard-only highlights, the AIHW minimums (n.p.), Counts by age, validation against PHEindicatormethods and epitools, and these notes |
| #7 | Searching diagnosis codes by condition, with ICD-10-AM titles from `Analysis.dbo.ICD10AM`, and each code entered spelt out below the box; APC procedure codes and ACHI blocks, searched the same way, combined with diagnoses by and or or |
| #8 | Cleanup: no map or funnel plot at state level, the static Map tab and Australian residents removed, a Download menu (map or funnel plot, PDF or PNG) and brand-coloured toolbars on the Explore tab, a second highlight colour in Counts by age, and an add-in for each app in place of the menu (browser only) |
| Pending | Sidebar: tinted section headings, ages and male and female under Population, Geographic scope, tighter spacing, notes behind information buttons, a resizable sidebar with the handle off the scrollbar, and a problem report; notifications in the brand colours |

The pull request descriptions give the detail of each change and what to check against EpiServer.

## Where the code is

| File | Holds |
|---|---|
| `R/dsr_calculate.R` | `dsr_calculate()` and `dsr_age_specific()`; strata (`.dsr_strata()`), exact Poisson limits (`.pois_lower()`, `.pois_upper()`), suppression and argument checks |
| `R/episerver_dsr.R` | `episerver_dsr()` and `episerver_dsr_stop()`; the definitions (`.dsr_asgs`, `.dsr_datasets`, `.dsr_scopes`, `.dsr_surrounds`, `.dsr_levels`, `.dsr_age_labels`); validated settings (`.dsr_spec()`, `.dsr_parse_codes()`, `.dsr_parse_procedures()`); diagnosis and procedure titles and their search (`.dsr_icd_*()`); SQL builders (`.dsr_sql_*()`); the code generator (`.dsr_code()`); tables (`.dsr_datatable()`, `.dsr_counts*()`, `.dsr_tsv()`); the codes boxes (`.dsr_codes_box()`), settings with notes (`.dsr_with_help()`) and map controls; the problem report (`.dsr_problem_report()`); the app factory `episerver_dsr_app()` |
| `R/dsr_map.R` | ABS boundaries of SA3s and SA2s (`.dsr_boundaries()`, `.dsr_fetch_json()`, the cache) and the Explore tab's map (`.dsr_map_plot()`, with `.dsr_map_size()` for the size of a saved map) |
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
- **Populations** (`.dsr_scopes`) are ACT residents, and the ACT and surrounds, whose residents mostly use ACT hospitals.
  - The ACT and surrounds are the ACT plus the four NSW SA3s that border it (`.dsr_surrounds`), with their populations from the `_AUS` tables.
  - Australian residents are not offered. The maintainer removed them: a rate, and above all a DSR, over areas far beyond the ACT's hospitals means little.
- **Excluded events.** Events with no age, no residence, or an area missing from the population table are left out of the rates and reported in the info bar.
- **Years.** The range offered is the years covered by both the events and the populations. The default is the latest complete year and the two before it.
- **Connections.** Every query goes through `.epi_connection()`, so through `episerver_connect()`. Every value put into SQL passes through `.dsr_spec()`.

### Interface

- **Layout.** bslib, not shinydashboard (see `shared-apps.md`).
  - The sidebar has the sections Events, Population (Geographic scope, Geographic level, Ages, Male and female), Time periods and Rate details (standard, rate per, suppression, minimums), with Calculate, Copy Code and the bug button held at its foot.
  - The population select is labelled Geographic scope, so that it does not repeat its section's name. Its input ID is still `scope`.
  - Section headings are in the info bar's tint (`#eae6f1`) in bold Nightshade, so that open sections can be told from their settings. The chevrons are Nightshade too (`--bs-accordion-btn-icon`).
- **Spacing (waiting on the maintainer).** Six CSS variables on `aside#settings` set the sidebar's spacing: `--dsr-font`, `--dsr-gap`, `--dsr-head-pad`, `--dsr-body-pad`, `--dsr-box-pad` and `--dsr-box-font`. Three sets were compared by the height of the sidebar with every section open, against 2,105 pixels before:

  | Option | Text, boxes | Gap | Headings, sections | Height |
  |---|---|---|---|---|
  | A Light | 16px, 15px | 12px | 12px 20px; 10px 20px 14px | 1,664 |
  | B Compact (current) | 15px, 14px | 10px | 10px 16px; 8px 16px 12px | 1,520 |
  | C Dense | 14px, 13px | 7px | 7px 14px; 6px 14px 10px | 1,336 |

  Box padding is 6px 12px, 4px 10px and 2px 8px. All three include moving the notes behind information buttons and removing the empty band above the foot. The maintainer chooses; changing option is changing the six values.
- **Resizing.** The sidebar can be resized by dragging its right edge (`resizable = TRUE`).
  - Pull request #6 turned resizing off because the handle covered the scrollbar. Tested in headless Chromium with a 15px scrollbar:
    - bslib 0.10.0: the handle covers the scrollbar's right 13px; the wheel and the thumb do nothing there.
    - bslib 0.11.0 and 0.12.0: the handle activates when the pointer crosses the sidebar's edge, and its active strip then covers the scrollbar's right 12px. Reaching the scrollbar from the results, the usual way, crosses the edge, so the wheel and the thumb fail. From inside the sidebar they work.
    - On a device with a touch screen (`any-pointer: coarse`), bslib's CSS makes the handle 26px wide and clickable over the scrollbar. This was read from the CSS, not tested.
  - CSS in `episerver_dsr_app()` moves the handle, its strip and its grip just outside the sidebar's edge (`aside#settings ~ .bslib-sidebar-resize-handle`). With it, the wheel and the thumb work across the scrollbar from either side, dragging the edge still resizes, and the sidebar's collapse arrow is not covered, in all three versions.
  - The rules depend on bslib's class names (`.bslib-sidebar-resize-handle`, its `::before` and `.resize-indicator`). A bslib release that renames them would put the handle back over the scrollbar; the harness check "SIDEBAR scrollbar" shows it.
- **Notes behind information buttons.** Guidance that does not change with the settings is hidden under an information button at the right of the setting's label (`.dsr_with_help()`): Diagnosis codes, Diagnosis search (which fields each dataset searches), Procedure codes, and Minimum events for a DSR (the AIHW minimums).
  - A click shows the note below the label and a second click hides it, so a note can stay open while codes are typed. A tooltip was rejected because it closes when the pointer moves.
  - The notes are a Bootstrap collapse written into the page when the app starts, so they need no server round trip. Their text depends only on whether the titles could be read, which is known then.
  - What answers the settings stays in view: errors, the codes entered with their titles, that ED has no procedures (which explains the disabled box), and the periods of pooled years.
  - The ED diagnosis note moved from Diagnosis codes to Diagnosis search, the setting it explains, and now describes both datasets.
- **Problem report.** The bug button at the right of the foot opens a dialog in which the analyst says what happened. Copy report copies that and the details shown below it through `.epi_copy_text()`, and the dialog names the maintainer (from DESCRIPTION, `.epi_maintainer()`) with an email link.
  - The details: the calculator's state (calculated or not, the last EpiServer and settings errors, how many titles and standards were read, the years offered, the code button), every setting by input ID as R arguments for `session$setInputs()`, the Copy Code code for valid settings, and the session (`.epi_session_report()` in `R/epi_app.R`): actepir's version and the GitHub ref and commit it was installed from, R, the operating system, locale and time zone, library paths, which proxy variables are set, the ODBC drivers, the browser and window, errors shown in outputs and script errors, and every package loaded with its version.
  - No counts, rates or other data are included, and proxy variables are named but not given, as they can hold a password. A test checks both.
  - The report is copied rather than put in the email link: `mailto:` links are commonly limited to about 2,000 characters on Windows, and a report runs to several thousand. Opening a GitHub issue was rejected: the repository is public and analysts may not have accounts.
  - The browser's part (`bug_client`) is sent by the button's click; the dialog opens when the server receives it.
- **Opening.** The app opens in the web browser only, from its add-in or `episerver_dsr()` (see `shared-apps.md`).
- **Tables.** The Tables tab switches between Rates, Age-specific rates and Counts by age, with one Copy table button for the table shown.
  - Copied tables use en dashes in age groups and periods, so Excel keeps them as text.
  - Counts by age highlights populations under the minimum in Tuggeranong yellow and event totals under the minimum in Inner North blue, each mixed 75% with white (`#F1ECC2`, `#BFE3F1`; classes `dsr-short-pop` and `dsr-short-events`). The note above the table shows each in its colour. The blue was chosen from the regional and district colours, mixed the same way: it differs from the yellow by OKLab ΔE 8.3 or more in normal vision and simulated deuteranopia, protanopia and tritanopia, stands out from the white and striped rows more than the yellow does, and dark text on it has 9.3:1 contrast. Coral and pink tints came within 4.3 to 5.3 of the yellow under colour-vision deficiency, and Bluebell and Orchid tints within 3.5.
- **Ages on maps.** The map and the funnel plot state the ages of the value shown in the title, axis labels, tooltips and file names. Subtitles never state ages, so a map cannot give two age ranges.
- **Not presented.** A DSR below the minimums reads n.p., and pointing at it gives the reason. The info bar counts n.p. rows by reason and names the remedies (pool years, choose a larger area).

### Diagnosis and procedure search

- **Titles.** They come from `Analysis.dbo.ICD10AM`, which the maintainer moved there from a reference database that analysts cannot reach.
  - The table stacks every edition it holds (3 to 11), with diagnoses and procedures told apart by `VarType` (`'Diagnosis'`, `'Procedure'`).
  - `.dsr_sql_icd()` reads both once, when the app starts, and `.dsr_icd_titles()` splits them by type. Each code takes its title from the latest edition that holds it, and a code that some editions lack is labelled with the editions that hold it, for example "(editions 3 to 9)".
  - Editions come from `ICD10AMVersion`. The maintainer advised that `StartDate` and `EndDate` are not reliable.
  - `ProcedureBlock` and `BlockDescription` are empty for diagnoses, so no diagnosis blocks (such as I20-I25) are offered.
- **Not in the package.** ICD-10-AM is licensed by IHACPA and the repository is public, so the titles stay on EpiServer. Tests and fakes use invented titles only.
- **Searching.**
  - The browser holds only the suggestions for what has been typed, at most 100, fetched from a URL registered with `session$registerDataObj()`. The full list stays in R, so its size is not limited by the browser.
  - Each word typed must match the start of a word in the code or title (`.dsr_icd_words()`). Codes match with or without the dot, and suggestions are in code order, each code before the codes under it.
  - Accents are dropped from both sides, by code point, so the session's locale does not matter. Titles the driver returns in Windows-1252 are converted to UTF-8.
- **The box's value is unchanged.** It is still the code text, so `.dsr_parse_codes()`, the SQL and Copy Code are untouched.
  - A code or range is added as typed, by Enter, Tab or a comma, because the add option takes precedence for anything shaped like a code (`createFilter`).
  - Other text is never made into a tag. Enter or Tab adds the suggestion highlighted.
- **Ranges** are typed, as before. The note under the box spells out each entry: a code's title, the first and last codes under a partial code, or the codes at both ends of a range, and any code that is not in the table.
- **Without the table,** the note says the search is unavailable and the box takes typed codes as before.
- **Procedures** have a box of their own, with *Procedure search* (principal or any) and the and/or choice below it.
  - Only APC holds procedures: `Procedure1` to `Procedure50`, with each one's ACHI block in `BlockProcedure1` to `BlockProcedure50` (`.dsr_datasets`). `Procedure1` is the principal procedure.
  - For ED the box and both choices are disabled, and `.dsr_spec()` drops any procedure codes, so ED rates ignore them. The entries stay in the box for when APC is chosen again.
  - Codes are ACHI codes (`11012-00`), compared without hyphens or dots, so the SQL works whether the data hold them with or without the hyphen. Partial codes need the five-digit stem (`11012`).
  - **Blocks** are searched in the `BlockProcedure` fields, not expanded into codes, so a block keeps the codes the coder's edition put in it. A number of up to four digits is a block (`1826`, shown as `[1826]`) and five or more a code, so the two cannot be confused.
  - **Ranges** of codes take "to" (`11000-00 to 11027-00`), as codes contain a hyphen; a hyphen between two stems (`11000-11027`) or two blocks (`1820-1830`) also makes a range.
  - The search lists blocks and codes in block order, each block before its codes, and a block takes its description from the latest edition that holds it. The descriptions in the table are abbreviated to about 40 characters.
- **And or or.** With both diagnoses and procedures, the events SQL has one `EXISTS` for each, joined by `AND`, or by `OR` inside brackets. With only one of them the condition is as before, so the diagnosis-only SQL did not change.

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
- **SA3 and SA2 only.** The Explore tab compares areas at SA3 and SA2 (`comparable()` in the server). At state level ACT residents are one area, and the ACT and surrounds two blocks of SA3s, which neither a map nor a funnel plot helps compare, and a two-block map misleads. The tab then shows only the note "Choose SA3 or SA2 to compare areas within ...". The tables still give state rows. No state boundaries are downloaded, so `.dsr_boundary_layers` has no state layer.
- **One map.** The Explore tab is the only map. A static Map tab, with Copy map, PNG and PDF, was removed as the Explore tab does what it did and more.
- **Libraries.** The Explore tab uses ggiraph: SVG with tooltips and selection, with no map tiles. Leaflet was rejected: it adds about 19 packages, including sf, terra and raster, and its tiles would go through the proxy.
- **Exports.**
  - One Download menu saves the map or the funnel plot as PDF or PNG. It is a Bootstrap dropdown written in HTML (bslib already loads Bootstrap's JavaScript), so it adds no package. Its four links are `downloadLink()`s, with `suspendWhenHidden = FALSE` because they sit in a closed menu, and Shiny leaves a download link disabled until it has its address.
  - Each file is `ggsave()` of the same ggplot as on the page, with the heading over the plots as title and subtitle; PNGs at 200 dpi. ggiraph's interactive layers draw as plain layers outside the browser. File names come from `ex_file()`.
  - The map follows Zoom to selected (server side), not the toolbar's pan and zoom, which the server does not see.
  - The map takes the shape of the area in view, but is at least 8 inches wide, as the funnel plot is. Narrower, the subtitle (wrapped at 70 characters) was cut off beside the ACT, which is taller than it is wide.
- **Toolbars.** ggiraph's toolbars are set in `ex_girafe()`, whose `hidden` argument names the buttons left out.
  - The icons are Iris, Nightshade on hover, by CSS on `.ggiraph-toolbar-icon.neutral`; ggiraph's default is blue `#069`. Lasso deselection stays red.
  - Neither toolbar has ggiraph's PNG button (`hidden = "saveaspng"`), so Download is the one way to save. `saveaspng = FALSE` does not hide it: `girafe()` merges the toolbar options by the arguments named in the call and drops the `hidden` entry that `saveaspng = FALSE` sets.
  - The funnel plot's toolbar has no zoom with rectangle (`hidden = "zoom_rect"`): the maintainer found it zoomed out instead of in, and only reset pan/zoom brought the plot back. The map keeps it. The cause was not investigated; the funnel plot's `coord_cartesian(expand = FALSE)` limits are a candidate.

## Open items

- **Sidebar spacing (waiting on the maintainer).** Option B is in place; see Decisions, Spacing.

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
- **ICD-10-AM editions 12 and 13.** `Analysis.dbo.ICD10AM` stops at edition 11. Codes new in later editions have no titles: the search does not offer them and the note under the box calls them not in editions 3 to 11. Typed, they still select events. Adding the editions to the table needs no code change.
- **Start-up time.** The titles query runs when the app starts. Its time on EpiServer has not been measured.
- **Procedure code format.** The data's `Procedure` fields are `varchar(8)`, so they may hold `11012-00` or `1101200`; the SQL handles both. The live test "the ICD-10-AM and ACHI titles cover the codes recorded" checks that the codes and blocks of 2018 meet the table's.
- **Wording of mixed searches.** The info bar joins the two lists with "and" or "or", for example "principal diagnosis I21 or J46 or any procedure [1826]". With several codes on each side it can read ambiguously.
- **Download with nothing to save.** The Download menu's choices stay available before Calculate, at state level and, for the map, when the boundaries cannot be downloaded. A choice then fails in the browser's downloads. Disabling them would need the server to tell the page when each plot exists.
- **Boundary cache in tests.** `testthat::test_local()` writes `asgs2021_sa3_ACT_v1.rds` to the user's cache folder (`R_USER_CACHE_DIR`), on `main` too. No test file does so when run alone, and the test responsible was not found.
- **R CMD check notes,** also on `main`:
  - "Namespace in Imports field not imported from: 'labelled'";
  - the examples write two files to the sass cache.

## How to check a change

**Offline:**
- Run the three commands in CLAUDE.md. With no EpiServer, these tests skip: 18 that need EpiServer, 1 that needs an ODBC driver, and 1 that needs the ABS service when it cannot be reached. In a cloud session, run the check in a UTF-8 locale (see Pitfalls in `dev/harness/README.md`).
- `test-dsr_validation.R` needs `PHEindicatormethods` and `epitools`, and skips without them.
- For the app in a browser, run `Rscript dev/harness/run_app.R dev/harness/dsr_calculator.js <output dir>`. Compare the output with the results recorded in `dev/harness/README.md`, and look at the screenshots.
- For the live tests, run `Rscript dev/harness/dryrun_live.R`.
- When a SQL builder changes, update `dev/harness/fake_episerver.R`, and `dev/harness/fake_live.R` if the live tests send the query. Add or update a live test too (CLAUDE.md).

**Against EpiServer:** the maintainer runs `testthat::test_local()`, where `test-episerver_dsr_live.R` must run rather than skip. They then use the app as described in section 3 of the pull request.
