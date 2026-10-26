# Offline harness

EpiServer and the ABS boundary service cannot be reached from cloud sessions or CI, and `testthat::test_local()` does not open the apps in a browser. This folder fills that gap. It has three parts:
- synthetic stand-ins for both services;
- scripts that run the Shiny apps on them in headless Chromium;
- a dry run of the live tests.

None of it is in the built package, because `dev/` is in `.Rbuildignore`.

The harness shows that the apps and the test code work on synthetic data. It cannot show that the SQL suits the real tables, or that results are right for real data. Only the maintainer's run against EpiServer shows that.

## Requirements

- R with the package's Imports and Suggests, plus `pkgload` and `callr`.
- Node.js with the `playwright` package and Chromium.
  - Cloud sessions have both preinstalled (`NODE_PATH` and `PLAYWRIGHT_BROWSERS_PATH` are set). Do not run `playwright install` there.
  - Elsewhere, run `npm install playwright` and then `npx playwright install chromium`.

## Files

| File | Does |
|---|---|
| `fake_episerver.R` | `fake_query(sql)` answers every query the DSR Calculator sends. The counts are synthetic, for 14 SA3s and their SA2s, 2001 to 2024. The ICD-10-AM titles are invented, for 28,600 diagnosis codes and 2,002 procedure codes in 201 blocks. It passes the Dataset Browser's queries to `fake_browse_episerver()` in `tests/testthat/helper-browse.R`, the synthetic databases, tables and label tables that the offline tests also use. |
| `fake_abs.R` | `fake_fetch_json(url)` answers the ABS boundary requests with boxes around Canberra, coded as in `fake_episerver.R`. |
| `run_app.R` | Installs the working tree into a temporary library, starts an app through its real background launcher with the fakes swapped in, and runs a Playwright script against it. |
| `dsr_calculator.js` | Baseline checks of the DSR Calculator: layout, section headings, information buttons, scrolling and resizing the sidebar, hover, Calculate, the three tables and Copy table with its notification, Copy Code, the problem report, the top bar, Explore (its settings panel, toolbars, the Download menu and its four files, and state level), Help, and the page once the app stops. |
| `episerver_browser.js` | Baseline checks of the Dataset Browser: layout, section headings, spacing, information buttons, the Table list with its dataset labels and search, where each table's labels come from, the description and columns, fitting the window, value labels, choosing rows, hover, Copy Code with its notification, a label table chosen, another schema and database, Help, the problem report, and the page once the app stops. |
| `fake_live.R` | `fake(sql)` answers every query in `test-episerver_dsr_live.R`. Diagnosis conditions are applied to synthetic coded records. It passes the queries of `test-episerver_browse_live.R` to `fake_browse_episerver()`. |
| `dryrun_live.R` | Runs the live tests against `fake_live.R`. With `--without-dataproperties`, the fake `Analysis` has no `ref.DataProperties`, as before the label table is created. |

## Running

Run these from the repository root, with output directories outside the repository, such as the session's scratchpad:

```sh
Rscript dev/harness/run_app.R dev/harness/dsr_calculator.js <out>/dsr
Rscript dev/harness/run_app.R dev/harness/episerver_browser.js <out>/browse browse
Rscript dev/harness/dryrun_live.R
Rscript dev/harness/dryrun_live.R --without-dataproperties
```

- The browser scripts print one line per check, in the form `LABEL: value`. They save screenshots to the output directory: look at them.
- A run of `run_app.R` with `dsr_calculator.js` takes about 90 seconds, with the install.
- In the app's error stream, lines starting `fake ABS:` are the boundary requests the fake answered.

These were the results when the harness was added (pull request #6):

| Run | Results |
|---|---|
| `dsr_calculator.js` (ED, ACT residents by SA3, 2022 to 2023) | 20 rate rows, 4 of them n.p. "with a population under 30 in an age group" (SA3s 80103 and 80111). Copy table gives 20, 360 and 40 rows. 40 highlighted cells in Counts by age. Explore shows 10 map areas and 8 funnel points. Help has 8 sections. Hover goes from `rgb(50, 5, 87)` to `rgb(86, 44, 140)`. |
| `episerver_browser.js` | 5 columns listed, and Iris on hover. |
| `dryrun_live.R` | 10 tests, 214 expectations, 0 failed, 0 skipped. |

Pull request #7 added the diagnosis and procedure search checks to `dsr_calculator.js` and two live tests. The results above were unchanged, and the new ones were:

| Run | Results |
|---|---|
| `dsr_calculator.js`, diagnosis codes | `myocard` lists I21, I21.0, I21.4, I22 and I25.2. `old myoc` and Enter adds I25.2, with its title on hover. `I25-I27` and Enter, and `J4` and Tab, add what was typed. `zzzz` and Tab adds nothing. The note spells out the three entries. Clicking the empty box opens no list. |
| `dsr_calculator.js`, procedure codes | With ED the box and both choices are disabled, and the note says ED has no procedures; with APC they are enabled. `nerve` lists [900], 39001-00 and 39001-01, and Enter adds [900]. `39001-01` and Enter, and `1-5` and Tab, add what was typed. The note spells out the three entries. With "or", the info bar reads "APC separations, principal diagnosis I25.2, I25-I27 or J4 or principal procedure [900], 39001-01 or [1] to [5], ...". |
| `dryrun_live.R` | 12 tests, 235 expectations, 0 failed, 0 skipped. |

Pull request #8 removed the Map tab, the menu and Australian residents, and added checks of the Explore tab and of the two highlight colours in Counts by age. The other results above were unchanged, and the new ones were:

| Run | Results |
|---|---|
| `dsr_calculator.js`, tabs | Tables, Explore and Help. |
| `dsr_calculator.js`, Counts by age | 40 populations highlighted `rgb(241, 236, 194)` and no event totals. With the diagnosis and procedure codes, 40 populations and 4 event totals, these `rgb(191, 227, 241)`; the note shows each in its colour. |
| `dsr_calculator.js`, Explore | The map's toolbar icons are Iris, `rgb(86, 44, 140)`, except lasso deselection, which stays red, and there is no PNG button. The funnel plot's toolbar has no zoom with rectangle. The Download menu lists Map as PDF, Map as PNG, Funnel plot as PDF and Funnel plot as PNG, all enabled, and each downloads `dsr-map-dsr-2023-persons.pdf`, `.png`, `dsr-funnel-dsr-2023-persons.pdf` or `.png`. With the ACT and surrounds by state, the note reads "Choose SA3 or SA2 to compare areas within the ACT and surrounds." and neither plot is shown. |
| `episerver_browser.js` | Unchanged, through `episerver_browse()` with no display argument. |
| `dryrun_live.R` | 11 tests, 192 expectations, 0 failed, 0 skipped: the test of `ERP5_STE_AUS` and the Australian residents' queries are gone. |

Pull request #9 added checks of the sidebar, the notification and the problem report. The script now shows scrollbars, as Windows does. The results above were unchanged, and the new ones were:

| Run | Results |
|---|---|
| `dsr_calculator.js`, sidebar | Sections Events, Population (Geographic scope, Geographic level, Ages, Male and female), Time periods and Rate details. Headings `rgb(234, 230, 241)`, text `rgb(50, 5, 87)`, weight 600. Each of the four information buttons (`codes_help`, `diagnosis_help`, `procedures_help`, `min_events_help`) shows its note and hides it again. With a 15px scrollbar, the wheel from the results scrolls the sidebar at 5 of 5 points across the scrollbar, dragging the thumb scrolls it, and dragging the edge 120px makes it 460px wide. |
| `dsr_calculator.js`, notification | "Table copied to the clipboard" on `rgb(234, 230, 241)`, text and left border `rgb(50, 5, 87)`. |
| `dsr_calculator.js`, problem report | The dialog "Report a problem" asks for an email to healthinfo@act.gov.au. In a 1440 by 650 window it fits, the wheel does not move it, and the details scroll. Copy report copies the sections What happened, Calculator, Settings, R code for these settings and R session, with the text typed, `Calculated: yes`, the settings, actepir's version and the browser. |
| `dsr_calculator.js`, Explore | Settings down the left: Show, Year, Sex, Colour the map by, Allow for overdispersion, Selection and zoom, Zoom to selected, Show all, Clear selection and Download. The settings end at x = 596 and the map starts at 612; the plots end at y = 680 in a 900-pixel window. The panel has no right border and is 453px tall; in a 650-pixel window, scrolling 159px moves it up 112px, as it is held in view until the end of the tab's content. |
| `dsr_calculator.js`, top bar | The name 33px from the left, the tabs 48px after it, the logo 33px from the right, 60px tall in a 104px bar (56px since the actepir lockup replaced the ACT Government logo), all three centred at y = 52, and no Done. Closing the page's connection shows "The DSR Calculator is no longer running." and how to open it again. |
| `episerver_browser.js` | Unchanged results. The labels, boxes and gaps take the shared spacing (`.epi_app_style()`): 15px regular labels, 14px boxes, 10px gaps. |
| `dryrun_live.R` | Unchanged: no query changed. |

Pull request #13, the first of the Dataset Browser's plan (`dev/notes/dataset-browser.md`), rebuilt the browser as a bslib app, and moved its fake to `tests/testthat/helper-browse.R`. `episerver_browser.js` was rewritten; the DSR Calculator's results above were unchanged. The new results were:

| Run | Results |
|---|---|
| `episerver_browser.js`, layout | Tabs Columns, Collection and Help. The name 33px from the left, the tabs 48px after it, the logo 33px from the right, 56px tall in a 104px bar, all centred at y = 52, and no Done. Sections Dataset (Database, Schema, Table), Labels (Labels table, Show value labels) and Code (Connection, Declare packages, Import with labels); headings `rgb(234, 230, 241)`, text `rgb(50, 5, 87)`, weight 600. The sidebar 340px wide and resizable, with Copy Code and the bug button at its foot. 15px regular labels, 14px boxes, 10px gaps. Both information buttons show their notes and hide them again. |
| `episerver_browser.js`, Table list | Databases Analysis and HospitalAPC. Groups Tables and Views; the options ACTGHSMYX, APC, Deaths and ED show their dataset labels in `rgb(119, 119, 119)`, or in white at 0.8 opacity on the chosen option, and Births, Plain and vw_ED none. Backspace and `emergency` lists ED alone, and Enter chooses it. |
| `episerver_browser.js`, labels | ACTGHSMYX, APC and Births from ref.DataProperties; ED and vw_ED (a view) from ref.DataLabels, "as ref.DataProperties holds none for this table"; Deaths from dbo.DataLabels, "as ref.DataProperties and ref.DataLabels hold none for this table"; Plain from none of the three. With ref.DataLabels chosen, APC's labels and its label in the list come from it; with dbo.DataLabels, "dbo.DataLabels holds no labels for this table". stage.APC takes its own labels from ref.DataProperties. HospitalAPC has no label table. |
| `episerver_browser.js`, columns | APC: 43 columns, with descriptions, value labels (shown by default) and notes in code order. Above them, Expand value labels, Collapse value labels, Clear selection (disabled) and "No columns chosen: the code reads them all."; the table's header is 98px below the info bar. The table ends 16px above the window's bottom at 1920 by 950 and 1536 by 730 pixels, and the page does not scroll. Expand opens the one list, to 1 = Male, 2 = Female, 9 = Not stated and 10 = Other; Collapse closes it; opening it by hand chooses no row. With Show value labels off, Expand and Collapse are disabled at opacity 0.65 and the column goes. Two rows chosen are `rgb(220, 200, 250)` with text `rgb(50, 5, 87)` and an Iris bar, `rgb(86, 44, 140)`, 4px wide in the first cell; the line reads "2 columns chosen for the code." and Clear selection is enabled. Copy Code gives `dplyr::select(Sex, AgeYrs)`. Clear selection then clears both rows and is disabled again. Hover goes from `rgb(50, 5, 87)` to `rgb(86, 44, 140)`. |
| `episerver_browser.js`, Collection | For APC the tab holds its description, three notes in code order and a linked table; for Plain, "This table has no description, notes or linked tables." The tab is titled "Collection" for both. |
| `episerver_browser.js`, Help, report and stop | Help has 5 sections in 13px text, without the info bar. The report has the sections What happened, Browser, Settings, R code for these settings and R session, with the label tables and where the labels came from. Closing the connection shows "The Dataset Browser is no longer running." |
| `dryrun_live.R` | 18 tests, 252 expectations, 0 failed, 1 skipped: the browser's 7 tests and the calculator's 11. The test that runs the copied code skips, as it connects through ODBC itself and needs a SQL Server driver. With `--without-dataproperties`, 18 tests, 249 expectations, 0 failed, 1 skipped. |

Plan pull request 2 (`dev/notes/dataset-browser.md`) added `test-episerver_getlabels_live.R`. Its tests that use the app's connection run in the dry run against `fake_browse_episerver()`; the two that read lazy tables need an ODBC driver and skip. `episerver_browser.js` also copies the code with a label table chosen. The browser's other results and the calculator's were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, labels | With ref.DataLabels chosen, Copy Code gives `actepir::collect_withlabels(lbl_schema = "ref", lbl_table = "DataLabels")`. |
| `dryrun_live.R` | 22 tests, 285 expectations, 0 failed, 3 skipped (the three tests that need an ODBC driver). With `--without-dataproperties`, 22 tests, 281 expectations, 0 failed, 3 skipped. |

The launcher fix (`dev/notes/shared-apps.md`, background process) starts each app in one process, without processx's supervisor. The results of both scripts and of the dry run were unchanged. The supervisor runs in cloud sessions, so the harness never showed the fault it fixed. To see it with an older launcher, block the supervisor before launching, with `utils::assignInNamespace("supervisor_watch_pid", function(pid) stop("system error 1260"), "processx")`, and start R with a large heap (`R_NSIZE=20000000 R_VSIZE=1500M`), so that garbage collection does not run during the launch; then call `gc()` and check whether the app's port is still open.

## How it works

- **Swapping in the fakes.** The apps reach EpiServer only through `.epi_connection()`, and the ABS service only through `.dsr_fetch_json()`.
  - `run_app.R` writes an `.Rprofile` into a temporary working directory. It sources `tests/testthat/helper-browse.R` and both fakes and, when the package loads, replaces those two functions in its namespace with `assignInNamespace()`.
  - `callr` starts the app's background process in that directory and reads its `.Rprofile`, so the app gets the fakes with no change to the package.
- **The real launcher.** The app runs through `episerver_dsr()` or `episerver_browse()`, in a background process, as analysts run it. `options(browser =)` stops it opening a browser, because Playwright opens its own.
- **Isolation.**
  - The package is installed into a temporary library.
  - `R_USER_CACHE_DIR` points to a temporary folder, so the boundary and theme caches do not touch the user's.
  - Only screenshots and `install.log` go to the output directory.
- **Driving the page.**
  - Scripts set inputs through their widgets, for example `$('#id')[0].selectize.setValue()` or `$('#id').data('ionRangeSlider').update()`.
  - They click buttons, wait until Shiny is idle (no `shiny-busy` class on the page) and read the page.
  - Reading the clipboard needs `grantPermissions(['clipboard-read', 'clipboard-write'])`.
  - Playwright hides scrollbars unless Chromium is launched with `ignoreDefaultArgs: ['--hide-scrollbars']`, as `dsr_calculator.js` is. The sidebar checks need the scrollbar.
  - A closed `<details>` hides its content with `content-visibility`, so test whether a note shows with `checkVisibility()`, not `offsetHeight`.
- **The dry run.**
  - `dryrun_live.R` copies the package's test helpers and the `_live` test files to a temporary directory, and adds a last helper that sources `fake_live.R`. It then runs them with `testthat::test_dir()` after `pkgload::load_all()`.
  - `fake_live.R` defines `.epi_connection()` in the test environment, where the tests find it before the package's. It also replaces it in the namespace for the apps the tests run, and turns `skip_if_no_episerver()` and `skip_if_no_abs()` into no-ops.
  - The package's helpers are copied with the tests, so `fake_live.R` finds `fake_browse_episerver()` there. `dryrun_live.R` sets `browse_properties` for it in the last helper.

## Keeping it working

- **When the SQL changes,** update the fakes in the same pull request. They answer by matching fragments of the SQL, such as `AS Events` or `MIN(ERPYear)`, and stop on anything else (`fake_query: unexpected SQL`).
  - Update `fake_episerver.R` for any query the DSR Calculator sends, and `tests/testthat/helper-browse.R` for the Dataset Browser's. The helper reads the browser's SQL rather than matching fragments, so a change to a condition or a filter may need its reading changed too.
  - Update `fake_live.R` if the live tests send the query too.
- **When an ID changes,** update the scripts. They find elements by the input and output IDs in the app's code.
- **For a new tool,** add its queries to `fake_episerver.R`, a script like `dsr_calculator.js`, and a case in `run_app.R`.
- **Keep checks worth repeating** in the scripts. One-off checks can go in a scratch copy.

## Pitfalls

- **C locale.** Cloud sessions run R in the C locale, where a non-ASCII character in R source, such as an en dash, is read as `<U+2013>`. Write `"–"` in R code, and compare such strings in tests with `fixed = TRUE`.
- **Sass cache.** Compiling the bslib theme writes to the user's cache folder unless `sass.cache` is off, so tests that compile it set `withr::local_options(sass.cache = FALSE)`.
  - To check that the tests write nothing there, run `R_USER_CACHE_DIR=$(mktemp -d) Rscript -e 'testthat::test_local()'` and list the folder.
  - R CMD check's examples write two files to it. This is true on `main` too.
- **rcmdcheck.** When scripting it, give `check_dir` an explicit temporary path. An unset shell variable once put the check directory at the root of the file system.
  - In the C locale the check gives a warning, `Sys.setlocale("LC_CTYPE", "en_US.UTF-8")` cannot be honoured, as cloud containers have no `en_US.UTF-8` locale. It comes from the container, not the package. Run the check with `LANG=C.UTF-8 LC_ALL=C.UTF-8` set, as `C.UTF-8` is installed.
- **DataTables.** With `scrollX`, the header is drawn in a separate table, so `thead` matches twice. Read rows with `#<output id> tbody tr`, and scroll with `#<output id> .dataTables_scrollBody`.
- **Waiting for Shiny.** A choice can take two round trips, as when the Table list is updated and then the table read. Wait until the page has had no `shiny-busy` class for a while, as `settle()` in `episerver_browser.js` does, rather than for its first absence.
- **Single selects.** selectize ignores typing in a single select while it holds an item: press Backspace first, as an analyst would. A select's label has the ID `<id>-label`; its `for` changes once selectize starts.
- **Download menu.** Choosing a file closes the menu, so open it (`#ex_download`) again before the next. Shiny marks a download link `disabled` until it has its address; the script reports it.
- **ggiraph.** Map areas and funnel points are SVG elements whose `data-id` is the area code. Tooltips are separate `div`s shown on hover; find them by their text.
  - A toolbar shows only while the pointer is over its plot; hover first. Its buttons have the class `ggiraph-toolbar-icon-<key>`, such as `ggiraph-toolbar-icon-zoom_reset`, and their tooltip as `title`.
  - When a plot's `req()` fails, htmlwidgets hides the last plot drawn (`visibility: hidden`) rather than removing it, so check visibility, not the SVG elements.
- **Network.** `www.abs.gov.au`, `www.aihw.gov.au` and `geo.abs.gov.au` are blocked unless the environment's network settings allow them; when `geo.abs.gov.au` is blocked, the ABS boundary test skips. Packages install through Posit Package Manager.
- **Packages.** The setup script installs a later roxygen2 (8.1.1 at pull request #8) than the 8.0.0 that `DESCRIPTION` records; install that with `remotes::install_version("roxygen2", "8.0.0")`.
- **For the environment's setup script (waiting on the maintainer).** Installing roxygen2 8.0.0, and setting `LANG=C.UTF-8` (or adding the `en_US.UTF-8` locale), would remove the two workarounds above.
