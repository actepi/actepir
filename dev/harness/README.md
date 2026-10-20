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
| `fake_episerver.R` | `fake_query(sql)` answers every query the DSR Calculator and the EpiServer Browser send. The counts are synthetic, for 14 SA3s and their SA2s, 2001 to 2024. The ICD-10-AM titles are invented, for 28,600 diagnosis codes and 2,002 procedure codes in 201 blocks. |
| `fake_abs.R` | `fake_fetch_json(url)` answers the ABS boundary requests with boxes around Canberra, coded as in `fake_episerver.R`. |
| `run_app.R` | Installs the working tree into a temporary library, starts an app through its real background launcher with the fakes swapped in, and runs a Playwright script against it. |
| `dsr_calculator.js` | Baseline checks of the DSR Calculator: layout, section headings, information buttons, scrolling and resizing the sidebar, hover, Calculate, the three tables and Copy table with its notification, Copy Code, the problem report, the top bar, Explore (its settings panel, toolbars, the Download menu and its four files, and state level), Help, and the page once the app stops. |
| `episerver_browser.js` | Baseline checks of the EpiServer Browser. |
| `fake_live.R` | `fake(sql)` answers every query in `test-episerver_dsr_live.R`. Diagnosis conditions are applied to synthetic coded records. |
| `dryrun_live.R` | Runs the live tests against `fake_live.R`. |

## Running

Run these from the repository root, with output directories outside the repository, such as the session's scratchpad:

```sh
Rscript dev/harness/run_app.R dev/harness/dsr_calculator.js <out>/dsr
Rscript dev/harness/run_app.R dev/harness/episerver_browser.js <out>/browse browse
Rscript dev/harness/dryrun_live.R
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

## How it works

- **Swapping in the fakes.** The apps reach EpiServer only through `.epi_connection()`, and the ABS service only through `.dsr_fetch_json()`.
  - `run_app.R` writes an `.Rprofile` into a temporary working directory. It sources both fakes and, when the package loads, replaces those two functions in its namespace with `assignInNamespace()`.
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

## Keeping it working

- **When the SQL changes,** update the fakes in the same pull request. They answer by matching fragments of the SQL, such as `AS Events` or `MIN(ERPYear)`, and stop on anything else (`fake_query: unexpected SQL`).
  - Update `fake_episerver.R` for any query an app sends.
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
- **Download menu.** Choosing a file closes the menu, so open it (`#ex_download`) again before the next. Shiny marks a download link `disabled` until it has its address; the script reports it.
- **ggiraph.** Map areas and funnel points are SVG elements whose `data-id` is the area code. Tooltips are separate `div`s shown on hover; find them by their text.
  - A toolbar shows only while the pointer is over its plot; hover first. Its buttons have the class `ggiraph-toolbar-icon-<key>`, such as `ggiraph-toolbar-icon-zoom_reset`, and their tooltip as `title`.
  - When a plot's `req()` fails, htmlwidgets hides the last plot drawn (`visibility: hidden`) rather than removing it, so check visibility, not the SVG elements.
- **Network.** `www.abs.gov.au`, `www.aihw.gov.au` and `geo.abs.gov.au` are blocked unless the environment's network settings allow them; when `geo.abs.gov.au` is blocked, the ABS boundary test skips. Packages install through Posit Package Manager.
- **Packages.** The setup script installs a later roxygen2 (8.1.1 at pull request #8) than the 8.0.0 that `DESCRIPTION` records; install that with `remotes::install_version("roxygen2", "8.0.0")`.
- **For the environment's setup script (waiting on the maintainer).** Installing roxygen2 8.0.0, and setting `LANG=C.UTF-8` (or adding the `en_US.UTF-8` locale), would remove the two workarounds above.
