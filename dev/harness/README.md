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
| `episerver_browser.js` | Baseline checks of the Dataset Browser: layout, section headings, spacing, information buttons, the Table list with its dataset labels and search, where each table's labels come from, the description and columns, fitting the window, value labels, choosing rows, hover, Copy Code with its notification, a label table chosen, the Download menu and the dictionary files it gives, Dictionaries of all tables with its folder list, written to a folder, another schema and database, the Tables tab, on which it opens, and its clicks, the Var Finder tab and its clicks, the Profiler tab and its whole table, Help, the problem report, and the page once the app stops. |
| `fake_quarto/quarto` | Stands in for the Quarto in RStudio, which cloud sessions lack, for `quarto typst compile`, with pip's `typst` package at a 0.13 version. Put its folder first on the PATH to make the dictionaries' PDFs (see Running). |
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
- Cloud sessions have no Quarto, so the dictionaries' PDF item is disabled. To make the PDFs, run `pip install "typst>=0.13,<0.14"` and put `dev/harness/fake_quarto` first on the PATH: `PATH="$PWD/dev/harness/fake_quarto:$PATH" Rscript dev/harness/run_app.R ...`. The same PATH lets the offline test that compiles a PDF run rather than skip. Render the pages with the same package (`typst.compile(..., format="png")`) to look at them.

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

Plan pull request 3, the full dictionaries (`dev/notes/dictionaries.md`), added the Download menu to the browser and `test-episerver_dictionary_live.R`. `episerver_browser.js` opens the menu and downloads the dictionary; the calculator's results were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, foot and Help | The foot holds `insert_code`, `dict_download` and `bug_report`; Help has 6 sections, Dictionary before Problems. Otherwise unchanged. |
| `episerver_browser.js`, Download | The menu opens upwards, with "Dictionary as PDF (needs Quarto)", disabled, "Dictionary as HTML" and "Dictionaries of all tables…". `APC_dictionary.html` downloads; opened alone it is headed "Admitted patient care", with "Labels from ref.DataProperties.", 43 rows, "1 of 43 columns" when searched for "female", and no requests for other files. |
| `episerver_browser.js`, Dictionaries of all tables | The dialog offers the schema `Analysis.dbo`, chosen, and the whole database; HTML, chosen, and PDF, disabled; the folder `dictionaries` in the app's working directory, which does not exist, so the folder list notes that it will be made and lists the working directory's folders. The folder list (`picker` in the output directory, holding `2024`, `Alpha/Inner`, `beta` and `.hidden`) shows Up, Home and Working directory, then 2024, Alpha and beta, in a list 220px high at most that scrolls (`overflow-y: auto`, `overscroll-behavior: contain`), with rows 31px tall. A click on Alpha puts `…/picker/Alpha` in the box and lists Inner; Up, then 2024, gives `…/picker/2024`; `…/picker/new/folder` lists picker's folders with the note. Writing the whole database to the output directory's `bulk` folder gives "Wrote 7 dictionaries, as HTML, to …/bulk." and "7 of the 9 tables and views of Analysis have labels; the others were left out.", with the HTML files of `dbo` (ACTGHSMYX, APC, Births, Deaths, ED, vw_ED) and `stage` (APC). |
| The same, with `dev/harness/fake_quarto` first on the PATH | The PDF item is enabled, and `APC_dictionary.pdf` downloads, starting `%PDF-`. In Dictionaries of all tables, PDF is enabled and chosen, and the 7 dictionaries are written "as HTML and PDF", one PDF beside each HTML file. |
| `dryrun_live.R` | 25 tests, 316 expectations, 0 failed, 4 skipped (those that need an ODBC driver). With `--without-dataproperties`, 25 tests, 312 expectations, 0 failed, 4 skipped. |

Plan pull request 4 (`dev/notes/dataset-browser.md`) added the Tables and Var Finder tabs, and two live tests. `fake_browse_episerver()` answers their queries, with invented dates (one month apart in catalogue order, altered 100 days after) and rows. `episerver_browser.js` opens both tabs; the browser's other results and the calculator's were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, tabs and Help | The tabs are Tables, Columns, Collection, Var Finder and Help. The browser opens on Tables, and the Columns tab's table is not drawn until that tab is shown, as Shiny suspends the outputs of a hidden tab. Help has 8 sections: Finding a table, Tables, Var Finder, Labels, Choosing columns, Code, Dictionary and Problems. |
| `episerver_browser.js`, Tables | "7 tables and views in Analysis.dbo. Click one to choose it.", with the info bar shown. The headings are Table, Description, Type, Rows, Columns, Without var labels, Labels from, Created and Structure changed, and all but Table, Type and Columns have a title. APC reads "APC, Admitted patient care, Table, 1,234,567, 43, 41, ref.DataProperties, 2020-02-14, 2020-05-24"; Deaths takes dbo.DataLabels and ED ref.DataLabels; Plain has no label table; vw_ED is a View with no rows. APC, the table chosen, is marked. The page does not scroll at 1920 by 950, and at 1536 by 730 the table needs no sideways scrolling. A click on ED chooses it in the sidebar and marks it; choosing ACTGHSMYX in the sidebar marks it. |
| `episerver_browser.js`, Var Finder | The settings panel is 240px wide, with Column names, Variable labels and Value labels ticked. "female" with Enter gives "2 columns of Analysis match "female"." (dbo.ACTGHSMYX.RSEX_Y20 and dbo.APC.Sex, "2 = Female"), and the results have no search box of their own. "staged", with the button clicked straight after typing, gives stage.APC.Sex, "Sex as staged"; a click on it opens the Columns tab on Analysis.stage.APC. |
| `dryrun_live.R` | 27 tests, 355 expectations, 0 failed, 4 skipped (those that need an ODBC driver). With `--without-dataproperties`, 27 tests, 350 expectations, 0 failed, 4 skipped. |

Plan pull request 5 (`dev/notes/dataset-browser.md`) added the Profiler tab, and a live test. `fake_browse_episerver()` answers its queries from invented rows of each table, gives `Analysis.dbo.APC` a primary key, `PatientID`, and takes the database from the first `FROM [...]`, as the profile's SQL names columns before it. `episerver_browser.js` opens the tab; the browser's other results were unchanged. The settings placed straight in a tab's settings panel are now spaced (`dev/notes/dataset-browser.md`, Spacing in the panel), so the calculator's Explore panel is taller; its other results were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, tabs and Help | The tabs are Tables, Columns, Profiler, Collection, Var Finder and Help. Help has 9 sections, with Profiler after Choosing columns. |
| `episerver_browser.js`, Profiler | The settings panel is 240px wide, offering Sex, the first column chosen, then AgeYrs and Diagnosis1; PatientID is left out, as "Columns left out (1)" says, and the note names it "PatientID (key)". The Column list, opened, is drawn on the page's body, and the panel does not scroll. "Profile of Analysis.dbo.APC.Sex (Sex of the patient).", with the summary in the panel: Rows 150, Missing (NULL) 15 (10.0%), Distinct values 5, Minimum 1 (Male), Maximum 10 (Other), Value labels 4, Codes without labels 1 code: 4; counts 1 Male 45 30.0, 2 Female 45 30.0, NA 15 10.0, 4 (no label) 15 10.0, 9 Not stated 15 10.0, 10 Other 15 10.0. At 1536 by 730 the table runs from y = 330 to 674, and neither the page nor the panel scrolls. By Diagnosis1: headings I21.0, J18.9, S72.0 and Total; rows 1 Male 15 15 15 45 to NA 5 5 5 15, and Total 50 50 50 150; Per cent of row gives 33.3 33.3 33.3 100.0. Deaths.Cause: Blank text 8 (26.7%), and the maximum cut at 60 characters and the longest value at 200. |
| `dsr_calculator.js`, Explore | The panel is 503px tall, where it was 453px, as its settings are 10px apart; in a 650-pixel window, scrolling 159px moves it up 142px. |
| `dryrun_live.R` | 28 tests, 468 expectations, 0 failed, 4 skipped (those that need an ODBC driver). With `--without-dataproperties`, 28 tests, 463 expectations, 0 failed, 4 skipped. |

Plan pull request 6 (`dev/notes/dataset-browser.md`) added the Profiler's whole table, and extended the live tests. It sends no new SQL, so the fakes are unchanged. `episerver_browser.js` profiles APC's whole table; the browser's other results and the calculator's were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, whole table | With The whole table chosen, Column, By and the summary are hidden, and Stop shows only during a run. "Profile of Analysis.dbo.APC, 150 rows: 42 columns, 1 left out, in 10 s. Click a column to profile it alone.": 42 rows, as the fake waits 0.2 s on each query and 43 are sent (a summary for each column, and counts for Sex, the one column with value labels). Sex reads "Sex of the patient, int, 10.0, (blank), 5, 1 (Male), 10 (Other), 4, 1, 4". Every heading but Column, Description and Type has a title. The table ends at y = 894 in a 950-pixel window, and neither the page nor the panel scrolls. Sorted twice by Codes without labels, Sex is first. A click on AgeYrs chooses One column and gives "Profile of Analysis.dbo.APC.AgeYrs (Age in years (<1 is 0))."; The whole table again shows the 42 rows with no new read, and a second click on AgeYrs, after Column was set to Sex, chooses AgeYrs again. |
| `dryrun_live.R` | 28 tests, 562 expectations, 0 failed, 4 skipped (those that need an ODBC driver). With `--without-dataproperties`, 28 tests, 557 expectations, 0 failed, 4 skipped. |

Three checks were run once, with scripts not kept:
- **Stop.** 2 s into APC's whole table, with the fake's 0.2 s wait, the note read "Profiling column 8 of 42." and Stop showed; a click on it gave "stopped after 9 of 42 columns", the column being read finished.
- **Steps.** With the fake's wait taken out, 42 columns took 0.85 s with one column at each step, as built, and 0.36 s with a second's columns at each step, a version tried and dropped.
- **The tallest panel.** At 1536 by 730, the panel with Sex by Diagnosis1 and its summary ends at y = 825, so the tab's main area scrolls 135px. The switch between one column and the whole table adds 84px to the panel.

Plan pull request 7 (`dev/notes/dataset-browser.md`) moved Copy Code, the code's settings and Download to the Columns tab's own panel. `episerver_browser.js` checks the information buttons of the sidebar and of the Columns tab's panel in turn, the panel, the table's widths, the notes list and where the Download menu opens; the browser's other results and the calculator's were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, sidebar and Columns tab | The sidebar holds Dataset and Labels, and its foot `bug_report` alone. The Columns tab's panel is 240px wide, with Connection, Declare packages and Import with labels, then `insert_code` and `dict_download`; its information button works. The table's header is 114px below the info bar, and the table ends at y = 894 in a 950-pixel window and 674 in a 730-pixel one, as on the Profiler and Var Finder tabs; it is 1276px wide at 1920 and 892px at 1536, where its columns are #, 42, Column, 156, Description, 311, Type, 90, Value labels, 190 and Notes, 87. AgeYrs's notes read "3 notes" and open without choosing the row. Copy Code copies the same code as before. |
| `episerver_browser.js`, Download | The menu opens downwards with a fixed position, ending at y = 584 below the panel's foot at 467, and the panel does not scroll. Its items and downloads are unchanged. |

Plan pull request 8 (`dev/notes/dataset-browser.md`) added Copy dictionary for Excel and Dictionary logo to the Download menu. `episerver_browser.js` copies APC's dictionary and reads the clipboard, then opens the logo dialog, sets a white logo it makes as a screenshot with a transparent background, and goes back to the lockup. The logo is kept under the harness's `R_USER_CONFIG_DIR`, so the folder list of Dictionaries of all tables also lists `config`. The browser's other results and the calculator's were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, Download | The menu holds "Dictionary as PDF (needs Quarto)", disabled, "Dictionary as HTML", "Copy dictionary for Excel", "Dictionaries of all tables…" and "Dictionary logo…". It ends at y = 648, and the panel does not scroll. |
| `episerver_browser.js`, Excel | The clipboard holds `text/plain` and `text/html`: 47 lines of text, the headings and APC's 46 rows (Sex's code 9 reads "Analysis.dbo.APC, 2, Sex, Sex of the patient, int, 9, Not stated"), and an HTML table of 47 rows whose 322 cells but the positions (46 rows of 7) carry `mso-number-format`, with AgeYrs's notes in one cell. |
| `episerver_browser.js`, logo | The dialog shows the lockup (`data:image/svg+xml`) 48px tall on Nightshade, rgb(50, 5, 87), and "The dictionaries show the actepir lockup." ("logo" since plan pull request 9); a 640 by 200 PNG chosen gives `data:image/png` and "The dictionaries show your logo, 640 by 200 pixels.", with no warning; Use the actepir lockup (now Use the actepir logo) goes back. |
| `dryrun_live.R` | 28 tests, 576 expectations, 0 failed, 4 skipped (those that need an ODBC driver). With `--without-dataproperties`, 28 tests, 571 expectations, 0 failed, 4 skipped. |

Plan pull request 9 (`dev/notes/dataset-browser.md`) changed the browser's wording, the Collection tab and the Help tab; the calculator's results were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, Collection | APC: "Admitted patient care", then "Dataset notes" with "Coded first.", "Coded second." and "Not coded.", then "Linked tables" with "APC_Extra (on PatientID)". Plain: "Analysis.dbo.Plain", then "Dataset notes" with "No dataset notes available for this collection.", and no Linked tables. |
| `episerver_browser.js`, logo | "The dictionaries show the actepir logo." before and after a logo is set. |
| `episerver_browser.js`, Help | 10 sections: Finding a table, Tables, Columns, Copying code, Dictionaries, Profiler, Collection, Var Finder, Where labels come from and Reporting a problem. |
| `dryrun_live.R` | 28 tests, 578 expectations, 0 failed, 4 skipped (those that need an ODBC driver). With `--without-dataproperties`, 28 tests, 573 expectations, 0 failed, 4 skipped. |

Plan pull request 10 (`dev/notes/dataset-browser.md`) reads the dataset description and the themes of dataset notes. `fake_browse_episerver()` gives `ref.DataProperties` the theme columns, says so in the label tables' query (Themes), returns them in the rows query when asked, and gives dbo.APC a description of two paragraphs and notes under the themes General and Data quality. The browser's other results and the calculator's were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, Collection | APC: "Admitted patient care", then "Dataset description" with "First paragraph." and "Second paragraph.", then "Dataset notes" with "General" ("Coded first.", "Coded second.") and "Data quality" ("Not coded."), then "Linked tables" with "APC_Extra (on PatientID)". Plain: "Analysis.dbo.Plain", then "No dataset description available for this collection." and "No dataset notes available for this collection." |
| `dryrun_live.R` | 28 tests, 582 expectations, 0 failed, 4 skipped (those that need an ODBC driver). With `--without-dataproperties`, 28 tests, 576 expectations, 0 failed, 4 skipped. |

Plan pull request 11 (`dev/notes/dictionaries.md`) changed the dictionaries' dataset information, the PDF's pages and the HTML file's layout. `episerver_browser.js` checks the downloaded HTML file's tabs and its search bar while scrolled; the browser's other results and the calculator's were unchanged.

| Run | Results |
|---|---|
| `episerver_browser.js`, dictionary HTML | Columns is chosen and shown, and Dataset information hidden; its tab shows Dataset description, Dataset notes, General, Data quality and Linked tables. Scrolled down, the search bar is at y = 0 and the table's headings at y = 50, its foot. The rest is unchanged: 43 rows, "1 of 43 columns" for "female", no requests. |
| The PDF of APC, compiled with pip's `typst` 0.13.7 and looked at as PNG | Page 1: the heading, then Dataset information with the description's two points and the notes under General and Data quality. Page 2 starts with Columns. |
| `dryrun_live.R` | 28 tests, 596 expectations, 0 failed, 4 skipped (those that need an ODBC driver). With `--without-dataproperties`, 28 tests, 590 expectations, 0 failed, 4 skipped. |

## How it works

- **Swapping in the fakes.** The apps reach EpiServer only through `.epi_connection()`, and the ABS service only through `.dsr_fetch_json()`.
  - `run_app.R` writes an `.Rprofile` into a temporary working directory. It sources `tests/testthat/helper-browse.R` and both fakes and, when the package loads, replaces those two functions in its namespace with `assignInNamespace()`.
  - `callr` starts the app's background process in that directory and reads its `.Rprofile`, so the app gets the fakes with no change to the package.
- **The real launcher.** The app runs through `episerver_dsr()` or `episerver_browse()`, in a background process, as analysts run it. `options(browser =)` stops it opening a browser, because Playwright opens its own.
- **Isolation.**
  - The package is installed into a temporary library.
  - `R_USER_CACHE_DIR` and `R_USER_CONFIG_DIR` point to temporary folders, so the boundary and theme caches and the Profiler's list of columns left out do not touch the user's.
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
- **Paths on Windows.** `dirname()` and `normalizePath(winslash = "/")` give forward slashes, while `tempdir()` and `file.path()` on it keep backslashes, so a test comparing paths normalises both sides. An offline test that passed in a cloud session failed on the maintainer's machine for this reason.
- **Download menu.** Choosing a file closes the menu, so open it (`#ex_download`) again before the next. Shiny marks a download link `disabled` until it has its address; the script reports it.
- **ggiraph.** Map areas and funnel points are SVG elements whose `data-id` is the area code. Tooltips are separate `div`s shown on hover; find them by their text.
  - A toolbar shows only while the pointer is over its plot; hover first. Its buttons have the class `ggiraph-toolbar-icon-<key>`, such as `ggiraph-toolbar-icon-zoom_reset`, and their tooltip as `title`.
  - When a plot's `req()` fails, htmlwidgets hides the last plot drawn (`visibility: hidden`) rather than removing it, so check visibility, not the SVG elements.
- **Network.** `www.abs.gov.au`, `www.aihw.gov.au` and `geo.abs.gov.au` are blocked unless the environment's network settings allow them; when `geo.abs.gov.au` is blocked, the ABS boundary test skips. Packages install through Posit Package Manager.
- **Packages.** The setup script installs a later roxygen2 (8.1.1 at pull request #8) than the 8.0.0 that `DESCRIPTION` records; install that with `remotes::install_version("roxygen2", "8.0.0")`.
- **For the environment's setup script (waiting on the maintainer).** Installing roxygen2 8.0.0, and setting `LANG=C.UTF-8` (or adding the `en_US.UTF-8` locale), would remove the two workarounds above.
