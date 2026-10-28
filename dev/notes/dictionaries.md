# Data dictionaries

The full data dictionary of any table or view on EpiServer, built from its label tables. `episerver_dictionary()` writes it as a PDF and a standalone HTML file and returns it as a data frame, for tables named or for every table with labels in a database or schema. The Dataset Browser's Download menu gives the same files for the table chosen, and writes them to a folder for every table with labels in the schema or database. It is pull request 3 of the plan in `dataset-browser.md`; the browser's improvements follow as pull request 4.

## Status

| Pull request | Content |
|---|---|
| The Dataset Browser's plan pull request 8 (open) | Copy dictionary for Excel (`.dict_excel()`) and the Dictionary logo dialog, in the browser's Download menu. |
| #18, plan pull request 3 (merged) | `episerver_dictionary()`, `dictionary_logo()`, the PDF and HTML files, and the browser's Download menu with Dictionaries of all tables. `create_dictionary()` removed. |

The design was agreed with the maintainer on 25 Oct 2026, narrowed on 26 Oct 2026 when the linkage dictionary was set aside, and settled the same day (see Decisions).

## Where the code is

| File | Holds |
|---|---|
| `R/episerver_dictionary.R` | `episerver_dictionary()` and `dictionary_logo()`; a database's tables and whether they have labels (`.dict_tables()`, `.dict_targets()`, `.dict_summary()`); writing many dictionaries (`.dict_write_all()`, which `episerver_dictionary()` and the browser share); the dictionary (`.dict_build()`, `.dict_assemble()`, `.dict_flat()`, `.dict_flat_empty()`, `.dict_excel()`, `.dict_text()`, `.dict_data()`, `.dict_write()`); the PDF (`.dict_quarto()`, `.dict_run_quarto()`, `.dict_pdf()`); the HTML (`.dict_html()`); the logo (`.dict_logo()`, `.dict_logo_file()`, `.dict_lockup()`, `.dict_logo_src()`, `.png_info()`) |
| `R/episerver_labels.R` | `.lbl_read()`, which reads a table's labels for the dictionary and for `.lbl_get()` (`episerver_getlabels()`), and `.lbl_named()`, the label table that the `lbl_*` arguments name, with its layout |
| `R/episerver_browse.R` | The Download menu in the Columns tab's panel and its handlers (`dictionary()`, `dict_download()` in the server); Dictionaries of all tables: its dialogs (`.browse_bulk_dialog()`, `.browse_bulk_done()`), the folder list (`.browse_folder_at()`, `.browse_subfolders()`, `.browse_folder_ui()`, and the script that sends a click as `input$bulk_into`), Explorer (`.browse_open_fn()`, `.browse_open_dir()`) and its handlers (`bulk_dir`, `output$bulk_folders`, `input$bulk_write` in the server) |
| `inst/dictionary/dictionary.typ` | The PDF's Typst template |
| `inst/dictionary/dictionary.css`, `dictionary.js` | The HTML file's style and script, written into each file |
| `tests/testthat/test-episerver_dictionary.R` | The dictionary from the fake, as data, its text, the HTML file, the PDF with Quarto mocked, finding Quarto, `episerver_dictionary()` with the connection mocked, for tables and for a database, `dictionary_logo()`, and the Download menu and Dictionaries of all tables through `testServer()`. One test compiles PDFs where Quarto is found. |
| `tests/testthat/test-episerver_dictionary_live.R` | The labels of 11 tables of `Analysis.dbo` against `episerver_getlabels()`'s, the tables of `Analysis.dbo` with whether they have labels against `.lbl_read()`, and `episerver_dictionary()` writing a table's files |
| `dev/harness/fake_quarto/quarto` | A stand-in for Quarto in cloud sessions (see `dev/harness/README.md`) |

## The linkage dictionary, set aside (26 Oct 2026)

The first design also covered the linkage dictionary and the variable list workbook that linkage applicants fill in.
- The linkage dictionary is a subset of a table's columns, with aliases, themes and caveats.
- Their rules were to move from the maintainer's Excel workbook into dictionary tables in `Analysis.ref`.

Pull request #16 built seven such tables, a worked example and a conversion of the workbook. The maintainer closed it unmerged, for three reasons:
- seven tables to define one kind of dictionary is over-engineered;
- keeping the rules up to date across them in SSMS would not work in practice;
- the conversion and descriptions of the workbook do not belong in a public repository.

The linkage dictionary and variable list stay in the maintainer's local process, and may come back in another form. Do not reintroduce dictionary tables, profiles or a conversion of the workbook without the maintainer's agreement. None of the local process's files, rules or templates go in the repository.

## Decisions

- **Content.** A dictionary is built from what the label tables already hold, so it needs no other tables and no upkeep beyond the labels:
  - the table's dataset label, dataset description, dataset notes and linked tables: the description a point for each paragraph, and the notes under the headings of their themes, grouped by `.lbl_note_groups()`, as the Dataset Browser's Collection tab shows them (the Dataset Browser's plan pull request 11, at the maintainer's request, 28 Oct 2026);
  - its columns, in the table's order, with their variable labels, types, value labels and notes.
  - `.dict_build()` reads them with the browser's functions: `.browse_sql_tables()` (the table's type, and its name in its own case), `.browse_sql_columns()` and `.lbl_read()`. `.dict_assemble()` puts them together, matching columns to labels in lower case, with `.browse_type()`'s types, so a file matches the Columns tab.
  - The browser's Download calls `.dict_assemble()` with what it has read for the table chosen and the label table chosen, and reads the value labels for it when they are not shown.
- **`create_dictionary()` is removed** (the maintainer's choice, 26 Oct 2026). It flattened the labels of a labelled data frame already in R; nothing in the package used it, and the maintainer knew of no one who did. `episerver_dictionary()` returns the dictionary as a data frame in its place, one row per value label, and one for a column without value labels: `table`, `position`, `column`, `description`, `type`, `code`, `value_label`, `notes`. sjlabelled, which only `create_dictionary()` used, left Imports; the Quarto analysis template loads it itself with `pacman::p_load()`.
- **Text.**
  - The band reads `title` and `subtitle`, by default "Data dictionary" and "Epidemiology Section, ACT Health and Community Services Directorate". The browser uses the defaults.
  - The heading is the table's dataset label, or the table's name when it has none, as `ref.DataProperties` is not yet filled for every table (the maintainer's choice). Beneath it: "Analysis.dbo.APC: a table of 43 columns", then where the labels came from, as the info bar says it.
  - The footer gives the table's full name, the label table and the date; the PDF's also gives the page, as "page 1 of 2".
  - Dataset information has the sub-headings Dataset description and Dataset notes, with the themes' headings under the notes and Linked tables when there are any. A table without a description or notes says "No dataset description available for this collection." or "No dataset notes available for this collection.", as the Collection tab does; it said "This table has no notes or linked tables." before plan pull request 11.
- **A whole database** (the maintainer asked for it on 26 Oct 2026). `episerver_dictionary(db = "Analysis")`, or with `schema`, writes the dictionary of every table and view that a label table holds labels for, and leaves out the rest, with a message counting them.
  - `.dict_tables()` lists the schemas and their tables and views with `.browse_all_tables()` (through `.browse_sql_schemas()` and `.browse_sql_tables()`, which leaves out label tables; the browser's search lists them the same way), and asks `.lbl_sql_pick()` of each, against the database's label tables or the one the `lbl_*` arguments name: one small query a table, and no new SQL.
  - Without `schema`, each schema's files go in a folder of its name, as tables of one name can be in several schemas (`dbo.APC` and `stage.APC`). With `schema`, or with tables named, the files go in `dir` itself.
  - `.dict_write_all()` builds and writes each table's files in turn. For a database it keeps going when a table fails, and `episerver_dictionary()` then warns, naming each table with its error; for tables named, the first error stops it.
  - The browser does the same with Dictionaries of all tables (see Browser).
- **Value labels.** The PDF shows the first `max_values` value labels of a column, 50 by default (the maintainer's choice), then "and 1,234 more value labels; see the HTML dictionary". The HTML file shows them all, folded.
- **Outputs.**
  - PDF and standalone HTML, from the one dictionary. Word is not made: in the old process it was only a step towards the PDF, and it was slow with large tables.
  - Pages are A4 landscape, in the brand colours of `dev/reference/act-brand.md`: a Nightshade band with the title and the logo, then the dataset information and the columns, with Nightshade table headings and rows striped in the info bar's tint.
  - Files are named `<table>_dictionary.pdf` and `.html`. `episerver_dictionary()` stops before connecting when two tables of a batch have the same name, as their files would.
- **PDF.** Typst, compiled with the copy of Quarto that comes with RStudio: `quarto typst compile in.typ out.pdf`.
  - On the maintainer's SOE, `quarto --version` gives 1.8.25 and `quarto typst --version` gives typst 0.13.0 (8dce676d). `Sys.which("quarto")` finds `C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe`, and `RSTUDIO_PANDOC` is `C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools`.
  - `.dict_quarto()` finds Quarto with `Sys.which("quarto")`, falling back to `quarto.exe` beside `RSTUDIO_PANDOC`'s folder. Without it, `episerver_dictionary()` stops before connecting when the PDF is asked for, and the browser's PDF item is disabled.
  - The template, `inst/dictionary/dictionary.typ`, is fixed. `.dict_pdf()` copies it to a temporary folder with the logo and the dictionary as `dictionary.json` (`.dict_data()`, written by jsonlite), and the template reads the JSON. Text from the label tables thus arrives as strings, and is never read as Typst markup, so nothing needs escaping.
  - The columns start on a new page (`#pagebreak(weak: true)` before Columns), so that the table starts at the top of a page (the maintainer's request, 28 Oct 2026). `weak` adds no blank page should the information end at a page's foot.
  - Written for Typst 0.13, and checked in a cloud session with pip's `typst` package at 0.13.7. Fonts follow the brand (Montserrat for headings, Source Sans 3 for text) where installed, then Calibri and Arial, as on the SOE.
  - Rejected:
    - printing the HTML from a browser: not one click, not scriptable;
    - pagedown or chromote with headless Edge: more packages, Edge's debugging port is often disabled by policy, and 127.0.0.1 requests meet the proxy, as noted in `.run_app()`;
    - LaTeX: analysts do not have it.
- **HTML.** One file that needs nothing beyond itself, so it can be sent on.
  - The page is written by htmltools, which escapes the text. htmltools moves the content of `tags$head()` out for Shiny to place, so `.dict_html()` writes the head itself.
  - The CSS and script in `inst/dictionary/` are written into it, the logo as a data URI, and the dictionary as JSON, with every `<` written as `\u003c` so that no label can end the script element. No font is downloaded.
  - Two tabs, under the band rather than in it, as the maintainer asked (28 Oct 2026): Columns, shown first, and Dataset information. They replaced the Contents line of links. The script makes them (`.tabbed` on the body): without it, and in print, both sections show, Dataset information first, each with its own heading, which the tabs hide on screen.
  - The search reads the JSON. It shows the columns whose name, label, type, notes or value labels hold the text, and opens the value labels that do. Expand and Collapse open and close every list; printing opens them all and hides the search.
  - The search bar and buttons (`.tools`) stay at the top of the window as the columns scroll, with the table's headings stuck below them (the maintainer's stretch goal, 28 Oct 2026). The headings' place is `--tools-height`, which the script sets from the bar's height and again when the window is resized, as the bar wraps in a narrow window.
  - The columns' widths are fixed, in percent, by a `colgroup` and `table-layout: fixed`: #, Column, Description, Type, Value labels and Notes take 4, 15, 24, 9, 28 and 20, or 4, 17, 33, 10 and 36 without notes. Long text wraps within its column. At first the browser sized the columns to their text, and long notes took over half the width and squeezed the value labels (the maintainer, 26 Oct 2026).
  - The CSS names the table's own cells with child selectors (`table.columns > tbody > tr > td`), so the rules under its rows are not drawn in the table of value labels inside a cell.
- **Logo.**
  - The band shows the actepir lockup (`inst/www/logo-lockup.svg`) by default (called "the actepir logo" wherever analysts read it, since the Dataset Browser's plan pull request 9), so that dictionaries sent on carry the package's mark (the maintainer's choice). The repository and package hold no government logo.
  - `dictionary_logo(path)` keeps a PNG in actepir's folder in R's user settings directory (`tools::R_user_dir("actepir", "config")`), so it stays set in later sessions; `dictionary_logo()` says which logo is set, and `dictionary_logo(remove = TRUE)` goes back to the lockup. One function with a stored setting, rather than a `logo` argument, so that the browser's Download uses it too.
  - `.png_info()` reads the PNG's size and colour type from its header, and a tRNS chunk before its image data, with no package. `dictionary_logo()` warns when the logo is under 600 pixels wide, has no transparency, or is over 500 KB, as every HTML file carries it.
  - Tests set `R_USER_CONFIG_DIR` to a temporary folder.
- **Browser.** A Download menu under the code button, in the Columns tab's own panel since the Dataset Browser's plan pull request 7 (it was in the sidebar's foot, opening upwards), opening downwards: Dictionary as PDF, Dictionary as HTML, Copy dictionary for Excel, then Dictionaries of all tables and Dictionary logo (the last two items since plan pull request 8; see `dataset-browser.md`). Without Quarto, the PDF's item is a disabled entry, "Dictionary as PDF (needs Quarto)", not a link: Shiny enables a download link once it has its address, which would undo a disabled class. A failure shows a notification.
- **Dictionaries of all tables** (the maintainer asked for it on 26 Oct 2026, with PDFs required). The browser writes the files to a folder on the analyst's computer, where the app runs, rather than giving a download.
  - A dialog asks for the tables and views (of the schema chosen, or of the whole database, each schema in a folder of its name), the files (HTML, PDF or both; PDF disabled without Quarto) and the folder. The folder starts as `dictionaries` in R's working directory and is remembered for the session. Write makes the folder if it does not exist.
  - The folder is typed or pasted in the Folder box, or found in the folder list below it (`.browse_folder_ui()`): Up, Home (`~`, which R for Windows takes as Documents) and Working directory, then the folders in the folder in the box, or in the nearest folder above it that exists, without hidden and system folders. A click on any of them puts its path in the box. The list follows the box once typing pauses (a 300 ms debounce), as a network folder can be slow to list. A note under the buttons says when the folder does not exist yet and will be made.
  - Why the folder list and not a dialog. The Dictionary logo dialog's file chooser (plan pull request 8) is the browser's own (`<input type="file">`): Edge draws it, and hands the app the file's content, never its path, which is all a logo needs. A folder to write into needs a path the R process can write to, and a browser never tells a page where a file or folder is. `utils::choose.dir()` is R's dialog, drawn by the background R process, which did not show it on the SOE. Edge's folder picker (`showDirectoryPicker()`) gives a page leave to write into a folder without its path: the browser would have to write every file, sent to it from R, and ask leave again in each session, as the app's address changes at each launch, and Open the folder and the remembered folder would go. This was put to the maintainer on 27 Oct 2026; the folder list stays unless they choose otherwise.
  - Windows' folder dialog was used first, through a Choose… button calling `utils::choose.dir()`. On the maintainer's SOE (26 Oct 2026) it did nothing: no dialog opened and the app kept working, so the call returned at once in the browser's background process. The maintainer chose the folder list in its place, which needs no new package and works the same on every system.
  - On Windows, the closing dialog has Open the folder (`shell.exec()`), which works from the background process. `shell.exec()` exists only in R for Windows, so it is looked up with `get0()` and the button is left out elsewhere, so that R CMD check passes on Linux.
  - Writing reads the label table chosen in the sidebar, or the automatic choice, and shows a progress bar counting the tables. It keeps going when a table fails. The closing dialog says how many dictionaries were written, as which files and where, how many tables and views were left out without labels (`.dict_summary()`, as `episerver_dictionary()` says it), and lists any that failed with their errors.
  - Rejected:
    - a ZIP download: PDFs for a schema take minutes, too long for a download, so it could give HTML only, and `utils::zip()` needs a zip program, which Windows has only with Rtools, so it would add the zip package to Imports;
    - scripts only: the maintainer wanted it in the browser.

## Open items

- **Dictionaries of all tables on the SOE (waiting on the maintainer's test).** The maintainer's first test (26 Oct 2026) found Open the folder and writing under the redirected Documents working, and Choose… not working; the folder list replaced Choose…. Not yet seen on Windows:
  - the folder list: Home as the redirected Documents, Up from a UNC path, and how fast a network folder lists;
  - how long the PDFs of `Analysis.dbo` take.
- **The logo control and the Excel copy (the Dataset Browser's plan pull request 8, waiting on the maintainer's test).** The browser's Dictionary logo dialog sets and removes the stored logo through `dictionary_logo()`, and Copy dictionary for Excel copies `.dict_flat()`'s rows; `dataset-browser.md` gives the decisions. Whether Excel keeps codes such as 01 as text on paste has not been seen.
- The look of the PDF on the SOE, with Calibri and Arial, has not been seen: the cloud session has none of the brand fonts, Calibri or Arial, so its pages were checked in Typst's own fonts.

## How to check a change

**Offline:**
- The three commands in CLAUDE.md. The test that compiles PDFs skips in a cloud session unless `dev/harness/fake_quarto` is first on the PATH (see `dev/harness/README.md`).
- The Typst template compiled with Typst 0.13 (pip's `typst` package), and its pages rendered as PNG and looked at.
- The HTML file opened in the harness's headless Chromium: `episerver_browser.js` downloads it from the browser and searches it. It also writes Dictionaries of all tables for the fake's `Analysis` and lists the files.

**Against EpiServer:**
- The maintainer runs `testthat::test_local()`, in which `test-episerver_dictionary_live.R` must run rather than skip, and the test that compiles PDFs runs with RStudio's Quarto.
- They write the dictionaries of a few tables with `episerver_dictionary()` (one labelled in `ref.DataProperties`, one in a DataLabels table, one with many value labels, a view) and compare them with the browser's Columns tab, then download the same from the browser.
- In the browser, from Download > Dictionaries of all tables, they find a folder under their redirected Documents in the folder list (Home, then a click on a folder, then Up), write the dictionaries of `Analysis.dbo` to it as HTML and PDF, note how long it takes, open the folder with Open the folder, and write those of the whole database as HTML.
- They set their own logo with `dictionary_logo()` and remove it again, and do the same in the browser's Dictionary logo dialog.
- They paste Copy dictionary for Excel into Excel and check that codes such as 01 are kept as written.
