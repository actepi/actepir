# Data dictionaries

The full data dictionary of any table or view on EpiServer, built from its label tables. `episerver_dictionary()` writes it as a PDF and a standalone HTML file and returns it as a data frame; the Dataset Browser's Download menu gives the same files for the table chosen. It is pull request 3 of the plan in `dataset-browser.md`; the browser's improvements follow as pull request 4.

## Status

| Pull request | Content |
|---|---|
| #18, plan pull request 3 (open, waiting on the maintainer's test against EpiServer) | `episerver_dictionary()`, `dictionary_logo()`, the PDF and HTML files and the browser's Download menu. `create_dictionary()` removed. |

The design was agreed with the maintainer on 25 Oct 2026, narrowed on 26 Oct 2026 when the linkage dictionary was set aside, and settled the same day (see Decisions).

## Where the code is

| File | Holds |
|---|---|
| `R/episerver_dictionary.R` | `episerver_dictionary()` and `dictionary_logo()`; the dictionary (`.dict_build()`, `.dict_assemble()`, `.dict_flat()`, `.dict_text()`, `.dict_data()`, `.dict_write()`); the PDF (`.dict_quarto()`, `.dict_run_quarto()`, `.dict_pdf()`); the HTML (`.dict_html()`); the logo (`.dict_logo()`, `.dict_logo_file()`, `.dict_lockup()`, `.png_info()`) |
| `R/episerver_labels.R` | `.lbl_read()`, which reads a table's labels for the dictionary and for `.lbl_get()` (`episerver_getlabels()`) |
| `R/episerver_browse.R` | The Download menu in the sidebar's foot and its handlers (`dictionary()`, `dict_download()` in the server) |
| `inst/dictionary/dictionary.typ` | The PDF's Typst template |
| `inst/dictionary/dictionary.css`, `dictionary.js` | The HTML file's style and script, written into each file |
| `tests/testthat/test-episerver_dictionary.R` | The dictionary from the fake, as data, its text, the HTML file, the PDF with Quarto mocked, finding Quarto, `episerver_dictionary()` with the connection mocked, `dictionary_logo()`, and the Download menu through `testServer()`. One test compiles PDFs where Quarto is found. |
| `tests/testthat/test-episerver_dictionary_live.R` | The labels of 11 tables of `Analysis.dbo` against `episerver_getlabels()`'s, and `episerver_dictionary()` writing a table's files |
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
  - the table's dataset label, dataset notes and linked tables;
  - its columns, in the table's order, with their variable labels, types, value labels and notes.
  - `.dict_build()` reads them with the browser's functions: `.browse_sql_tables()` (the table's type, and its name in its own case), `.browse_sql_columns()` and `.lbl_read()`. `.dict_assemble()` puts them together, matching columns to labels in lower case, with `.browse_type()`'s types, so a file matches the Columns tab.
  - The browser's Download calls `.dict_assemble()` with what it has read for the table chosen and the label table chosen, and reads the value labels for it when they are not shown.
- **`create_dictionary()` is removed** (the maintainer's choice, 26 Oct 2026). It flattened the labels of a labelled data frame already in R; nothing in the package used it, and the maintainer knew of no one who did. `episerver_dictionary()` returns the dictionary as a data frame in its place, one row per value label, and one for a column without value labels: `table`, `position`, `column`, `description`, `type`, `code`, `value_label`, `notes`. sjlabelled, which only `create_dictionary()` used, left Imports; the Quarto analysis template loads it itself with `pacman::p_load()`.
- **Text.**
  - The band reads `title` and `subtitle`, by default "Data dictionary" and "Epidemiology Section, ACT Health and Community Services Directorate". The browser uses the defaults.
  - The heading is the table's dataset label, or the table's name when it has none, as `ref.DataProperties` is not yet filled for every table (the maintainer's choice). Beneath it: "Analysis.dbo.APC: a table of 43 columns", then where the labels came from, as the info bar says it.
  - The footer gives the table's full name, the label table and the date; the PDF's also gives the page, as "page 1 of 2".
  - A table without notes or linked tables says so under Dataset information.
- **Value labels.** The PDF shows the first `max_values` value labels of a column, 50 by default (the maintainer's choice), then "and 1,234 more value labels; see the HTML dictionary". The HTML file shows them all, folded.
- **Outputs.**
  - PDF and standalone HTML, from the one dictionary. Word is not made: in the old process it was only a step towards the PDF, and it was slow with large tables.
  - Pages are A4 landscape, in the brand colours of `dev/reference/act-brand.md`: a Nightshade band with the title and the logo, then the dataset information and the columns, with Nightshade table headings and rows striped in the info bar's tint.
  - Files are named `<table>_dictionary.pdf` and `.html`. `episerver_dictionary()` stops before connecting when two tables of a batch have the same name, as their files would.
- **PDF.** Typst, compiled with the copy of Quarto that comes with RStudio: `quarto typst compile in.typ out.pdf`.
  - On the maintainer's SOE, `quarto --version` gives 1.8.25 and `quarto typst --version` gives typst 0.13.0 (8dce676d). `Sys.which("quarto")` finds `C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe`, and `RSTUDIO_PANDOC` is `C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools`.
  - `.dict_quarto()` finds Quarto with `Sys.which("quarto")`, falling back to `quarto.exe` beside `RSTUDIO_PANDOC`'s folder. Without it, `episerver_dictionary()` stops before connecting when the PDF is asked for, and the browser's PDF item is disabled.
  - The template, `inst/dictionary/dictionary.typ`, is fixed. `.dict_pdf()` copies it to a temporary folder with the logo and the dictionary as `dictionary.json` (`.dict_data()`, written by jsonlite), and the template reads the JSON. Text from the label tables thus arrives as strings, and is never read as Typst markup, so nothing needs escaping.
  - Written for Typst 0.13, and checked in a cloud session with pip's `typst` package at 0.13.7. Fonts follow the brand (Montserrat for headings, Source Sans 3 for text) where installed, then Calibri and Arial, as on the SOE.
  - Rejected:
    - printing the HTML from a browser: not one click, not scriptable;
    - pagedown or chromote with headless Edge: more packages, Edge's debugging port is often disabled by policy, and 127.0.0.1 requests meet the proxy, as noted in `.run_app()`;
    - LaTeX: analysts do not have it.
- **HTML.** One file that needs nothing beyond itself, so it can be sent on.
  - The page is written by htmltools, which escapes the text. htmltools moves the content of `tags$head()` out for Shiny to place, so `.dict_html()` writes the head itself.
  - The CSS and script in `inst/dictionary/` are written into it, the logo as a data URI, and the dictionary as JSON, with every `<` written as `\u003c` so that no label can end the script element. No font is downloaded.
  - The search reads the JSON. It shows the columns whose name, label, type, notes or value labels hold the text, and opens the value labels that do. Expand and Collapse open and close every list; printing opens them all and hides the search.
- **Logo.**
  - The band shows the actepir lockup (`inst/www/logo-lockup.svg`) by default, so that dictionaries sent on carry the package's mark (the maintainer's choice). The repository and package hold no government logo.
  - `dictionary_logo(path)` keeps a PNG in actepir's folder in R's user settings directory (`tools::R_user_dir("actepir", "config")`), so it stays set in later sessions; `dictionary_logo()` says which logo is set, and `dictionary_logo(remove = TRUE)` goes back to the lockup. One function with a stored setting, rather than a `logo` argument, so that the browser's Download uses it too.
  - `.png_info()` reads the PNG's size and colour type from its header, and a tRNS chunk before its image data, with no package. `dictionary_logo()` warns when the logo is under 600 pixels wide, has no transparency, or is over 500 KB, as every HTML file carries it.
  - Tests set `R_USER_CONFIG_DIR` to a temporary folder.
- **Browser.** A Download menu in the sidebar's foot, between the code button and the bug button, opening upwards: Dictionary as PDF and Dictionary as HTML. Without Quarto, the PDF's item is a disabled entry, "Dictionary as PDF (needs Quarto)", not a link: Shiny enables a download link once it has its address, which would undo a disabled class. A failure shows a notification.

## Open items

- Two improvements of pull request 4 can use the dictionary as data (`.dict_flat()`): label coverage, and copying the dictionary as a table for Excel.
- The look of the PDF on the SOE, with Calibri and Arial, has not been seen: the cloud session has none of the brand fonts, Calibri or Arial, so its pages were checked in Typst's own fonts.

## How to check a change

**Offline:**
- The three commands in CLAUDE.md. The test that compiles PDFs skips in a cloud session unless `dev/harness/fake_quarto` is first on the PATH (see `dev/harness/README.md`).
- The Typst template compiled with Typst 0.13 (pip's `typst` package), and its pages rendered as PNG and looked at.
- The HTML file opened in the harness's headless Chromium: `episerver_browser.js` downloads it from the browser and searches it.

**Against EpiServer:**
- The maintainer runs `testthat::test_local()`, in which `test-episerver_dictionary_live.R` must run rather than skip, and the test that compiles PDFs runs with RStudio's Quarto.
- They write the dictionaries of a few tables with `episerver_dictionary()` (one labelled in `ref.DataProperties`, one in a DataLabels table, one with many value labels, a view) and compare them with the browser's Columns tab, then download the same from the browser.
- They set their own logo with `dictionary_logo()` and remove it again.
