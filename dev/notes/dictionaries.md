# Data dictionaries

Planned work: a full data dictionary of any table or view on EpiServer, built from its label tables, exported as PDF and standalone HTML, and offered in the Dataset Browser. It is pull request 3 of the plan in `dataset-browser.md`; the browser's improvements follow as pull request 4.

## Status

No code yet. The design was agreed with the maintainer on 25 Oct 2026 and narrowed on 26 Oct 2026, when the linkage dictionary was set aside.

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

- **Content.** A full dictionary is built from what the label tables already hold, through the labels lookup in `R/episerver_labels.R`, so it needs no other tables and no upkeep beyond the labels:
  - the table's dataset label, dataset notes and linked tables;
  - its columns, in the order of the browser's Columns tab, with their variable labels, types, value labels and notes.
  - A table can have thousands of value labels, so a full dictionary needs a limit on value labels per column.
- **Outputs.**
  - PDF and standalone HTML, from one builder that also feeds the browser's Columns tab, so a file matches the screen.
  - Word is not made: in the old process it was only a step towards the PDF, and it was slow with large tables.
  - Pages are A4 landscape.
  - Use the brand colours in `dev/reference/act-brand.md` and keep Nightshade. The layout:
    - a Nightshade band at the top with the title and the logo;
    - the dataset information;
    - the columns;
    - a footer with the table's name and the date.
- **PDF engine.** Typst, compiled with the copy of Quarto that comes with RStudio: `quarto typst compile in.typ out.pdf`.
  - On the maintainer's SOE, `quarto --version` gives 1.8.25 and `quarto typst --version` gives typst 0.13.0 (8dce676d).
  - `Sys.which("quarto")` finds `C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe`, and `RSTUDIO_PANDOC` is `C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools`.
  - Find Quarto with `Sys.which("quarto")`, falling back to the copy beside `RSTUDIO_PANDOC`.
  - Write Typst that works in 0.13. In a cloud session, check it compiles with pip's `typst` package at a 0.13 version, and look at the pages.
  - No new R package is needed.
  - Rejected:
    - printing the HTML from a browser: not one click, not scriptable;
    - pagedown or chromote with headless Edge: more packages, Edge's debugging port is often disabled by policy, and 127.0.0.1 requests meet the proxy, as noted in `.run_app()`;
    - LaTeX: analysts do not have it.
- **HTML.** One file with nothing external: the CSS, a small script and the dictionary as JSON written inline, and the logo as a data URI. It has search, expandable value labels, contents and print styles. No new package.
- **Logo.** The repository and package hold no government logo.
  - `dictionary_logo(path)` copies a PNG into actepir's folder in R's user settings directory (`tools::R_user_dir("actepir", "config")`), so it persists across sessions. `dictionary_logo()` shows it, and `dictionary_logo(remove = TRUE)` removes it. Without a logo, outputs show the title only.
  - The PDF and the HTML use it, scaled to a fixed height in the header band, with width following its shape. The apps keep the actepir lockup.
  - Recommended file:
    - a PNG of the white (reversed) version on a transparent background, as it sits on Nightshade;
    - about 1,400 by 430 pixels;
    - at least 600 pixels wide and under 500 KB, as the HTML file carries it.
  - The function reads the PNG's size and colour type from its header (no package needed), and warns when it is small or has no transparency.
  - Tests set `R_USER_CONFIG_DIR` to a temporary folder.
- **Scripts.** `episerver_dictionary()` (the name is to be confirmed with the maintainer) builds the dictionary of one table or a batch, and writes the PDF and HTML. It passes connection arguments through `...`.
- **Browser.** A Download menu offers the PDF and HTML of the table chosen.

## Open items

- Confirm the name of `episerver_dictionary()` and the arguments of `dictionary_logo()` with the maintainer.
- The title lines of a full dictionary, and the limit on value labels per column, to settle with the maintainer in pull request 3.
- `create_dictionary()` already flattens the labels of a labelled data frame collected into R. Pull request 3 must settle how the builder, which reads the label tables, relates to it, as CLAUDE.md allows one mechanism per job.
- Two improvements of pull request 4 touch the dictionary: label coverage, and copying the dictionary as a table for Excel. Build the dictionary so that they can use it.

## How to check a change

**Offline:** the three commands in CLAUDE.md; the generated Typst compiled with Typst 0.13 and its pages looked at; the HTML file opened in the harness's headless Chromium.

**Against EpiServer:** the maintainer builds the dictionaries of a few tables (one labelled in `ref.DataProperties`, one in a DataLabels table, one with many value labels) and compares them with the browser's Columns tab.
