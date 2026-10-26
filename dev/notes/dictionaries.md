# Data dictionaries

Planned work: pull requests 3, 4 and 5 of the plan in `dataset-browser.md`. The aim is to bring every kind of data dictionary into the Dataset Browser and actepir:

- a full dictionary of any table;
- the linkage dictionary, a subset of a table's columns with aliases, offered for data linkage;
- the linkage variable list workbook that applicants fill in.

Both linkage products are made by the same code from the same rules, so they always agree. Nothing here has been written yet.

## Status

Design agreed with the maintainer on 25 Oct 2026. No code yet.

## The process it replaces

The maintainer's local process is not in the repository. Three of its files must be uploaded to the session again when this work starts:

- `definitions.xlsx`, the rules;
- `dictionary_direct.Rmd` and `dictionary_call.Rmd`, the R Markdown that builds a Word dictionary with flextable and officer, and the variable list with openxlsx from `templates/template_varlist.xlsx`;
- the blank `template_varlist.xlsx`.

`dictionary_direct.Rmd` holds the password used to protect the workbook. It must never be committed, and the new workbook is protected without a password (decided below).

The rules, by sheet of `definitions.xlsx`:

| Sheet | Columns | Meaning |
|---|---|---|
| `vars` | Dataset, Theme, Column, ColumnAlias, DescriptionAlias, ColumnAction, ValueAction, Limits, Comments, Other | One row per column of a dataset. INCLUDE or EXCLUDE for linkage; a row with no ColumnAction counts as included. Theme groups columns into "Category:" sections, with Uncategorised for the rest. Columns sharing a ColumnAlias collapse to one row, and DescriptionAlias replaces the label. For example, `Diagnosis1` is aliased "Primary Diagnosis", and `Diagnosis2` to `Diagnosis100` collapse to `Diagnosis2-100`, "Secondary diagnoses". ValueAction Suppress hides a column's value labels. Limits is a caveat code. Comments and Other are working notes, never printed. |
| `notes` | Dataset, Aspect, Issue, Comment | The dataset information table: Dataset Description, Data Quality Statement, Links & Resources (a comment starting `http` becomes a link) and others. |
| `limits` | code, hex, symbol, text | Caveats: a coloured symbol (for example ◆, ▲ or ◼) beside a column, and its footnote. |

Further rules in the old code:

- **Column order:** columns are sorted by name, with numeric suffixes in number order. Value codes are sorted numerically, then as text.
- **Run script:** `dictionary_call.Rmd` holds, for each dataset, its full name, abbreviation, DOI, version, database, schema, table and label table.
- **Not carried over (agreed):**
  - the optional `arrays` sheet (regular-expression rules), which the workbook does not use;
  - the `use_definitions` and `basis` switches;
  - Word output.

## Decisions

- **Storage.** The tables are in `Analysis.ref`, keyed by profile. "Linkage" is the first profile. They are edited in SSMS; the Excel workbook is retired once converted. Proposed tables:

  | Table | Replaces | Holds |
  |---|---|---|
  | `DictionaryProfiles` | The Word template | Profile, title lines (for Linkage, "Data Linkage Dataset Metadata" and "Data Analytics Branch: Epidemiology Section"), and the closing sections (general enquiries with the contact email and the request portal, Acknowledgement of Country, accessibility), each a heading and text in order. If this proves awkward, the maintainer will accept the text in the package for now. |
  | `DictionaryDatasets` | The table in `dictionary_call.Rmd` | Profile, dataset code (APC), full name, database, schema, table, DOI, version, active. No label table: the labels lookup finds it. |
  | `DictionaryColumns` | `vars` | Profile, dataset code, column, theme, alias, alias description, include or exclude, suppress, caveat code, internal comment. |
  | `DictionaryNotes` | `notes` | Profile, dataset code, order, aspect, issue, comment. |
  | `DictionaryCaveats` | `limits` | Profile, code, symbol, colour, text. |
  | `DictionaryThemes` | (new) | Profile, theme, order. The old order followed the first column name alphabetically. Themes not listed follow alphabetically, then Uncategorised. |

  - Columns with a fixed set of values get CHECK constraints, and the conversion standardises their spelling (INCLUDE, EXCLUDE, Suppress and suppress appear in the workbook).
  - The definitions do not go in `ref.DataProperties`, for four reasons:
    - sqldta reads that table;
    - the definitions cover tables in several databases;
    - one table may have several profiles;
    - the comments are internal.
  - Column labels and value labels still come from the labels lookup (DataProperties first), unless a column has an alias description.
  - Each array member keeps its own row. A column added to a table later (for example `Diagnosis101`) is left out of a profile until it has a row. The label coverage improvement in pull request 6 lists such columns.
  - Pull request 3 delivers the SQL that creates the tables, a worked example of adding rows, and a script that converts `definitions.xlsx` into an INSERT script the maintainer runs in SSMS. Neither the workbook nor the generated script is committed.
  - Two data problems to fix before converting:
    - the five `ESWLClusterArray` columns have aliases `ESWL Cluster 1-5` to `1-9`, which looks like Excel auto-fill;
    - `COD1-10` covers `COD1` to `COD9` only.
- **Outputs.**
  - PDF and standalone HTML, from one builder that also feeds the browser's Columns tab, so a file matches the screen.
  - Word is no longer made: it was only a step towards the PDF, and it was slow with large tables.
  - Pages are A4 landscape; the old Word template was US Letter by mistake.
  - Use the brand colours in `dev/reference/act-brand.md` and keep Nightshade. The Word layout need not be copied exactly: a Nightshade band at the top with the title lines and the logo, the dataset information table, the columns by category, caveat symbols in their colours with footnotes, and a footer with the dataset, release number (version) and date.
  - Full dictionaries need a limit on value labels per column, as the linkage profile's suppression does not apply to them.
- **PDF engine.** Typst, compiled with the copy of Quarto that comes with RStudio: `quarto typst compile in.typ out.pdf`.
  - On the maintainer's SOE, `quarto --version` gives 1.8.25 and `quarto typst --version` gives typst 0.13.0 (8dce676d). `Sys.which("quarto")` finds `C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe`, and `RSTUDIO_PANDOC` is `C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools`.
  - Find Quarto with `Sys.which("quarto")`, falling back to the copy beside `RSTUDIO_PANDOC`.
  - Write Typst that works in 0.13. In a cloud session, check it compiles with pip's `typst` package at a 0.13 version and look at the pages.
  - No new R package is needed.
  - Rejected:
    - printing the HTML from a browser: not one click, not scriptable;
    - pagedown or chromote with headless Edge: more packages, Edge's debugging port is often disabled by policy, and 127.0.0.1 requests meet the proxy, as noted in `.run_app()`;
    - LaTeX: analysts do not have it.
- **HTML.** One file with nothing external: the CSS, a small script and the dictionary as JSON written inline, and the logo as a data URI. It has search, expandable value labels, contents and print styles. No new package.
- **Logo.** The repository and package hold no government logo.
  - `dictionary_logo(path)` copies a PNG into actepir's folder in R's user settings directory (`tools::R_user_dir("actepir", "config")`), so it persists across sessions. `dictionary_logo()` shows it, and `dictionary_logo(remove = TRUE)` removes it. Without a logo, outputs show the title only.
  - The PDF, the HTML and the variable list use it, scaled to a fixed height in each header band, with width following its shape. The apps keep the actepir lockup.
  - Recommended file:
    - PNG, because Excel cannot place SVG;
    - the white (reversed) version on a transparent background, as it sits on Nightshade;
    - about 1,400 by 430 pixels, the size of the logo in the old Word file;
    - at least 600 pixels wide and under 500 KB, as the HTML file carries it.
  - The function reads the PNG's size and colour type from its header (no package needed) and warns when it is small or has no transparency.
  - Tests set `R_USER_CONFIG_DIR` to a temporary folder.
- **Scripts.** `episerver_dictionary()` (the name is to be confirmed with the maintainer) builds a full or profile dictionary for one dataset or a batch, and writes the PDF, HTML and variable list. It replaces `dictionary_call.Rmd`. It passes connection arguments through `...`.
- **Variable list.**
  - Built in actepir with openxlsx, which goes in Imports, from `template_varlist.xlsx` in `inst/`. Remove the two copies of the white ACT Health and Community Services logo from the packaged template, and insert the configured logo in their places.
  - The template's named ranges are:
    - `VARNAMES_DUMP` (`Variables!B16`: column, description, datatype, caveat);
    - `LIMITS_DUMP` (`Variables!E16`: conditional formats colouring the symbols);
    - `ALIAS_DUMP` (`Aliases!A1`: alias and column, one row per column);
    - `CP_VERSION`, `CP_UPDATEDATE`, `CP_DATASETABB` and `CP_DATASETFULL`.
  - The old code then deleted unused rows below the variables, hid unused columns, hid the Aliases, Meta and CP sheets, and protected the sheets and the workbook.
  - Protect without a password (the maintainer's decision).
  - `Guide!B29` builds the SQL column list of the variables requested, expanding aliases and applying the date formats in the CP sheet, so the workbook is also the specification of the extract.
  - The CP sheet still holds the old blue palette; consider the brand's purples.

## Open items

- Confirm the name of `episerver_dictionary()` and the arguments of `dictionary_logo()` with the maintainer.
- Whether full dictionaries in the browser should group by theme when a profile is not chosen (the themes belong to a profile).
- The DOI is a placeholder in the run script; show it on the PDF's first page if it is set.

## How to check a change

**Offline:** the three commands in CLAUDE.md; the generated Typst compiled with Typst 0.13 and its pages looked at; the HTML file opened in the harness's headless Chromium; the workbook opened with openxlsx and its named ranges checked.

**Against EpiServer:** the maintainer creates and loads the tables, then builds the linkage dictionary and variable list of a dataset, such as APC, and compares them with the last ones made by the old process.
