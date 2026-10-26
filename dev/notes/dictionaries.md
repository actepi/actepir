# Data dictionaries

Planned work: pull requests 3, 4 and 5 of the plan in `dataset-browser.md`. The aim is to bring every kind of data dictionary into the Dataset Browser and actepir:

- a full dictionary of any table;
- the linkage dictionary, a subset of a table's columns with aliases, offered for data linkage;
- the linkage variable list workbook that applicants fill in.

Both linkage products are made by the same code from the same rules, so they always agree. Pull request 3 added the tables that hold the rules and the conversion of the old workbook into them; the builder and its outputs are pull requests 4 and 5.

## Status

| Pull request | Content |
|---|---|
| #16, plan pull request 3 (open, waiting on the maintainer's run in SSMS) | The seven dictionary tables in `Analysis.ref`, a worked example of adding rows, and a script that converts `definitions.xlsx` into an INSERT script for the Linkage profile. |

The design was agreed with the maintainer on 25 Oct 2026, and the tables' details on 26 Oct 2026, when the maintainer uploaded the files below.

## Where the code is

| File | Holds |
|---|---|
| `inst/sql/dictionary_tables.sql` | The SQL that creates the seven tables, run in SSMS. It creates only the tables that do not exist. Installed with the package, as the builder will read these tables. |
| `inst/sql/dictionary_example.sql` | A worked example of adding, changing and removing rows, for a profile named Example. It ends with `ROLLBACK`, so it changes nothing as it stands. |
| `dev/dictionaries/convert_definitions.R` | The conversion: `convert_definitions()` reads the workbook (`read_definitions()`), builds and checks the rows (`definition_rows()`, `alias_problems()`), and writes the INSERT script (`sql_script()`, `sql_insert()`, `sql_value()`). The Linkage profile's title lines and closing sections, and the proposed order of themes, are at its top. It is a script in `dev/`, not package code (the maintainer's choice), as it is run once. |
| `dev/dictionaries/test-convert_definitions.R` | Its tests, on a made-up workbook. `testthat::test_local()` does not run them; run `Rscript -e 'testthat::test_file("dev/dictionaries/test-convert_definitions.R")'`. |
| `dev/dictionaries/check_insert_script.py` | Checks a generated script without SQL Server: T-SQL syntax with sqlglot, a run in SQLite against the tables' constraints, and every value read back against R's rows. |

## The process it replaces

The maintainer's local process is not in the repository. Its files were uploaded to the session for pull request 3 and are not committed. Upload them again for later work: `template_varlist.xlsx` is needed for pull request 5.

- `definitions.xlsx`, the rules;
- `dictionary_direct.Rmd` and `dictionary_call.Rmd`, the R Markdown that builds a Word dictionary with flextable and officer, and the variable list with openxlsx from `templates/template_varlist.xlsx`;
- the blank `template_varlist.xlsx`.

`dictionary_direct.Rmd` held the password used to protect the workbook; the copy uploaded for pull request 3 had it removed. None of these files may be committed: they hold internal comments, the server's name and database names. The new workbook is protected without a password (decided below).

What pull request 3 found in them:

- The workbook's `Dataset` is the table's name, as `dat.tb` in the run script (`RBDMBirths`), not the dataset's abbreviation (`RBDM-B`). The old code filtered the rules by `Dataset == dat.tb`.
- The run script's `tables` has, for each dataset, `active`, `fullname`, `abbrname`, `doi`, `version`, `db`, `dat.sc`, `dat.tb`, `lbl.sc` and `lbl.tb`. Every DOI is the placeholder `10.9999/exam/ple999`.
- The Word template (`template_dictionary_blank.docx`, US Letter landscape) holds the title lines in its header and, after the columns, a page headed Further Information with three sections: General Enquiries (the section's names, the data custodian's email and the linkage request portal), Acknowledgment of Country and Accessibility.

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

- **Storage.** The tables are in `Analysis.ref`, keyed by profile. "Linkage" is the first profile. They are edited in SSMS; the Excel workbook is retired once converted. The tables, as built in pull request 3 (`inst/sql/dictionary_tables.sql` gives their columns and constraints):

  | Table | Replaces | Holds |
  |---|---|---|
  | `DictionaryProfiles` | The Word template's header | Profile, Title and Subtitle (for Linkage, "Data Linkage Dataset Metadata" and "Data Analytics Branch: Epidemiology Section"). |
  | `DictionarySections` | The Word template's closing page | Profile, SectionOrder, Heading and SectionText: General Enquiries, Acknowledgment of Country and Accessibility. The page's heading, Further Information, is for the builder to write. |
  | `DictionaryDatasets` | The table in `dictionary_call.Rmd` | Profile, DatasetCode (the abbreviation, APC), FullName, DatabaseName, SchemaName, TableName, DOI, Version, Active. No label table: the labels lookup finds it. |
  | `DictionaryColumns` | `vars` | Profile, DatasetCode, ColumnName, Theme, ColumnAlias, DescriptionAlias, ColumnAction, ValueAction, CaveatCode, and the internal Comments and Other. |
  | `DictionaryNotes` | `notes` | Profile, DatasetCode, NoteOrder, Aspect, Issue, Comment. |
  | `DictionaryCaveats` | `limits` | Profile, CaveatCode, Symbol, Colour, CaveatText. |
  | `DictionaryThemes` | (new) | Profile, Theme, ThemeOrder. The old order followed the first column name alphabetically. Themes not listed follow alphabetically, then Uncategorised. |

  - The profile's text was first planned as part of `DictionaryProfiles`. It is two tables, because the number of closing sections varies (the maintainer's choice, 26 Oct 2026). Putting the text in the package was the fallback, not needed.
  - Columns keep the workbook's names where they are clear. `Dataset` became `DatasetCode`, as the workbook's `Dataset` is a table name; `Limits` became `CaveatCode`, after the table.
  - Both of the workbook's internal columns are kept: `Comments` (23 values, such as questions between reviewers) and `Other` (412 values, mostly other wordings of a column's label). The plan named one internal comment; keeping both loses nothing. Neither is shown.
  - Columns with a fixed set of values get CHECK constraints, and the conversion standardises their spelling. `ColumnAction` is INCLUDE or EXCLUDE, and NOT NULL with INCLUDE as its default, as a row without one counts as included. `ValueAction` is SUPPRESS or NULL (the workbook has Suppress in 43 rows and suppress in 29). Caveat codes are written in lower case and colours in upper case.
  - Primary keys on the profile and natural key of each table, and foreign keys from each table to its profile, from columns and notes to their dataset, and from columns to their caveat. The foreign keys do not cascade: cascading updates from both datasets and caveats to columns would be two cascade paths, which SQL Server refuses, and codes are rarely renamed. A dataset is removed after its columns and notes.
  - Text columns are `nvarchar`, as the caveat symbols and some notes are not in the database's code page.
  - The definitions do not go in `ref.DataProperties`, for four reasons:
    - sqldta reads that table;
    - the definitions cover tables in several databases;
    - one table may have several profiles;
    - the comments are internal.
  - Column labels and value labels still come from the labels lookup (DataProperties first), unless a column has an alias description.
  - Each array member keeps its own row. A column added to a table later (for example `Diagnosis101`) is left out of a profile until it has a row. The label coverage improvement in pull request 6 lists such columns.
  - Pull request 3 delivers the SQL that creates the tables, a worked example of adding rows, and a script that converts `definitions.xlsx` into an INSERT script the maintainer runs in SSMS. Neither the workbook nor the generated script is committed.
  - Two data problems were fixed by the maintainer in the workbook before converting (26 Oct 2026): the `ESWLClusterArray` columns' aliases, `ESWL Cluster 1-5` to `1-9` (Excel auto-fill), are now `ESWLCluster1-5`; `COD1-10`, over `COD1` to `COD9`, is now `COD1-9`. The conversion checks every alias that ends in a range against the numbers of its columns.
- **The conversion** (`dev/dictionaries/convert_definitions.R`).
  - The datasets come from the run script's `tables`, which the maintainer builds by running that chunk of `dictionary_call.Rmd`; the workbook's rows are matched to them by `dat.tb`. Committing the datasets' details would have put database names in the public repository.
  - It stops, writing nothing, on a problem that would give wrong rows or make an INSERT fail: an action or caveat code it does not know, a column twice (in any case), an alias whose columns differ in what is shown or do not match its range, a value too long for its column, or a dataset or caveat without what it needs. Each problem names its sheet and rows.
  - It reports, and leaves out, the rows of a `Dataset` that is not a `dat.tb` of `tables`, and reports a dataset without rows, the spellings it standardised, and themes not in the order.
  - The placeholder DOI is written as NULL.
  - Themes are ordered by `linkage_theme_order` at the top of the script, a proposal (see Open items), with steps of 10.
  - The script it writes is plain ASCII: line breaks and other characters are written as `NCHAR()` joined to `N'...'` pieces. Depending on its settings, SSMS can read a file without a byte order mark in the Windows code page, which would garble the caveat symbols and the en dashes in notes, and a file saved again with Windows line endings would change the line breaks in the text.
  - It adds every row in one transaction with `XACT_ABORT`, so a failed row changes nothing. It stops if the profile is loaded already, and lists the DELETE statements for loading it again. It ends by counting the rows loaded against those expected.
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

- **Waiting on the maintainer, before converting:**
  - `vars` and `notes` file the Kindergarten Health Check's rows under `KHCMY`, but the run script's table is `KHCDLMY`. The old process found no rules for it, as it matched `Dataset` to `dat.tb`. The conversion leaves the 286 `vars` and 5 `notes` rows out until one of the two is changed.
  - `BCTG` has 204 rows in `vars`, without themes or ColumnAction, and no dataset in the run script. They are left out until it has one.
  - The order of themes in `linkage_theme_order` is a proposal: the themes most datasets share, then each dataset's own, and Data management last. The maintainer can change it before converting, or in SSMS after.
  - The template spells "Acknowledgment of Country"; the plan wrote "Acknowledgement". The conversion keeps the template's spelling.
- Confirm the name of `episerver_dictionary()` and the arguments of `dictionary_logo()` with the maintainer.
- `.gitignore` has `.xlsx`, `.xls` and `.csv`, which match only files of those names, not `*.xlsx`; a workbook inside the checkout is not ignored. The conversion's usage keeps the workbook and its script outside the repository. If the patterns are changed to `*.xlsx`, pull request 5 needs an exception for the template in `inst/`.
- Whether full dictionaries in the browser should group by theme when a profile is not chosen (the themes belong to a profile).
- The DOI is a placeholder in the run script; show it on the PDF's first page if it is set.

## How to check a change

**Offline:**
- The three commands in CLAUDE.md.
- The conversion: `Rscript -e 'testthat::test_file("dev/dictionaries/test-convert_definitions.R")'`. With the maintainer's workbook uploaded, convert it and run `dev/dictionaries/check_insert_script.py` on the script (its header shows how). Keep the workbook and the script in the scratchpad, never in the repository.
- The SQL files parse as T-SQL (sqlglot) and are plain ASCII.
- From pull request 4: the generated Typst compiled with Typst 0.13 and its pages looked at; the HTML file opened in the harness's headless Chromium; the workbook opened with openxlsx and its named ranges checked.

**Against EpiServer:**
- The maintainer runs `inst/sql/dictionary_tables.sql`, then `inst/sql/dictionary_example.sql`, which must end with no Example rows left, then the generated script, whose last queries count the rows against those expected and show the caveat symbols. Running the generated script a second time must stop with its message and change nothing.
- From pull request 4: the maintainer builds the linkage dictionary and variable list of a dataset, such as APC, and compares them with the last ones made by the old process.
