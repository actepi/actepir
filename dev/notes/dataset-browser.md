# Dataset Browser

`episerver_browse()` opens the Dataset Browser, formerly the EpiServer Browser. Analysts use it to find a database, schema, table or view on EpiServer, read its columns with their labels, and copy the R code that reads the table. It is opened from Addins > Open Dataset Browser or by calling `episerver_browse()`. Pull request 1 rebuilt it to `dev/reference/app-style.md` and taught it to read the new label table, `ref.DataProperties`. That work later grows into one place for every kind of data dictionary (see `dictionaries.md`).

## Status

| Pull request | Content |
|---|---|
| #13, plan pull request 1 (open, waiting on the maintainer's test against EpiServer) | The browser rebuilt as a bslib app, and the labels lookup: `ref.DataProperties` first, then the DataLabels tables, each table falling back separately. The problem report, bug button, stopped page and Help tab style shared with the DSR Calculator; miniUI removed; the add-in renamed Open Dataset Browser. Offline, `testServer()` and live tests, and the harness. |

Pull request 1 was started on the branch `claude/sweet-ritchie-rt6bn7` (commit "Dataset Browser: work in progress and handoff notes") and finished on `claude/elegant-pasteur-05gapn`, which builds on that commit. Pull requests 2 to 6 have not started. After pull request 1 is merged, restart the session branch from `main` for pull request 2.

## Plan agreed with the maintainer (25 Oct 2026)

One pull request at a time on the session branch. The maintainer tests each one against EpiServer and merges it before the next starts; after a merge, restart the branch from `main`.

1. **Dataset Browser.** The rebuild and the labels lookup (this file).
2. **Labels in scripts.** `collect_withlabels()` and `episerver_getlabels()` use the same lookup, including the labels of linked tables (Link rows). Until this is merged, code copied from the browser imports labels from `ref.DataLabels` only.
3. **Dictionary tables.** The SQL that creates them in `Analysis.ref`, and a script that converts the maintainer's `definitions.xlsx` into an INSERT script for SSMS (see `dictionaries.md`).
4. **Dictionaries.**
   - One builder for a full dictionary or a profile's (Linkage) dictionary, exported as PDF (Typst) and standalone HTML.
   - The per-user logo setting.
   - `episerver_dictionary()` for scripts and batches.
   - A Dictionary choice (full or a profile) and a Download menu in the browser.
5. **Variable list.** The linkage variable list workbook, built by the same builder (see `dictionaries.md`).
6. **Improvements, all approved:**
   - search a database for a column by name, label or value label;
   - a Tables tab (every table and view with its dataset label, approximate rows from `sys.partitions`, column count, last modified, whether labelled);
   - preview the first 100 rows with labels;
   - a profile of one column, on demand only (counts by value with labels, missing values, minimum and maximum, codes without labels);
   - label coverage: columns without labels, and tables with DataLabels rows but no DataProperties rows;
   - split tables: Link rows shown as one dataset, with the join in the copied code; `collect_withlabels()` may need refactoring for this;
   - reopen at the last database, schema and table (browser storage);
   - copy the dictionary as a table for Excel.

## Where the code is

| File | Holds |
|---|---|
| `R/episerver_labels.R` | The labels lookup: `.lbl_layouts`, `.lbl_types`, `.sql_ident()`, `.sql_quote()`; the SQL (`.lbl_sql_sources()`, `.lbl_where()`, `.lbl_sql_pick()`, `.lbl_sql_rows()`, `.lbl_sql_dataset_labels()`); the rows (`.lbl_normalise()`, `.lbl_dataset_labels()`, `.lbl_code_order()`, `.lbl_table()`) |
| `R/episerver_browse.R` | `episerver_browse()` and `episerver_browse_stop()`; `.browse_system_dbs`; the catalogue SQL (`.browse_sql_databases()`, `.browse_sql_schemas()`, `.browse_sql_tables()`, `.browse_sql_columns()`); the lists, columns' table and code (`.browse_databases()`, `.browse_table_choices()`, `.browse_type()`, `.browse_columns()`, `.browse_about()`, `.browse_labels_note()`, `.browse_code()`); the Help tab (`.browse_help()`); the app factory `episerver_browse_app()` |
| `R/epi_app.R` | The shared app code, including the problem report and the page shown once an app stops (see `shared-apps.md`) |

| Test file | Covers |
|---|---|
| `helper-browse.R` | `fake_browse_episerver()`: a synthetic EpiServer that answers the browser's SQL by reading it. The offline tests, the harness and the dry run all use it. |
| `test-episerver_labels.R` | The lookup's SQL, normalising both layouts, the type names, the order of notes and codes, and each table's dataset label |
| `test-episerver_browse.R` | The catalogue SQL, the lists, the columns' table, the info bar's labels line, the code, the page, and the app through `shiny::testServer()`: the fallback for each table, a label table chosen, a database without label tables, value labels read only when shown, read errors, the code button and the problem report |
| `test-episerver_browse_live.R` | Every query against `Analysis`, the order and layouts of its label tables, the pick against each label table asked alone, the dataset labels, every form of the copied code, and the app; skipped offline |

## Decisions

- **Order in which label tables are tried.**
  - First `ref.DataProperties`, then today's order unchanged: tables named `DataLabels` (`ref` before other schemas, then by schema), then `DataLabelsNew` (the same), then other tables whose name holds DataLabels.
  - DataProperties is only looked for in the `ref` schema (the maintainer's decision).
  - Label tables may be tables or views, because sqldta's example builds `ref.DataProperties` as a view over the old table.
  - A label table is offered only if it has every column of its layout. DataLabels tables still need `DataType`, as before, although it is not read.
- **Fallback for each table.** A table takes its labels from the first label table, in that order, that holds rows for it (`.lbl_sql_pick()`: one `CASE WHEN EXISTS` query).
  - sqldta (`w4rhol/sqldta`) instead falls back only when the label table does not exist. The maintainer agreed to differ, for two reasons. Labels will move to DataProperties table by table. And sqldta's view (`examples/data_properties_view.sql`) files label sets that are not mapped under `dbo` only, so tables in other schemas would lose labels they have today.
  - During the move, R and Stata may therefore show different labels for a table.
  - The Table list follows the same rule: a table's dataset label comes from the first label table that holds any rows for it, even if that one has no dataset label and a later one has. Otherwise the list and the description above the columns could show different labels for one table. `.lbl_sql_dataset_labels()` gets this in one query: each label table's dataset labels for the schema and, for every label table but the last, the names of the tables it holds rows for (`SELECT DISTINCT`, which an index on the name can serve). `.lbl_dataset_labels()` then takes each table's label in R. The parts of the `UNION ALL` take `COLLATE DATABASE_DEFAULT`, so that label tables of different collations do not conflict.
- **The two layouts,** as in sqldta's `docs/setup.md`:
  - DataProperties: `TableSchema`, `TableName`, `ColumnName`, `PropertyType`, `PropertyCode`, `PropertyText`, matched by schema and name with `N'...'` literals.
  - DataLabels: `Dataset`, `VarName`, `LabelType`, `DataCode`, `LabelName`, matched by table name only, with plain literals.
  - Types are read in any case. `Opt`, `Var`, `NoteVar`, `NoteDta` and `DtaLabel` map to ValueLabel, VariableLabel, VariableNote, DatasetNote and DatasetLabel. `DataLabel` (sqldta 1.0.0) maps to DatasetLabel.
  - VariableFormat (Stata display formats) is not read.
  - Duplicates keep the first row, as `applydatalabels()` does.
  - Notes with codes come first, in code order (numerically when every code of the column's notes is a number), then the rest in row order, as sqldta applies them.
  - Value labels are sorted numerically first, then as text.
  - Column names are matched to label rows in lower case, as SQL Server matches names.
- **Value labels are read only when shown** (`.lbl_sql_rows(values = TRUE)`), because a table can have many thousands. The other rows are read when a table is chosen.
- **Name.** The app is "Dataset Browser" and the add-in "Open Dataset Browser", which keeps the add-ins' "Open <tool>" pattern (the maintainer's choice). `episerver_browse()` and `episerver_browse_stop()` keep their names. `episerver_browse()` passes `...` to `episerver_connect()`, documented with `@inheritDotParams episerver_connect encrypt trust_certificate`, as CLAUDE.md requires and `episerver_dsr()` does. `episerver_dsr()` inherits its parameters from `episerver_browse()`, so they stay documented there.
- **Tables and views** are both listed. Label tables (names holding DataLabels, `DataProperties` and sqldta's `DataPropertyTables`) are left out, in every schema.
- **Layout,** as built:
  - bslib `page_navbar()` with `.epi_theme()`, `.epi_app_style()` and the actepir lockup at the right. No Done button: `.epi_stopped_script("Dataset Browser", "Open Dataset Browser", "episerver_browse")` and, in the foreground, `session$onSessionEnded(shiny::stopApp)`.
  - Sidebar, 340px and resizable, in accordion sections that start open:
    - Dataset: Database (Analysis by default; system databases and those starting with an underscore hidden, as before), Schema (dbo by default), and a searchable Table list. Each option shows its dataset label: the label goes in the option text after a tab, and a selectize render function shows it muted (`.browse-label`, lighter on the chosen option's Nightshade). Tables and views are grouped. The table chosen stays chosen when the list changes and still holds it; otherwise the first table is chosen.
    - Labels: Labels table (Automatic by default, or any label table found) and Show value labels. A label table chosen in another database counts as Automatic until the list is updated.
    - Code: Connection (quickconnect, connect or none), Declare packages, Import with labels. The information buttons on Labels table and Connection hold their notes.
  - Foot (`.epi-actions`): Copy Code (Insert Code in the foreground) and `.epi_bug_button()`.
  - Tabs: Columns and Help. The Help tab uses the shared `.help` style, moved from the DSR Calculator to `.epi_app_style()`.
  - Info bar: the table, whether a table or a view, and its number of columns; where its labels came from (`.browse_labels_note()`), naming the earlier label tables that hold none for it; or that no label table holds labels for it, or that the database has none.
  - Read errors: a failure to read the tables or the columns makes the info bar an error (`epi-error`); a failure to list the databases or label tables, or to read the dataset labels, the labels or the value labels, adds a warning (`epi-warn`) and the rest is still shown.
  - The dataset label (a heading), dataset notes and linked tables appear above the column table (`.browse_about()`).
  - Column table (DT, `bootstrap5`): #, Column, Description, Type, plus Value labels (a `<details>` list, when shown) and Notes (when any column has them). Text from the label tables is escaped. Selected rows give the `select()` in the code; clicks on a value labels list do not select its row.
  - The table fits the window: a script sets `--browse-table-height` from the window's height less the table's distance from the top of the page (at least 200px), and DataTables' `scrollY` is `var(--browse-table-height, 50vh)`. This replaced the two sizing sliders.
  - The code generator moved into `.browse_code()`. It gives the old app's code with two changes, which the maintainer approved on 26 Oct 2026:
    - without Import with labels, the quickconnect and none forms end in `dplyr::collect()` when packages are declared. The old code had `actepir::collect()`, which fails, as actepir does not export `collect()`;
    - column names that are not syntactic R names (with spaces, reserved words such as `if`, or a leading digit or underscore) are backquoted in `select()`, by `deparse(as.name(x), backtick = TRUE)`. The old code wrote them bare, which does not parse.
  - Its output was compared with the old app's for 144 combinations: connection, declare, labels, none to three columns (the third named `Age group`), and the three forms of database and schema. 90 are identical, and each of the other 54 differs by one or both of the two changes alone.
  - The live test "the code the browser copies reads the table in every form" runs all 12 forms of the code on `ACTGHSMYX`. As the code connects through ODBC itself, it also needs a SQL Server driver, so it is skipped in the dry run.
- **Testability.** `episerver_browse_app(driver, max_attempts, ..., query = NULL)`, as `episerver_dsr_app()` has. One synthetic EpiServer, `fake_browse_episerver()` in `tests/testthat/helper-browse.R`, serves the offline tests, the harness (`dev/harness/fake_episerver.R` passes the browser's queries to it) and the dry run (`dev/harness/fake_live.R`). It reads the browser's SQL (the `EXISTS` conditions, the `WHERE` and type filters, the parts of the `UNION ALL`) rather than matching fixed strings, so the tests check the fallback itself. Its `properties = FALSE` gives an `Analysis` without `ref.DataProperties`.
- **Selected rows** take the info bar's tint with dark text, as `dev/reference/app-style.md` says. bslib's Bootstrap 5 theme sets DataTables' `--dt-row-selected` on `.table.dataTable` from the primary colour, which painted them Nightshade, so the shared rule in `.epi_app_style()` now sets the variables there too.

## Open items

- **Import with labels** applies the labels of `ref.DataLabels` only, until pull request 2. The Help tab and `?episerver_browse` say so.
- **Searching the Table list.** selectize ignores typing in a single select while it holds an item, so the analyst deletes the table shown with Backspace before typing. This is how every single select in the apps behaves; the Help tab says so.
- **Query times.** Choosing a table runs three queries (the pick, the rows and the columns), and a fourth while value labels are shown; choosing a schema runs the tables query and the dataset labels query, which reads the table names held by every label table but the last. Their times on EpiServer have not been measured.
- **Environment.** This session's environment had R 4.6.1 and every dependency (30 of 30 packages loadable in `/var/log/r-setup.log`). roxygen2 was 8.1.1, so 8.0.0 was installed for the session, as `dev/harness/README.md` describes.

## How to check a change

**Offline:** the three commands in CLAUDE.md; `Rscript dev/harness/run_app.R dev/harness/episerver_browser.js <out>/browse browse`, compared with the results in `dev/harness/README.md`, with its screenshots looked at; and `Rscript dev/harness/dryrun_live.R`, with and without `--without-dataproperties`.

**Against EpiServer:** the maintainer runs `testthat::test_local()`, in which `test-episerver_browse_live.R` must run rather than skip. They then open the browser on:

- a table that has rows in `ref.DataProperties`;
- one that has rows only in `ref.DataLabels`;
- one that has rows only in `dbo.DataLabels`;
- a view.
