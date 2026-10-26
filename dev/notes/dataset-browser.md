# Dataset Browser

`episerver_browse()` opens the Dataset Browser, formerly the EpiServer Browser. Analysts use it to find a database, schema, table or view on EpiServer, read its columns with their labels, and copy the R code that reads the table. It is opened from Addins > Open Dataset Browser or by calling `episerver_browse()`. Pull request 1 rebuilt it to `dev/reference/app-style.md` and taught it to read the new label table, `ref.DataProperties`. That work later grows into one place for every kind of data dictionary (see `dictionaries.md`).

## Status

| Pull request | Content |
|---|---|
| #13, plan pull request 1 (merged; the live and offline tests passed in the maintainer's runs on 26 Oct 2026) | The browser rebuilt as a bslib app, and the labels lookup: `ref.DataProperties` first, then the DataLabels tables, each table falling back separately. The problem report, bug button, stopped page and Help tab style shared with the DSR Calculator; miniUI removed; the add-in renamed Open Dataset Browser. After review: the description and dataset notes on a Collection tab of their own; value labels shown by default, with Expand and Collapse buttons; chosen rows in Lavender with an Iris bar, a line counting them and Clear selection. Offline, `testServer()` and live tests, and the harness. |
| Plan pull request 2 (open, waiting on the maintainer's test against EpiServer) | `episerver_getlabels()` and `collect_withlabels()` read labels through the same lookup as the browser. `applydatalabels()` applies value labels to a column without a variable label. Offline and live tests. |

Pull request 1 was started on the branch `claude/sweet-ritchie-rt6bn7` (commit "Dataset Browser: work in progress and handoff notes") and finished on `claude/elegant-pasteur-05gapn`, which builds on that commit. The maintainer's run of `testthat::test_local()` in RStudio on 26 Oct 2026 ran every live test and passed them. Three offline tests failed there, because the code button reads Insert Code in RStudio and the tests expected Copy Code; one of them, in `test-episerver_dsr.R`, failed on `main` too. The tests now fix the label by mocking `rstudioapi::isAvailable()` (see `shared-apps.md`), and the maintainer's second run passed. Pull request 1 was merged as #13, and pull request 2 is on `claude/elegant-pasteur-05gapn`, restarted from `main` after the merge. Pull requests 3 to 6 have not started.

## Plan agreed with the maintainer (25 Oct 2026)

One pull request at a time on the session branch. The maintainer tests each one against EpiServer and merges it before the next starts; after a merge, restart the branch from `main`.

1. **Dataset Browser.** The rebuild and the labels lookup (this file).
2. **Labels in scripts.** `collect_withlabels()` and `episerver_getlabels()` use the same lookup. Before it, code copied from the browser imported labels from `ref.DataLabels` only. Decided with the maintainer on 26 Oct 2026:
   - `episerver_getlabels()` keeps returning rows in the DataLabels layout (`Dataset`, `VarName`, `LabelType`, `DataCode`, `LabelName`), with rows from `ref.DataProperties` translated into it, so that existing scripts and `applydatalabels()` keep working.
   - Only variable labels and value labels are applied, as now. Column notes, the dataset label and dataset notes are not attached to the data: the maintainer considered attributes for them, but they would be of little use to analysts for now. This may be revisited.
   - `lbl_db`, `lbl_schema` and `lbl_table`, when given, name the one label table to read, as the browser's Labels table choice does; left empty, the lookup chooses as the browser's Automatic does. This is what the arguments were written to mean.
   - The labels of linked tables (Link rows) are not applied automatically: `collect_withlabels()` applies the labels of the table collected. sqldta joins linked tables because Stata has no equivalent of dbplyr for writing the join; in R the analyst writes it, and split tables are rarely used. This may be revisited if `ref.DataProperties` comes to support one-to-many links. Plan pull request 2 originally included linked tables; the maintainer dropped them.
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
   - split tables: Link rows shown as one dataset, with the join in the copied code; `collect_withlabels()` may need refactoring for this. To be confirmed with the maintainer before it is built, as they chose not to automate linked tables in pull request 2;
   - reopen at the last database, schema and table (browser storage);
   - copy the dictionary as a table for Excel.

## Where the code is

| File | Holds |
|---|---|
| `R/episerver_labels.R` | The labels lookup: `.lbl_layouts`, `.lbl_types`, `.sql_ident()`, `.sql_quote()`; the SQL (`.lbl_sql_sources()`, `.lbl_where()`, `.lbl_sql_pick()`, `.lbl_sql_rows()`, `.lbl_sql_dataset_labels()`); the rows (`.lbl_normalise()`, `.lbl_dataset_labels()`, `.lbl_code_order()`, `.lbl_table()`) |
| `R/episerver_browse.R` | `episerver_browse()` and `episerver_browse_stop()`; `.browse_system_dbs`; the catalogue SQL (`.browse_sql_databases()`, `.browse_sql_schemas()`, `.browse_sql_tables()`, `.browse_sql_columns()`); the lists, columns' table and code (`.browse_databases()`, `.browse_table_choices()`, `.browse_type()`, `.browse_columns()`, `.browse_about()`, `.browse_labels_note()`, `.browse_code()`); the Help tab (`.browse_help()`); the app factory `episerver_browse_app()` |
| `R/epi_app.R` | The shared app code, including the problem report and the page shown once an app stops (see `shared-apps.md`) |
| `R/episerver_getlabels.R` | `episerver_getlabels()`; where a lazy table reads (`.lbl_table_path()`) and which table and label table are wanted (`.lbl_target()`). The labels are read by `.lbl_get()` and `.lbl_layout()` in `R/episerver_labels.R`. |
| `R/collect_withlabels.R`, `R/applydatalabels.R` | `collect_withlabels()`, which collects and applies `episerver_getlabels()`'s labels with `applydatalabels()` |

| Test file | Covers |
|---|---|
| `helper-browse.R` | `fake_browse_episerver()`: a synthetic EpiServer that answers the browser's SQL by reading it. The offline tests, the harness and the dry run all use it. |
| `test-episerver_labels.R` | The lookup's SQL, normalising both layouts, the type names, the order of notes and codes, and each table's dataset label |
| `test-episerver_browse.R` | The catalogue SQL, the lists, the columns' table, the info bar's labels line, the code, the page, and the app through `shiny::testServer()`: the fallback for each table, a label table chosen, a database without label tables, value labels read only when shown, read errors, the code button and the problem report |
| `test-episerver_browse_live.R` | Every query against `Analysis`, the order and layouts of its label tables, the pick against each label table asked alone, the dataset labels, every form of the copied code, and the app; skipped offline |
| `test-episerver_getlabels.R`, `test-applydatalabels.R` | Where a lazy table reads, the label table the `lbl_*` arguments name, `episerver_getlabels()` for a lazy table and a table's name and `collect_withlabels()` with the connection mocked, and `applydatalabels()` |
| `test-episerver_getlabels_live.R` | The labels scripts read for 31 tables of `Analysis.dbo` against the browser's, a label table named in either layout, `ref.DataLabels` named against the query `episerver_getlabels()` ran before the lookup, and `collect_withlabels()`; skipped offline. The last two read lazy tables, so they also need an ODBC driver, and skip in the dry run. |

## Decisions

- **Order in which label tables are tried.**
  - First `ref.DataProperties`, then today's order unchanged: tables named `DataLabels` (`ref` before other schemas, then by schema), then `DataLabelsNew` (the same), then other tables whose name holds DataLabels.
  - DataProperties is only looked for in the `ref` schema (the maintainer's decision).
  - Label tables may be tables or views, because sqldta's example builds `ref.DataProperties` as a view over the old table.
  - A label table is offered only if it has every column of its layout. DataLabels tables still need `DataType`, as before, although it is not read.
- **Fallback for each table.** A table takes its labels from the first label table, in that order, that holds rows for it (`.lbl_sql_pick()`: one `CASE WHEN EXISTS` query).
  - sqldta (`w4rhol/sqldta`) instead falls back only when the label table does not exist. The maintainer agreed to differ, for two reasons. Labels will move to DataProperties table by table. And sqldta's view (`examples/data_properties_view.sql`) files label sets that are not mapped under `dbo` only, so tables in other schemas would lose labels they have today.
  - During the move, R and Stata may therefore show different labels for a table.
  - The Table list follows the same rule: a table's dataset label comes from the first label table that holds any rows for it, even if that one has no dataset label and a later one has. Otherwise the list and the description on the Collection tab could show different labels for one table. `.lbl_sql_dataset_labels()` gets this in one query: each label table's dataset labels for the schema and, for every label table but the last, the names of the tables it holds rows for (`SELECT DISTINCT`, which an index on the name can serve). `.lbl_dataset_labels()` then takes each table's label in R. The parts of the `UNION ALL` take `COLLATE DATABASE_DEFAULT`, so that label tables of different collations do not conflict.
- **The two layouts,** as in sqldta's `docs/setup.md`:
  - DataProperties: `TableSchema`, `TableName`, `ColumnName`, `PropertyType`, `PropertyCode`, `PropertyText`, matched by schema and name with `N'...'` literals.
  - DataLabels: `Dataset`, `VarName`, `LabelType`, `DataCode`, `LabelName`, matched by table name only, with plain literals.
  - Types are read in any case. `Opt`, `Var`, `NoteVar`, `NoteDta` and `DtaLabel` map to ValueLabel, VariableLabel, VariableNote, DatasetNote and DatasetLabel. `DataLabel` (sqldta 1.0.0) maps to DatasetLabel.
  - VariableFormat (Stata display formats) is not read.
  - Duplicates keep the first row, as `applydatalabels()` does.
  - Notes with codes come first, in code order (numerically when every code of the column's notes is a number), then the rest in row order, as sqldta applies them.
  - Value labels are sorted numerically first, then as text.
  - Column names are matched to label rows in lower case, as SQL Server matches names.
- **Value labels are read only when shown** (`.lbl_sql_rows(values = TRUE)`), because a table can have many thousands. The other rows are read when a table is chosen. Show value labels is on by default (the maintainer's choice, 26 Oct 2026), so a table's value labels are read when it is chosen unless the analyst turns them off.
- **Name.** The app is "Dataset Browser" and the add-in "Open Dataset Browser", which keeps the add-ins' "Open <tool>" pattern (the maintainer's choice). `episerver_browse()` and `episerver_browse_stop()` keep their names. `episerver_browse()` passes `...` to `episerver_connect()`, documented with `@inheritDotParams episerver_connect encrypt trust_certificate`, as CLAUDE.md requires and `episerver_dsr()` does. `episerver_dsr()` inherits its parameters from `episerver_browse()`, so they stay documented there.
- **Tables and views** are both listed. Label tables (names holding DataLabels, `DataProperties` and sqldta's `DataPropertyTables`) are left out, in every schema.
- **Layout,** as built:
  - bslib `page_navbar()` with `.epi_theme()`, `.epi_app_style()` and the actepir lockup at the right. No Done button: `.epi_stopped_script("Dataset Browser", "Open Dataset Browser", "episerver_browse")` and, in the foreground, `session$onSessionEnded(shiny::stopApp)`.
  - Sidebar, 340px and resizable, in accordion sections that start open:
    - Dataset: Database (Analysis by default; system databases and those starting with an underscore hidden, as before), Schema (dbo by default), and a searchable Table list. Each option shows its dataset label: the label goes in the option text after a tab, and a selectize render function shows it muted (`.browse-label`, lighter on the chosen option's Nightshade). Tables and views are grouped. The table chosen stays chosen when the list changes and still holds it; otherwise the first table is chosen.
    - Labels: Labels table (Automatic by default, or any label table found) and Show value labels. A label table chosen in another database counts as Automatic until the list is updated.
    - Code: Connection (quickconnect, connect or none), Declare packages, Import with labels. The information buttons on Labels table and Connection hold their notes.
  - Foot (`.epi-actions`): Copy Code (Insert Code in the foreground) and `.epi_bug_button()`.
  - Tabs: Columns, Collection and Help. The Help tab uses the shared `.help` style, moved from the DSR Calculator to `.epi_app_style()`.
  - Info bar: the table, whether a table or a view, and its number of columns; where its labels came from (`.browse_labels_note()`), naming the earlier label tables that hold none for it; or that no label table holds labels for it, or that the database has none.
  - Read errors: a failure to read the tables or the columns makes the info bar an error (`epi-error`); a failure to list the databases or label tables, or to read the dataset labels, the labels or the value labels, adds a warning (`epi-warn`) and the rest is still shown.
  - The Collection tab holds the dataset label (the description, as a heading), the dataset notes and the linked tables (`.browse_about()`), or says the table has none. They were first shown above the column table, as sqldta's dialog shows them; the maintainer moved them to their own tab (26 Oct 2026), because some tables have many notes on the history of a survey or collection, which pushed the columns to the bottom of the window. Few tables have a description; more have notes. The tab was first called Dataset notes, with the number of notes in its title, as "Dataset notes (3)"; the maintainer chose the name Collection and a title without the number.
  - Above the column table, in a row (`options-row`): Expand value labels and Collapse value labels, which open and close every value labels list in the page, and are disabled while Show value labels is off; Clear selection, disabled while no row is chosen, which clears the rows chosen through `DT::selectRows()` on a proxy; and a line counting the columns chosen (`chosen_note`). Expand and Collapse work in the page alone, with no round trip. The maintainer asked for all three on 26 Oct 2026.
  - Column table (DT, `bootstrap5`): #, Column, Description, Type, plus Value labels (a `<details>` list, when shown) and Notes (when any column has them). Text from the label tables is escaped. Selected rows give the `select()` in the code; clicks on a value labels list do not select its row.
  - The table fits the window: a script sets `--browse-table-height` from the window's height less the table's distance from the top of the page (at least 200px), and DataTables' `scrollY` is `var(--browse-table-height, 50vh)`. This replaced the two sizing sliders.
  - The code generator moved into `.browse_code()`. It gives the old app's code with two changes, which the maintainer approved on 26 Oct 2026:
    - without Import with labels, the quickconnect and none forms end in `dplyr::collect()` when packages are declared. The old code had `actepir::collect()`, which fails, as actepir does not export `collect()`;
    - column names that are not syntactic R names (with spaces, reserved words such as `if`, or a leading digit or underscore) are backquoted in `select()`, by `deparse(as.name(x), backtick = TRUE)`. The old code wrote them bare, which does not parse.
  - Its output was compared with the old app's for 144 combinations: connection, declare, labels, none to three columns (the third named `Age group`), and the three forms of database and schema. 90 are identical, and each of the other 54 differs by one or both of the two changes alone.
  - The live test "the code the browser copies reads the table in every form" runs all 12 forms of the code on `ACTGHSMYX`. As the code connects through ODBC itself, it also needs a SQL Server driver, so it is skipped in the dry run.
- **Labels in scripts, as built (plan pull request 2).**
  - `episerver_getlabels()` reads the labels with `.lbl_get()`, which uses the browser's functions: `.lbl_sql_sources()`, `.lbl_sql_pick()`, `.lbl_sql_rows()` and `.lbl_normalise()`. So a script gets the labels the browser shows, trimmed, with types in any case and the first of duplicates kept. It runs four queries (the label tables, the pick, and the rows without and with value labels), where the old function ran one.
  - The result is a tibble in the DataLabels layout, as decided: `Dataset` (the table), `VarName`, `LabelType` (`Var` or `Opt`), `DataCode` (`NA` for `Var`) and `LabelName`; variable labels first, then value labels in code order. Other columns of a DataLabels table, such as `DataType`, are no longer returned.
  - A label table named by the `lbl_*` arguments is recognised by its columns, in either layout, whatever its name, as before the lookup; it needs `DataType` no more than it did then. A table with the columns of neither layout is refused with an error that names both.
  - A table's name alone (`episerver_getlabels("APC")`) is taken to be in `Analysis.dbo`, the defaults of `episerver_quickconnect()` (from `episerver_serverdetails()`), and the lookup chooses its label table. Before, a name needed all three `lbl_*` arguments, so the documented example `episerver_getlabels("APC")` failed. The connection made for a name is now closed once the labels are read.
  - A message says when there are no labels: when the database has no label table, when no label table holds rows for the table, or when the label table named holds none.
  - `applydatalabels()` used to drop the value labels of a column without a variable label: `reframe()` gave such a column no row. It now applies them, and sets a variable label only when there is one. Found while building the pull request; it is in a commit of its own.
- **Testability.** `episerver_browse_app(driver, max_attempts, ..., query = NULL)`, as `episerver_dsr_app()` has. One synthetic EpiServer, `fake_browse_episerver()` in `tests/testthat/helper-browse.R`, serves the offline tests, the harness (`dev/harness/fake_episerver.R` passes the browser's queries to it) and the dry run (`dev/harness/fake_live.R`). It reads the browser's SQL (the `EXISTS` conditions, the `WHERE` and type filters, the parts of the `UNION ALL`) rather than matching fixed strings, so the tests check the fallback itself. Its `properties = FALSE` gives an `Analysis` without `ref.DataProperties`.
- **Selected rows** are Lavender (`#DCC8FA`) with Nightshade text, which passes AA in `dev/reference/act-brand.md`, and an Iris bar 4px wide at their left. They first took the info bar's tint, `#eae6f1`, which the maintainer found far too faint beside the striped rows (26 Oct 2026). The colours are in the shared rule in `.epi_app_style()`, which sets DataTables' `--dt-row-selected` on `:root` and on `.table.dataTable`, where bslib's Bootstrap 5 theme sets it from the primary colour.

## Open items

- **Import with labels with a label table chosen.** The copied code calls `collect_withlabels()` without `lbl_*` arguments, so it applies the labels that Automatic finds, even when Labels table names another label table. The Help tab and `?episerver_browse` say so. Passing the chosen label table in the code would change the code for that input, so it waits on the maintainer.
- **Column names in another case.** `applydatalabels()` matches `VarName` to the data's column names exactly, while SQL Server, sqldta and the browser match them in any case. A label row whose column name differs in case from the table's is not applied in R. Waiting on the maintainer: matching in any case would change `applydatalabels()`.
- **Searching the Table list.** selectize ignores typing in a single select while it holds an item, so the analyst deletes the table shown with Backspace before typing. This is how every single select in the apps behaves; the Help tab says so.
- **Query times.** Choosing a table runs three queries (the pick, the rows and the columns), and a fourth for the value labels, which are shown by default; choosing a schema runs the tables query and the dataset labels query, which reads the table names held by every label table but the last. Their times on EpiServer have not been measured.
- **Environment.** This session's environment had R 4.6.1 and every dependency (30 of 30 packages loadable in `/var/log/r-setup.log`). roxygen2 was 8.1.1, so 8.0.0 was installed for the session, as `dev/harness/README.md` describes.

## How to check a change

**Offline:** the three commands in CLAUDE.md; `Rscript dev/harness/run_app.R dev/harness/episerver_browser.js <out>/browse browse`, compared with the results in `dev/harness/README.md`, with its screenshots looked at; and `Rscript dev/harness/dryrun_live.R`, with and without `--without-dataproperties`.

**Against EpiServer:** the maintainer runs `testthat::test_local()`, in which `test-episerver_browse_live.R` and `test-episerver_getlabels_live.R` must run rather than skip. They then open the browser on:

- a table that has rows in `ref.DataProperties`;
- one that has rows only in `ref.DataLabels`;
- one that has rows only in `dbo.DataLabels`;
- a view;

and, for the same tables, compare the labels that `collect_withlabels()` applies with those the browser shows.
