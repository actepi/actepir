# Dataset Browser

`episerver_browse()` opens the Dataset Browser, formerly the EpiServer Browser. Analysts use it to find a database, schema, table or view on EpiServer, read its columns with their labels, and copy the R code that reads the table. It is opened from Addins > Open Dataset Browser or by calling `episerver_browse()`. It is being rebuilt to `dev/reference/app-style.md` and to read the new label table, `ref.DataProperties`. That work later grows into one place for every kind of data dictionary (see `dictionaries.md`).

## Status

Work in progress on the session branch `claude/sweet-ritchie-rt6bn7`. This is pull request 1 of the plan below, and no pull request has been opened yet. The commit "Dataset Browser: work in progress and handoff notes" holds:

- `R/episerver_labels.R` (new, untested): the labels lookup described under Decisions.
- `R/epi_app.R`: the problem report, the bug button and the page shown once an app stops are now shared:
  - `.epi_problem_report()`, `.epi_bug_button()`, `.epi_report_script()`, `.epi_report_server()` and `.epi_stopped_script()`;
  - their CSS, and the sidebar tint `--bslib-sidebar-bg: #f7f4fd`, are now in `.epi_app_style()`;
  - the miniUI rules (`.gadget-*`, `#done`) are removed.
- `R/episerver_dsr.R`: the DSR Calculator calls those shared functions. `.dsr_problem_report()` is gone.
- Tests: the problem report test moved from `test-episerver_dsr.R` to `test-epi_app.R` and calls `.epi_problem_report()`.

Nothing in the commit has been run. The commit has two known breakages, which the rest of pull request 1 removes:

- `test-epi_app.R` still expects the `#done` hover rule and the add-in name "Open EpiServer Browser".
- `R/episerver_browse.R` is still the miniUI app, and it has lost its title bar and Done styles.

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
| `R/episerver_labels.R` | `.lbl_layouts`, `.lbl_types`, `.sql_ident()`, `.sql_quote()`, `.lbl_sql_sources()`, `.lbl_where()`, `.lbl_sql_pick()`, `.lbl_sql_rows()`, `.lbl_sql_dataset_labels()`, `.lbl_normalise()`, `.lbl_code_order()`, `.lbl_table()` |
| `R/episerver_browse.R` | Still the old miniUI app: to be rewritten |
| `R/epi_app.R` | The shared app code, including the problem report (see `shared-apps.md`) |

## Decisions

- **Order in which label tables are tried.**
  - First `ref.DataProperties`, then today's order unchanged: tables named `DataLabels` (`ref` before other schemas, then by schema), then `DataLabelsNew` (the same), then other tables whose name holds DataLabels.
  - DataProperties is only looked for in the `ref` schema (the maintainer's decision).
  - Label tables may be tables or views, because sqldta's example builds `ref.DataProperties` as a view over the old table.
  - A label table is offered only if it has every column of its layout. DataLabels tables still need `DataType`, as before, although it is not read.
- **Fallback for each table.** A table takes its labels from the first label table, in that order, that holds rows for it (`.lbl_sql_pick()`: one `CASE WHEN EXISTS` query).
  - sqldta (`w4rhol/sqldta`) instead falls back only when the label table does not exist. The maintainer agreed to differ, for two reasons. Labels will move to DataProperties table by table. And sqldta's view (`examples/data_properties_view.sql`) files label sets that are not mapped under `dbo` only, so tables in other schemas would lose labels they have today.
  - During the move, R and Stata may therefore show different labels for a table.
- **The two layouts,** as in sqldta's `docs/setup.md`:
  - DataProperties: `TableSchema`, `TableName`, `ColumnName`, `PropertyType`, `PropertyCode`, `PropertyText`, matched by schema and name with `N'...'` literals.
  - DataLabels: `Dataset`, `VarName`, `LabelType`, `DataCode`, `LabelName`, matched by table name only, with plain literals.
  - Types are read in any case. `Opt`, `Var`, `NoteVar`, `NoteDta` and `DtaLabel` map to ValueLabel, VariableLabel, VariableNote, DatasetNote and DatasetLabel. `DataLabel` (sqldta 1.0.0) maps to DatasetLabel.
  - VariableFormat (Stata display formats) is not read.
  - Duplicates keep the first row, as `applydatalabels()` does.
  - Notes with codes come first, in code order (numerically when every code of the column's notes is a number), then the rest in row order, as sqldta applies them.
  - Value labels are sorted numerically first, then as text.
- **Value labels are read only when shown** (`.lbl_sql_rows(values = TRUE)`), because a table can have many thousands. The other rows are read when a table is chosen.
- **Name.** The app is "Dataset Browser" and the add-in "Open Dataset Browser", which keeps the add-ins' "Open <tool>" pattern (the maintainer's choice). `episerver_browse()` and `episerver_browse_stop()` keep their names. Add `...` passed to `episerver_connect()`, documented with `@inheritDotParams episerver_connect encrypt trust_certificate`, as CLAUDE.md requires and `episerver_dsr()` does. `episerver_dsr()` inherits its parameters from `episerver_browse()`, so keep them documented there.
- **Tables and views** are both listed. Label tables (names holding DataLabels, `DataProperties` and sqldta's `DataPropertyTables`) are left out.
- **Layout (planned):**
  - bslib `page_navbar()` with `.epi_theme()`, `.epi_app_style()` and the actepir lockup at the right. No Done button: use `.epi_stopped_script("Dataset Browser", "Open Dataset Browser", "episerver_browse")` and, in the foreground, `session$onSessionEnded(shiny::stopApp)`.
  - Sidebar, 340px and resizable, in accordion sections:
    - Dataset: Database (Analysis by default; system databases and those starting with an underscore hidden, as now), Schema, and a searchable Table list. Each option shows its dataset label; the label goes in the option text after a tab, and a selectize render function shows it muted. Tables and views are grouped.
    - Labels: Labels table (Automatic by default, or any label table found) and Show value labels.
    - Code: quickconnect, connect or none; Declare packages; Import with labels.
  - Foot (`.epi-actions`): Copy Code (Insert Code in the foreground) and `.epi_bug_button()`.
  - Tabs: Columns and Help.
  - Info bar: the table and its type, the number of columns, which label table the labels came from, and when DataProperties held none for the table. Read errors are shown as warnings or errors.
  - The dataset label, dataset notes and linked tables appear above the column table.
  - Column table (DT): #, Column, Description, Type, plus Value labels (a `<details>` list, when shown) and Notes (when any). Selected rows give the `select()` in the code.
  - The table fits the window from a script, as the DSR Calculator's Explore plots do. This replaces the two sizing sliders.
  - The code generator stays as it was for the same inputs; move it into a function (`.browse_code()`) with offline tests.
  - Move the `.help` CSS (`.help`, `.help h5`) from the DSR Calculator to `.epi_app_style()` for the Help tab.
- **Testability.** `episerver_browse_app(driver, max_attempts, ..., query = NULL)`, as `episerver_dsr_app()` has, with `shiny::testServer()` tests on a fake.

## Open items

Next steps for pull request 1, in order:

1. Rewrite `R/episerver_browse.R` as described above, using `R/episerver_labels.R`.
2. Offline tests:
   - for `episerver_labels.R`: SQL text, normalising both layouts, type mapping, order of notes and codes, and the choice of label table;
   - for `.browse_code()`;
   - `testServer()` tests of the app.
3. A live test file, `tests/testthat/test-episerver_browse_live.R`. It should hold whether or not `ref.DataProperties` exists yet in `Analysis`.
4. Update the harness:
   - `dev/harness/fake_episerver.R`: the browser's new queries, with a DataProperties table, a DataLabels table, a table that falls back and a view;
   - `dev/harness/fake_live.R`;
   - `dev/harness/episerver_browser.js`: layout, info bar, labels source, hover, problem report and stopped page.
5. Other files:
   - `inst/rstudio/addins.dcf`: "Open Dataset Browser";
   - `DESCRIPTION`: remove miniUI;
   - `test-epi_app.R`: the hover test (no `#done`) and the add-in names.
6. Notes and documentation:
   - `shared-apps.md` and `dsr-calculator.md` still name `.dsr_problem_report()`;
   - `app-style.md` (remove the miniUI section; the sidebar tint is now in the shared style);
   - `dev/harness/README.md`;
   - the roxygen for `episerver_browse()`.
7. Run the three commands in CLAUDE.md, the harness and the dry run. Then open the pull request with the description CLAUDE.md asks for.

Other items:

- **Environment.** The cloud environment of the session that started this blocked CRAN and Posit Package Manager. R was installed with apt and packages were built from `github.com/cran`. Use an environment with R and the package's dependencies already installed.

## How to check a change

**Offline:** the three commands in CLAUDE.md, `Rscript dev/harness/run_app.R dev/harness/episerver_browser.js <out>/browse browse`, and `Rscript dev/harness/dryrun_live.R`.

**Against EpiServer:** the maintainer runs `testthat::test_local()`, in which `test-episerver_browse_live.R` must run rather than skip. They then open the browser on:

- a table that has rows in `ref.DataProperties`;
- one that has rows only in `ref.DataLabels`;
- one that has rows only in `dbo.DataLabels`;
- a view.
