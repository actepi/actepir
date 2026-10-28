#' Browse EpiServer's Databases, Tables and Columns
#'
#' @description
#' Opens the Dataset Browser, which finds a database, schema, table or view
#' on EpiServer, lists its columns with their labels, and gives R code that
#' reads the table. The labels come from the label tables of the table's
#' database: `ref.DataProperties`, which the Stata command sqldta also reads,
#' and the `DataLabels` tables (see **Labels**).
#'
#' By default the app runs in a background R process using
#' `callr::r_bg()`, following the technique described by Will Landau
#' (<https://wlandau.github.io/posts/2020-12-18-non-blocking-app/>), so the
#' console stays free while the browser is open. It opens in the system web
#' browser and can be opened from the RStudio Addins menu (*Open Dataset
#' Browser*). The settings are in a sidebar on the left, in sections that
#' open and close, and the tables of the schema, the columns, a profiler of
#' columns, the dataset's description and notes, a finder of columns in the
#' database and the help in tabs on the top bar. The
#' information button beside a setting shows notes on it, and the bug button
#' at the foot of the sidebar prepares a report of a problem to send to
#' \email{healthinfo@act.gov.au}.
#'
#' Close the browser tab when finished. A background browser keeps running,
#' so it opens again at once, until [episerver_browse_stop()] or the end of
#' the R session; with `background = FALSE`, closing the tab stops it.
#'
#' @param background Logical. If `TRUE` (default), the app runs in a
#'   background R process and the console remains free. If `FALSE`, the
#'   app runs in the current session, which it holds until its tab is closed.
#' @param port Integer or `NULL`. The local port for the background app.
#'   If `NULL` (default), the background process chooses a free port
#'   itself, retrying on collision.
#' @param driver Character string or `NULL`. ODBC driver to use for the
#'   connection. If `NULL` (default), the function will automatically select
#'   the appropriate driver using `episerver_serverdetails("driver")`.
#' @param max_attempts Integer to specify the number of connection attempts.
#'   Defaults to `episerver_connect` default.
#' @inheritDotParams episerver_connect encrypt trust_certificate
#'
#' @return In background mode, invisibly returns the `callr` process
#'   handle. In foreground mode, returns nothing.
#'
#' @details
#' **Finding a table**
#'
#' The *Dataset* section holds the *Database* (Analysis by default), the
#' *Schema* (dbo by default) and the *Table*. The table list holds the tables
#' and views of the schema, grouped, each with its dataset label. To find a
#' table by its name or its label, click the list, delete the table shown
#' with Backspace and type. SQL Server's system databases,
#' databases whose name begins with an underscore, and the label tables are
#' not listed.
#'
#' **Labels**
#'
#' A table's labels are the variable labels, value labels and notes of its
#' columns, and its dataset label, dataset description, dataset notes and
#' linked tables. They are read from the label tables of the table's
#' database, which may be tables or views:
#' * `ref.DataProperties`, in the layout that sqldta reads (`TableSchema`,
#'   `TableName`, `ColumnName`, `PropertyType`, `PropertyCode`,
#'   `PropertyText`), with rows filed by the table's schema and name, and,
#'   where it has them, `PropertyTheme` and `PropertyThemeOrder`, the
#'   headings of dataset notes and their order;
#' * tables named `DataLabels`, then `DataLabelsNew`, then others whose name
#'   holds DataLabels, each in the `ref` schema before others, in the layout
#'   `Dataset`, `VarName`, `LabelType`, `DataCode`, `LabelName` and
#'   `DataType`, with rows filed by the table's name alone.
#'
#' A label table is offered only if it has every column of its layout. With
#' *Labels table* set to Automatic (the default), each table takes its labels
#' from the first label table in this order that holds rows for it, so that a
#' table keeps its labels while they move from `DataLabels` to
#' `ref.DataProperties`. The info bar says which label table they came from.
#' Choosing a label table reads the labels from it alone.
#'
#' *Show value labels*, on by default, adds each column's value labels,
#' which are read only while shown, as a table can have thousands. Notes
#' with a code come first, in code order, then the others.
#'
#' **Columns and dataset notes**
#'
#' The *Columns* tab lists the table's columns with their position, name,
#' variable label (*Description*), type and, when present, value labels and
#' notes. The table fits the window and can be searched and sorted. Click
#' rows to choose columns for the code; the line above the table counts
#' them. The buttons above the table expand and collapse every list of value
#' labels, while value labels are shown, and clear the rows chosen.
#'
#' The *Collection* tab is headed by the table's dataset label, or its full
#' name when it has none. Below it are the dataset description, a point for
#' each paragraph, the dataset notes, under the headings of their themes
#' where they have them, and any linked tables. Paragraphs and notes are in
#' the order of their codes, and themes in their own order.
#'
#' **Tables and Var Finder**
#'
#' The *Tables* tab lists every table and view of the schema with its
#' description, type, approximate rows (from SQL Server's statistics; views
#' have none), number of columns, the columns without a variable label, the
#' label table its labels come from, and when it was made and when its
#' structure (not its data) last changed. Click a table to choose it.
#'
#' The *Var Finder* tab finds the columns of the database, in every schema,
#' whose names, variable labels or value labels hold the text typed, in the
#' labels the browser reads: with *Labels table* set to Automatic, only in
#' the label table each table's labels come from. Click a column to open its
#' table.
#'
#' **Profiler**
#'
#' The *Profiler* tab describes one column over the whole table: its missing
#' and distinct values, minimum and maximum, and the rows of each value with
#' its label, listing codes without a value label. With a *By* column it
#' gives a cross tab of the two, as rows or per cents of rows or columns.
#' *The whole table* gives a row for each column, with its missing and
#' blank values, distinct values, minimum, maximum and codes without a value
#' label, reading the columns in turn until done or stopped. It runs only
#' when *Profile* is clicked. Columns that are by themselves a primary key
#' or unique index, or whose names match a list of regular expressions kept
#' between sessions (identifiers such as `ID$` and `MRN`), are left out of
#' its lists and of the whole table's profile.
#'
#' **Code**
#'
#' The code button, in the *Columns* tab's own panel, gives R code that
#' reads the table, with a `select()` of the columns chosen. In foreground
#' mode it is labelled *Insert Code* and inserts the code at the cursor with
#' `rstudioapi::insertText()`. A background process has no connection to the
#' RStudio editor, so in background mode it is labelled *Copy Code* and
#' copies the code to the clipboard. The settings above it set the form of
#' the code:
#' * *Connection*: quickconnect (an [episerver_quickconnect()] pipe), connect
#'   ([episerver_connect()] and [episerver_lazytable()] calls) or none (only
#'   the pipe, from an existing lazy table `tb`);
#' * *Declare packages*: `pkg::` prefixes in the quickconnect and none forms,
#'   and `library()` calls in the connect form;
#' * *Import with labels*: [collect_withlabels()] in place of `collect()`.
#'   It applies the variable and value labels of the label table the browser
#'   reads: when *Labels table* names one, the code names it in `lbl_schema`
#'   and `lbl_table`.
#'
#' **Dictionary**
#'
#' *Download*, under the code button, gives the table's data dictionary as a
#' PDF or a standalone HTML file, as [episerver_dictionary()] writes them,
#' from the labels of the label table read. The PDF needs the Quarto that
#' comes with RStudio; without it, its item is disabled. *Dictionaries of all
#' tables*, in the same menu, writes the dictionaries of every table and view
#' with labels in the schema, or in the whole database, to a folder typed or
#' found in the dialog's list of folders, as HTML, PDF or both. *Copy
#' dictionary for Excel* copies the dictionary as a table, its cells marked
#' as text so that Excel keeps codes such as `01` as written. *Dictionary
#' logo* sets or removes the dictionaries' logo, as [dictionary_logo()] does.
#'
#' **Background process**
#'
#' In background mode the app keeps running when its browser tab is closed.
#' Calling the function again while the app is running reopens the existing
#' app rather than starting a second process, after first verifying it is
#' still serving; a process that is alive but no longer responding is killed
#' and replaced automatically. The app is stopped by
#' [episerver_browse_stop()] and when the R session exits. It runs in one
#' process, without processx's supervisor, which AppLocker blocks. After an
#' abnormal R crash the process keeps running, and can be ended from Task
#' Manager (Rterm.exe).
#' If the background process cannot be spawned in a restricted environment,
#' use `background = FALSE`.
#'
#' @note
#' * Requires an active network connection to EpiServer.
#' * The initial connection may take a few seconds to establish.
#' * Database, schema, table and column visibility is limited by the user's
#'   permissions.
#' * The database connection is validated before each query and
#'   re-established automatically if it has been dropped. A query that still
#'   fails is reported in the info bar.
#'
#' @seealso
#' [episerver_browse_stop()] for stopping the background app,
#' [episerver_info()] for non-interactive metadata queries,
#' [collect_withlabels()] for collecting data with labels applied
#'
#' @keywords episerver database metadata interactive
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch in a background process and open it in the web browser; the
#' # console stays free
#' episerver_browse()
#'
#' # Run in this R session, which it holds until its tab is closed
#' episerver_browse(background = FALSE)
#'
#' # Stop the background app
#' episerver_browse_stop()
#' }
#'
#' @author Warren Holroyd
#'
episerver_browse <- function(background = TRUE, port = NULL,
                             driver = NULL, max_attempts = NULL, ...) {

  .run_app(
    key          = "browse",
    title        = "Dataset Browser",
    fn           = "episerver_browse",
    factory      = "episerver_browse_app",
    factory_args = c(list(driver = driver, max_attempts = max_attempts),
                     list(...)),
    background   = background,
    port         = port
  )

}


#' Stop the Background Dataset Browser
#'
#' @description
#' Stops the background process started by \code{\link{episerver_browse}}.
#' Has no effect if no background browser is running.
#'
#' @return Invisibly returns \code{TRUE} if a process was stopped and
#'   \code{FALSE} otherwise.
#'
#' @seealso \code{\link{episerver_browse}}
#'
#' @keywords episerver interactive
#'
#' @export
#'
#' @examples
#' \dontrun{
#' episerver_browse_stop()
#' }
#'
#' @author Warren Holroyd
#'
episerver_browse_stop <- function() {

  .stop_app("browse", "Dataset Browser")

}


# ── Definitions ─────────────────────────────────────────────────────────────

# SQL Server's system databases, which the Database list leaves out, as it
# does databases whose name begins with an underscore
#' @noRd
.browse_system_dbs <- c("master", "model", "msdb", "tempdb")


# ── SQL ─────────────────────────────────────────────────────────────────────

#' @noRd
.browse_sql_databases <- function() {
  "SELECT name FROM sys.databases WHERE state_desc = 'ONLINE' ORDER BY name"
}

#' @noRd
.browse_sql_schemas <- function(db) {
  sprintf(
    "SELECT DISTINCT TABLE_SCHEMA
     FROM %s.INFORMATION_SCHEMA.TABLES
     ORDER BY TABLE_SCHEMA",
    .sql_ident(db)
  )
}

# The tables and views of schema in db, with TABLE_TYPE BASE TABLE or VIEW.
# Label tables are left out: names holding DataLabels, DataProperties and
# sqldta's DataPropertyTables.
#' @noRd
.browse_sql_tables <- function(db, schema) {
  sprintf(
    "SELECT TABLE_NAME, TABLE_TYPE
     FROM %s.INFORMATION_SCHEMA.TABLES
     WHERE TABLE_SCHEMA = %s
       AND TABLE_TYPE IN ('BASE TABLE', 'VIEW')
       AND TABLE_NAME NOT LIKE '%%DataLabels%%'
       AND TABLE_NAME NOT IN ('DataProperties', 'DataPropertyTables')
     ORDER BY TABLE_NAME",
    .sql_ident(db), .sql_quote(schema, TRUE)
  )
}

#' @noRd
.browse_sql_columns <- function(db, schema, table) {
  sprintf(
    "SELECT ORDINAL_POSITION         AS ord,
            COLUMN_NAME              AS col,
            DATA_TYPE                AS dtype,
            CHARACTER_MAXIMUM_LENGTH AS charlen,
            NUMERIC_PRECISION        AS numprec,
            NUMERIC_SCALE            AS numscale
     FROM %s.INFORMATION_SCHEMA.COLUMNS
     WHERE TABLE_SCHEMA = %s
       AND TABLE_NAME   = %s
     ORDER BY ORDINAL_POSITION",
    .sql_ident(db), .sql_quote(schema, TRUE), .sql_quote(table, TRUE)
  )
}

# When the tables and views of schema in db were made and last altered, and
# their approximate rows: TableName, Created, Altered (when the object's
# definition last changed, not its data) and ApproxRows, the rows of the
# heap or clustered index in SQL Server's partition statistics, so NULL for
# a view. The rows are cast to float, as odbc reads bigint as integer64.
#' @noRd
.browse_sql_details <- function(db, schema) {
  sprintf(
    "SELECT o.name AS TableName, o.create_date AS Created,
            o.modify_date AS Altered,
            (SELECT CAST(SUM(p.rows) AS float)
             FROM %1$s.sys.partitions p
             WHERE p.object_id = o.object_id
               AND p.index_id IN (0, 1)) AS ApproxRows
     FROM %1$s.sys.objects o
     JOIN %1$s.sys.schemas s ON s.schema_id = o.schema_id
     WHERE s.name = %2$s
       AND o.type IN ('U', 'V')",
    .sql_ident(db), .sql_quote(schema, TRUE)
  )
}

# The columns of every table and view of schema in db, by TABLE_NAME
#' @noRd
.browse_sql_schema_columns <- function(db, schema) {
  sprintf(
    "SELECT TABLE_NAME, COLUMN_NAME
     FROM %s.INFORMATION_SCHEMA.COLUMNS
     WHERE TABLE_SCHEMA = %s",
    .sql_ident(db), .sql_quote(schema, TRUE)
  )
}

# For each label table of `sources`, the tables of schema it holds rows for
# and the columns it gives a variable label: Source (the row number in
# `sources`), TableName, and ColumnKey, a column with a variable label, in
# lower case, or NULL on the row that says the table is held. DataLabels
# tables file rows by table name alone, so theirs are for every schema.
# Names take the database's collation, so that label tables of different
# collations can be read together.
#' @noRd
.browse_sql_coverage <- function(db, sources, schema) {
  parts <- vapply(seq_len(nrow(sources)), function(i) {
    properties <- sources$Layout[i] == "properties"
    f <- if (properties) {
      c(table = "TableName", column = "ColumnName", type = "PropertyType",
        text = "PropertyText")
    } else {
      c(table = "Dataset", column = "VarName", type = "LabelType",
        text = "LabelName")
    }
    sprintf(
      "SELECT DISTINCT %d AS Source,
         %s COLLATE DATABASE_DEFAULT AS TableName,
         CASE WHEN %s IN ('VariableLabel', 'Var')
               AND LTRIM(RTRIM(%s)) <> ''
              THEN LOWER(LTRIM(RTRIM(%s))) COLLATE DATABASE_DEFAULT
         END AS ColumnKey
       FROM %s%s",
      i, f[["table"]], f[["type"]], f[["text"]], f[["column"]],
      .sql_ident(db, sources$TABLE_SCHEMA[i], sources$TABLE_NAME[i]),
      if (properties) {
        paste0("\n       WHERE TableSchema = ", .sql_quote(schema, TRUE))
      } else ""
    )
  }, character(1))
  paste(parts, collapse = "\nUNION ALL\n")
}

# The columns of db whose names hold `term`, in any case: TABLE_SCHEMA,
# TABLE_NAME and COLUMN_NAME, of tables and views other than label tables,
# at most 1,000
#' @noRd
.browse_sql_search_columns <- function(db, term) {
  sprintf(
    "SELECT TOP 1000 c.TABLE_SCHEMA, c.TABLE_NAME, c.COLUMN_NAME
     FROM %1$s.INFORMATION_SCHEMA.COLUMNS c
     JOIN %1$s.INFORMATION_SCHEMA.TABLES t
       ON t.TABLE_SCHEMA = c.TABLE_SCHEMA AND t.TABLE_NAME = c.TABLE_NAME
     WHERE t.TABLE_TYPE IN ('BASE TABLE', 'VIEW')
       AND t.TABLE_NAME NOT LIKE '%%DataLabels%%'
       AND t.TABLE_NAME NOT IN ('DataProperties', 'DataPropertyTables')
       AND LOWER(c.COLUMN_NAME) LIKE LOWER(%2$s) ESCAPE '\\'
     ORDER BY c.TABLE_SCHEMA, c.TABLE_NAME, c.ORDINAL_POSITION",
    .sql_ident(db), .sql_like(term)
  )
}

# The variable labels (types "labels") and value labels ("values") in one
# label table (a row of `sources`) whose text holds `term`, in any case,
# with the columns named as in DataProperties; TableSchema is NULL for a
# DataLabels table. At most 1,000.
#' @noRd
.browse_sql_search_labels <- function(db, source, term,
                                      types = c("labels", "values")) {
  properties <- source$Layout == "properties"
  columns <- if (properties) {
    "TableSchema, TableName, ColumnName, PropertyType, PropertyCode, PropertyText"
  } else {
    paste("CAST(NULL AS nvarchar(128)) AS TableSchema, Dataset AS TableName,",
          "VarName AS ColumnName, LabelType AS PropertyType,",
          "DataCode AS PropertyCode, LabelName AS PropertyText")
  }
  names <- c(if ("labels" %in% types) c("VariableLabel", "Var"),
             if ("values" %in% types) c("ValueLabel", "Opt"))
  sprintf(
    "SELECT TOP 1000 %s
     FROM %s
     WHERE %s IN (%s)
       AND LOWER(%s) LIKE LOWER(%s) ESCAPE '\\'",
    columns,
    .sql_ident(db, source$TABLE_SCHEMA, source$TABLE_NAME),
    if (properties) "PropertyType" else "LabelType",
    paste(.sql_quote(names), collapse = ", "),
    if (properties) "PropertyText" else "LabelName",
    .sql_like(term)
  )
}

# Types that cannot be compared, grouped or counted distinct
#' @noRd
.browse_uncomparable <- c("text", "ntext", "image", "xml", "geography",
                          "geometry")

# A column's value as R reads it, by its type (`dtype`, as in
# INFORMATION_SCHEMA): bit as 0 or 1; binary types as hexadecimal text;
# spatial types and hierarchyid as their text; sql_variant and xml as text;
# others as they are
#' @noRd
.browse_sql_value <- function(col, dtype) {
  id <- .sql_ident(col)
  switch(
    tolower(dtype),
    bit = sprintf("CAST(%s AS tinyint)", id),
    binary = , varbinary = , image = , timestamp = , rowversion =
      sprintf("CONVERT(varchar(max), CAST(%s AS varbinary(max)), 1)", id),
    geography = , geometry = , hierarchyid = sprintf("%s.ToString()", id),
    sql_variant = sprintf("CAST(%s AS nvarchar(4000))", id),
    xml = sprintf("CAST(%s AS nvarchar(max))", id),
    id
  )
}

# One column's profile, over every row of table schema.table of db: its
# rows, the rows where it is not NULL, its distinct values, its least and
# greatest values, and, for text, the rows that are blank. Counts are cast
# to float, as odbc reads bigint as integer64. Types that cannot be compared
# give no distinct values, least or greatest.
#' @noRd
.browse_sql_profile <- function(db, schema, table, col, dtype) {
  id <- .sql_ident(col)
  value <- .browse_sql_value(col, dtype)
  comparable <- !tolower(dtype) %in% .browse_uncomparable
  text <- tolower(dtype) %in% c("char", "varchar", "nchar", "nvarchar")
  none <- "CAST(NULL AS float)"
  sprintf(
    "SELECT CAST(COUNT_BIG(*) AS float) AS TotalRows,
            CAST(SUM(CASE WHEN %s IS NULL THEN 0 ELSE 1 END) AS float) AS Present,
            %s AS DistinctValues,
            %s AS MinValue,
            %s AS MaxValue,
            %s AS Blank
     FROM %s",
    id,
    if (comparable) {
      sprintf("CAST(COUNT_BIG(DISTINCT %s) AS float)", value)
    } else none,
    if (comparable) sprintf("MIN(%s)", value) else "NULL",
    if (comparable) sprintf("MAX(%s)", value) else "NULL",
    if (text) {
      sprintf("CAST(SUM(CASE WHEN LTRIM(RTRIM(%s)) = '' THEN 1 ELSE 0 END) AS float)",
              id)
    } else none,
    .sql_ident(db, schema, table)
  )
}

# The rows of each value of one column of table schema.table of db, the
# most common 1,000 values, NULL among them, as ProfileValue and
# ProfileCount
#' @noRd
.browse_sql_profile_counts <- function(db, schema, table, col, dtype) {
  value <- .browse_sql_value(col, dtype)
  sprintf(
    "SELECT TOP 1000 %1$s AS ProfileValue,
            CAST(COUNT_BIG(*) AS float) AS ProfileCount
     FROM %2$s
     GROUP BY %1$s
     ORDER BY COUNT_BIG(*) DESC, %1$s",
    value, .sql_ident(db, schema, table)
  )
}

# The rows of each pair of values of column `col` and column `by` of table
# schema.table of db, NULL among them, as ProfileValue, ProfileBy and
# ProfileCount. It stops at 50,001 pairs, so that a cross tab too large to
# show is found without reading it all.
#' @noRd
.browse_sql_crosstab <- function(db, schema, table, col, dtype, by, by_dtype) {
  value <- .browse_sql_value(col, dtype)
  group <- .browse_sql_value(by, by_dtype)
  sprintf(
    "SELECT TOP 50001 %1$s AS ProfileValue, %2$s AS ProfileBy,
            CAST(COUNT_BIG(*) AS float) AS ProfileCount
     FROM %3$s
     GROUP BY %1$s, %2$s",
    value, group, .sql_ident(db, schema, table)
  )
}

# The columns of table schema.table of db that are by themselves a primary
# key or a unique index, as ColumnName: each value of such a column is
# found once, so the Profiler leaves it out. A column that is only one part
# of a key is not unique, and is kept.
#' @noRd
.browse_sql_keys <- function(db, schema, table) {
  sprintf(
    "SELECT DISTINCT c.name AS ColumnName
     FROM %1$s.sys.indexes i
     JOIN %1$s.sys.objects o ON o.object_id = i.object_id
     JOIN %1$s.sys.schemas s ON s.schema_id = o.schema_id
     JOIN %1$s.sys.index_columns ic
       ON ic.object_id = i.object_id AND ic.index_id = i.index_id
     JOIN %1$s.sys.columns c
       ON c.object_id = ic.object_id AND c.column_id = ic.column_id
     WHERE s.name = %2$s AND o.name = %3$s
       AND (i.is_primary_key = 1 OR i.is_unique = 1)
       AND ic.is_included_column = 0
       AND (SELECT COUNT(*) FROM %1$s.sys.index_columns k
            WHERE k.object_id = i.object_id AND k.index_id = i.index_id
              AND k.is_included_column = 0) = 1",
    .sql_ident(db), .sql_quote(schema, TRUE), .sql_quote(table, TRUE)
  )
}


# ── Lists, columns and code ─────────────────────────────────────────────────

# The databases offered from the names in sys.databases, or Analysis when
# none can be read
#' @noRd
.browse_databases <- function(names) {
  names <- as.character(names)
  names <- names[!tolower(names) %in% .browse_system_dbs &
                   !startsWith(names, "_")]
  if (length(names)) names else "Analysis"
}

# The Table list's choices from `tables` (.browse_sql_tables()): tables,
# then views, as groups. Each option's text is the table's name, then its
# dataset label from `labels` (.lbl_dataset_labels()) after a tab, which the
# list's render function shows muted.
#' @noRd
.browse_table_choices <- function(tables, labels) {
  if (is.null(tables) || !nrow(tables)) return(list())
  name  <- as.character(tables$TABLE_NAME)
  label <- unname(labels[tolower(name)])
  text  <- ifelse(is.na(label), name, paste0(name, "\t", label))
  group <- function(type) {
    keep <- tables$TABLE_TYPE == type
    stats::setNames(name[keep], text[keep])
  }
  out <- list(Tables = group("BASE TABLE"), Views = group("VIEW"))
  out[lengths(out) > 0]
}

# A column's type, with the length of character types and the precision
# and scale of numeric types that have a scale
#' @noRd
.browse_type <- function(dtype, charlen, numprec, numscale) {
  type  <- as.character(dtype)
  chars <- !is.na(charlen)
  type[chars] <- paste0(
    type[chars], "(",
    ifelse(charlen[chars] == -1, "max", as.character(charlen[chars])), ")"
  )
  scaled <- !chars & !is.na(numprec) & !is.na(numscale) & numscale > 0
  type[scaled] <- paste0(type[scaled], "(", numprec[scaled], ",",
                         numscale[scaled], ")")
  type
}

# The Columns tab's table, as HTML with the text escaped: the position,
# name, variable label and type of each column of `cols`
# (.browse_sql_columns()), its value labels when `values` is TRUE, and its
# notes when any column has notes, each in a list that opens. `lbl` is from
# .lbl_table(); columns are matched to its labels in lower case, as SQL
# Server matches names.
#' @noRd
.browse_columns <- function(cols, lbl, values = FALSE) {
  esc <- function(x) if (length(x)) htmltools::htmlEscape(x) else character()
  key   <- tolower(cols$col)
  label <- unname(lbl$label[key])
  out <- data.frame(
    `#`         = cols$ord,
    Column      = esc(as.character(cols$col)),
    Description = ifelse(is.na(label), "", esc(label)),
    Type        = .browse_type(cols$dtype, cols$charlen, cols$numprec,
                               cols$numscale),
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  if (values) {
    out$`Value labels` <- vapply(key, function(k) {
      v <- lbl$values[[k]]
      if (is.null(v) || !nrow(v)) return("")
      sprintf("<details><summary>%d label%s</summary><div>%s</div></details>",
              nrow(v), if (nrow(v) == 1) "" else "s",
              paste0(esc(v$Code), " = ", esc(v$Text), collapse = "<br>"))
    }, character(1), USE.NAMES = FALSE)
  }
  notes <- lapply(key, function(k) lbl$notes[[k]])
  if (any(lengths(notes) > 0)) {
    out$Notes <- vapply(notes, function(n) {
      if (!length(n)) return("")
      sprintf("<details><summary>%d note%s</summary><div>%s</div></details>",
              length(n), if (length(n) == 1) "" else "s",
              paste(esc(n), collapse = "<br>"))
    }, character(1))
  }
  out
}

# The Collection tab, from the table's dataset labels (dataset, from
# .lbl_table(), in order): its dataset label as the heading, or its full
# name `name` when it has none; its dataset description, a point for each
# paragraph; its dataset notes, under a heading for each theme when any has
# one, those without a theme under Other; a line where it has no description
# or no notes; and its linked tables, when it has any
#' @noRd
.browse_about <- function(dataset, name) {
  label <- dataset$label
  links <- dataset$links
  notes <- dataset$notes
  themes <- if (is.null(dataset$note_themes)) rep("", length(notes)) else {
    dataset$note_themes
  }
  points <- function(x) shiny::tags$ul(lapply(x, shiny::tags$li))
  none <- function(what) {
    shiny::p(paste("No dataset", what, "available for this collection."))
  }
  shiny::div(
    class = "browse-about",
    shiny::h4(if (length(label) && !is.na(label)) label else name),
    shiny::h5("Dataset description"),
    if (length(dataset$description)) points(dataset$description) else {
      none("description")
    },
    shiny::h5("Dataset notes"),
    if (!length(notes)) {
      none("notes")
    } else if (!any(nzchar(themes))) {
      points(notes)
    } else {
      lapply(unique(themes), function(theme) shiny::tagList(
        shiny::h6(if (nzchar(theme)) theme else "Other"),
        points(notes[themes == theme])
      ))
    },
    if (nrow(links)) {
      shiny::tagList(
        shiny::h5("Linked tables"),
        points(paste0(links$Table, " (on ", links$Key, ")"))
      )
    }
  )
}

# The info bar's line on where a table's labels came from. `source` is the
# label table used (a row of `sources`) or NULL; `sources` the label tables
# looked in, in order; `auto` whether they were looked for in order
# (Automatic), rather than in one label table chosen.
#' @noRd
.browse_labels_note <- function(source, sources, auto = TRUE) {
  name <- function(s) paste0(s$TABLE_SCHEMA, ".", s$TABLE_NAME)
  listed <- function(x) {
    if (length(x) < 2) return(x)
    paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
  }
  if (is.null(sources) || !nrow(sources)) {
    return("This database has no label table, so no labels are shown.")
  }
  names <- name(sources)
  if (is.null(source)) {
    if (!auto) return(paste(names[1], "holds no labels for this table."))
    return(paste0("No label table holds labels for this table (looked in ",
                  listed(names), ")."))
  }
  used <- name(source)
  earlier <- names[seq_len(match(used, names) - 1L)]
  paste0(
    "Labels from ", used,
    if (auto && length(earlier)) {
      paste0(", as ", listed(earlier),
             if (length(earlier) == 1) " holds" else " hold",
             " none for this table")
    },
    "."
  )
}

# ── Tables and search ───────────────────────────────────────────────────────

# The tables and views of database db, or of the schemas given: schema,
# table and type (BASE TABLE or VIEW), label tables left out
# (.browse_sql_tables())
#' @noRd
.browse_all_tables <- function(query, db, schemas = NULL) {
  if (is.null(schemas)) schemas <- query(.browse_sql_schemas(db))$TABLE_SCHEMA
  out <- lapply(schemas, function(s) {
    t <- query(.browse_sql_tables(db, s))
    data.frame(schema = rep(s, nrow(t)), table = as.character(t$TABLE_NAME),
               type = as.character(t$TABLE_TYPE), stringsAsFactors = FALSE)
  })
  do.call(rbind, c(out, list(data.frame(schema = character(),
                                        table = character(),
                                        type = character(),
                                        stringsAsFactors = FALSE))))
}

# Dates as yyyy-mm-dd, from date-times or text; "" where there is none
#' @noRd
.browse_date <- function(x) {
  if (is.null(x)) return(character())
  out <- if (inherits(x, "POSIXt")) format(x, "%Y-%m-%d") else {
    substr(as.character(x), 1, 10)
  }
  out[is.na(out)] <- ""
  out
}

# The Tables tab's table: for each table and view of `tables`
# (.browse_sql_tables()), its dataset label (`labels`, from
# .lbl_dataset_labels()), type, approximate rows and the dates it was made
# and last altered (`details`, .browse_sql_details()), its number of columns
# (`columns`, .browse_sql_schema_columns()), and, from `coverage`
# (.browse_sql_coverage() of the label tables `sources`), the label table
# its labels come from, the first that holds rows for it, and how many of
# its columns have no variable label there. Names are matched in lower
# case, as SQL Server matches them.
#' @noRd
.browse_tables_overview <- function(tables, labels, details, columns,
                                    coverage = NULL, sources = NULL) {
  name <- as.character(tables$TABLE_NAME)
  key  <- tolower(name)
  label <- unname(labels[key])
  d <- details[match(key, tolower(details$TableName)), , drop = FALSE]
  cols_of <- split(tolower(trimws(as.character(columns$COLUMN_NAME))),
                   tolower(as.character(columns$TABLE_NAME)))
  n_cols <- vapply(key, function(k) length(cols_of[[k]]), integer(1),
                   USE.NAMES = FALSE)
  from <- rep("", length(key))
  without <- n_cols
  if (!is.null(coverage) && nrow(coverage) && !is.null(sources) &&
      nrow(sources)) {
    held_by <- tolower(trimws(as.character(coverage$TableName)))
    for (i in seq_along(key)) {
      held <- coverage[held_by == key[i], , drop = FALSE]
      if (!nrow(held)) next
      first <- min(held$Source)
      from[i] <- paste0(sources$TABLE_SCHEMA[first], ".",
                        sources$TABLE_NAME[first])
      labelled <- held$ColumnKey[held$Source == first & !is.na(held$ColumnKey)]
      without[i] <- sum(!cols_of[[key[i]]] %in% labelled)
    }
  }
  data.frame(
    Table               = name,
    Description         = ifelse(is.na(label), "", label),
    Type                = ifelse(tables$TABLE_TYPE == "VIEW", "View", "Table"),
    Rows                = as.numeric(d$ApproxRows),
    Columns             = n_cols,
    `Without var labels` = without,
    `Labels from`       = from,
    Created             = .browse_date(d$Created),
    `Structure changed` = .browse_date(d$Altered),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# The columns of database db that `term` finds, in any case: by their name
# ("names" in `look_in`), variable label ("labels") or value labels
# ("values"), from the label tables `sources`. With `auto`, a label counts
# only when its table takes its labels from that label table, the first
# that holds rows for it, so that the search finds the labels the browser
# shows; otherwise `sources` is the one label table chosen. Labels in a
# DataLabels table are filed by table name alone, so they count for the
# tables of that name in every schema. Returns the columns found (results:
# Schema, Table, Column, the variable label and the value labels that
# matched, at most three of them shown) and whether a query stopped at its
# limit of 1,000 rows (capped).
#' @noRd
.browse_search <- function(query, db, term, look_in, sources, auto = TRUE) {
  limit <- 1000
  capped <- FALSE
  tables <- .browse_all_tables(query, db)
  in_db <- paste(tolower(tables$schema), tolower(tables$table), sep = ".")
  empty <- data.frame(schema = character(), table = character(),
                      column = character(), kind = character(),
                      code = character(), text = character(),
                      stringsAsFactors = FALSE)
  found <- list(empty)

  if ("names" %in% look_in) {
    n <- query(.browse_sql_search_columns(db, term))
    capped <- capped || nrow(n) >= limit
    n <- n[paste(tolower(n$TABLE_SCHEMA), tolower(n$TABLE_NAME), sep = ".") %in%
             in_db, , drop = FALSE]
    found$names <- data.frame(
      schema = as.character(n$TABLE_SCHEMA), table = as.character(n$TABLE_NAME),
      column = as.character(n$COLUMN_NAME), kind = rep("name", nrow(n)),
      code = rep("", nrow(n)), text = rep("", nrow(n)),
      stringsAsFactors = FALSE
    )
  }

  types <- intersect(c("labels", "values"), look_in)
  if (length(types) && !is.null(sources) && nrow(sources)) {
    text <- function(x) {
      x <- trimws(as.character(x))
      x[is.na(x)] <- ""
      x
    }
    labs <- do.call(rbind, lapply(seq_len(nrow(sources)), function(i) {
      r <- query(.browse_sql_search_labels(db, sources[i, , drop = FALSE],
                                           term, types))
      capped <<- capped || nrow(r) >= limit
      type <- unname(.lbl_types[tolower(text(r$PropertyType))])
      out <- data.frame(
        source = rep(i, nrow(r)), lschema = text(r$TableSchema),
        ltable = text(r$TableName), column = text(r$ColumnName),
        kind = ifelse(type %in% "ValueLabel", "value", "label"),
        code = text(r$PropertyCode), text = text(r$PropertyText),
        stringsAsFactors = FALSE
      )
      out[type %in% c("VariableLabel", "ValueLabel") & nzchar(out$text) &
            nzchar(out$column), , drop = FALSE]
    }))
    if (!is.null(labs) && nrow(labs)) {
      # Each label to the tables it is for: by schema and name, or by name
      # alone for a DataLabels table, whose rows have no schema
      to <- lapply(seq_len(nrow(labs)), function(j) {
        which(tolower(tables$table) == tolower(labs$ltable[j]) &
                (!nzchar(labs$lschema[j]) |
                   tolower(tables$schema) == tolower(labs$lschema[j])))
      })
      j <- rep(seq_len(nrow(labs)), lengths(to))
      t <- unlist(to)
      m <- data.frame(schema = tables$schema[t], table = tables$table[t],
                      labs[j, c("column", "kind", "code", "text", "source")],
                      stringsAsFactors = FALSE)
      if (auto && nrow(sources) > 1 && nrow(m)) {
        # The first label table that holds rows for each table, from the
        # query that gives the Table list its dataset labels
        first <- unlist(lapply(unique(m$schema), function(s) {
          r <- query(.lbl_sql_dataset_labels(db, sources, s))
          if (!nrow(r)) return(NULL)
          tapply(r$Source, paste(tolower(s), tolower(text(r$TableName)),
                                 sep = "."), min)
        }))
        f <- first[paste(tolower(m$schema), tolower(m$table), sep = ".")]
        f[is.na(f)] <- Inf
        m <- m[m$source <= f, , drop = FALSE]
      }
      found$labels <- m[names(empty)]
    }
  }

  all <- do.call(rbind, found)
  results <- data.frame(Schema = character(), Table = character(),
                        Column = character(), `Variable label` = character(),
                        `Value labels` = character(), check.names = FALSE,
                        stringsAsFactors = FALSE)
  if (nrow(all)) {
    key <- paste(tolower(all$schema), tolower(all$table), tolower(all$column),
                 sep = "\r")
    results <- do.call(rbind, lapply(split(all, factor(key, unique(key))),
                                     function(a) {
      named <- a$column[a$kind == "name"]
      vals <- a[a$kind == "value", , drop = FALSE]
      vals <- vals[!duplicated(vals$code), , drop = FALSE]
      vals <- vals[.lbl_code_order(vals$code), , drop = FALSE]
      shown <- utils::head(vals, 3)
      data.frame(
        Schema = a$schema[1], Table = a$table[1],
        Column = if (length(named)) named[1] else a$column[1],
        `Variable label` = c(a$text[a$kind == "label"], "")[1],
        `Value labels` = if (nrow(vals)) {
          paste0(paste0(shown$code, " = ", shown$text, collapse = "; "),
                 if (nrow(vals) > 3) paste0("; and ", nrow(vals) - 3, " more"))
        } else "",
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
    results <- results[order(tolower(results$Schema), tolower(results$Table),
                             tolower(results$Column)), , drop = FALSE]
    rownames(results) <- NULL
  }
  list(results = results, capped = capped)
}

# ── Profiler ────────────────────────────────────────────────────────────────

# Values as text: whole numbers without decimals or exponents, dates and
# times as R prints them, and NA as NA
#' @noRd
.browse_text <- function(x) {
  if (inherits(x, c("POSIXt", "Date"))) {
    out <- format(x)
  } else if (is.numeric(x) && !inherits(x, "integer64")) {
    out <- as.character(x)
    whole <- !is.na(x) & is.finite(x) & x == round(x) & abs(x) < 1e15
    out[whole] <- formatC(x[whole], format = "f", digits = 0)
  } else {
    out <- as.character(x)
  }
  out[is.na(x)] <- NA_character_
  out
}

# Text cut at `n` characters, marked with an ellipsis
#' @noRd
.browse_cut <- function(x, n = 200) {
  long <- !is.na(x) & nchar(x) > n
  x[long] <- paste0(substr(x[long], 1, n), "\u2026")
  x
}

# The value labels of `codes` (text, as .browse_text() gives them) in
# `vals`, a column's value labels (Code and Text, from .lbl_table()):
# matched as text, then as numbers, so that 1 finds a code written "01";
# NA where none matches
#' @noRd
.browse_value_labels <- function(codes, vals) {
  if (is.null(vals) || !nrow(vals)) return(rep(NA_character_, length(codes)))
  key <- trimws(codes)
  i <- match(key, trimws(vals$Code))
  num <- suppressWarnings(as.numeric(key))
  again <- is.na(i) & !is.na(num)
  i[again] <- match(num[again], suppressWarnings(as.numeric(vals$Code)))
  vals$Text[i]
}

# The Profiler's list of the columns it leaves out, as regular expressions
# (Perl's), each matched against a column's name with its case, so that ID
# can be told from the id of Valid or Opioid. These are offered until the
# analyst keeps a list of their own.
#' @noRd
.browse_exclude_default <- c("ID$", "Id$", "_id$", "^ID", "^id$", "MRN", "^URN")

# Where the Profiler's list is kept between sessions: actepir's folder in
# R's user settings directory, where the dictionaries' logo is kept
#' @noRd
.browse_exclude_file <- function() {
  file.path(tools::R_user_dir("actepir", "config"), "profiler_exclude.txt")
}

# The list kept, or the defaults when none is
#' @noRd
.browse_exclude_read <- function() {
  file <- .browse_exclude_file()
  if (!file.exists(file)) return(.browse_exclude_default)
  x <- trimws(readLines(file, warn = FALSE, encoding = "UTF-8"))
  x[nzchar(x)]
}

# Keeps `patterns` for later sessions; NULL goes back to the defaults
#' @noRd
.browse_exclude_save <- function(patterns) {
  file <- .browse_exclude_file()
  if (is.null(patterns)) {
    unlink(file)
  } else {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    con <- file(file, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(patterns, con)
  }
  invisible(NULL)
}

# The patterns typed, one a line (blank lines ignored), and those of them
# that are not regular expressions
#' @noRd
.browse_exclude_parse <- function(text) {
  x <- trimws(strsplit(if (is.null(text)) "" else text, "\n")[[1]])
  x <- x[nzchar(x)]
  # grepl() warns before it stops on a pattern that does not compile
  valid <- vapply(x, function(p) {
    !inherits(tryCatch(suppressWarnings(grepl(p, "", perl = TRUE)),
                       error = function(e) e),
              "error")
  }, logical(1), USE.NAMES = FALSE)
  list(patterns = x, invalid = x[!valid])
}

# Why each of `names` is left out: "key" when it is by itself a primary key
# or unique index (`keys`, matched in any case, as SQL Server matches
# names), else the first of `patterns` that matches it, or NA when it is kept
#' @noRd
.browse_excluded <- function(names, patterns, keys = character()) {
  why <- rep(NA_character_, length(names))
  why[tolower(names) %in% tolower(keys)] <- "key"
  for (p in patterns) {
    hit <- is.na(why) & grepl(p, names, perl = TRUE)
    why[hit] <- p
  }
  why
}

# The Profiler's cross tab from the rows of each pair of values (`pairs`,
# from .browse_sql_crosstab()): a row for each of the column's `max_rows`
# most common values, with its value label from `vals`; a column for each
# of the `max_cols` most common values of the By column, with the value
# label from `by_vals` as its heading; both in code order, NULL last; the
# rest as Other values and Other; and totals. `show` gives the rows
# ("rows"), or each cell as a per cent of its row's total ("row") or of its
# column's ("column"). Returns the table, the headings' titles (code and
# label), and whether rows or columns were put together as Other.
#' @noRd
.browse_crosstab <- function(pairs, vals = NULL, by_vals = NULL, show = "rows",
                             max_rows = 1000, max_cols = 20) {
  na <- "\r"
  other <- "\rOther"
  value <- .browse_text(pairs$ProfileValue)
  by <- .browse_text(pairs$ProfileBy)
  value[is.na(value)] <- na
  by[is.na(by)] <- na
  n <- pairs$ProfileCount

  # The most common values, in code order, NULL last
  top <- function(key, k) {
    totals <- tapply(n, key, sum)
    kept <- names(totals)[order(-totals)][seq_len(min(k, length(totals)))]
    codes <- setdiff(kept, na)
    c(codes[.lbl_code_order(codes)], intersect(na, kept))
  }
  rows <- top(value, max_rows)
  cols <- top(by, max_cols)
  value[!value %in% rows] <- other
  by[!by %in% cols] <- other
  rows <- c(rows, if (other %in% value) other)
  cols <- c(cols, if (other %in% by) other)
  m <- tapply(n, list(factor(value, rows), factor(by, cols)), sum)
  m[is.na(m)] <- 0
  m <- cbind(m, Total = rowSums(m))
  m <- rbind(m, Total = colSums(m))
  if (show == "row") {
    m <- 100 * m / m[, ncol(m)]
  } else if (show == "column") {
    m <- 100 * sweep(m, 2, m[nrow(m), ], "/")
  }
  m[!is.finite(m)] <- NA

  shown_as <- function(key, labels) {
    text <- ifelse(key == na, "NA", ifelse(key == other, "Other", key))
    code <- ifelse(key %in% c(na, other), NA, key)
    list(text = text, label = .browse_value_labels(code, labels))
  }
  r <- shown_as(c(rows, "Total"), vals)
  r$text[length(r$text)] <- "Total"
  r$text[r$text == "Other"] <- "Other values"
  table <- data.frame(Value = .browse_cut(r$text, 60), stringsAsFactors = FALSE)
  if (!is.null(vals) && nrow(vals)) {
    is_code <- !c(rows, "Total") %in% c(na, other, "Total")
    table$Label <- ifelse(!is.na(r$label), r$label,
                          ifelse(is_code, "(no label)", ""))
  }
  h <- shown_as(cols, by_vals)
  heads <- .browse_cut(ifelse(is.na(h$label), h$text, h$label), 40)
  same <- duplicated(heads) | duplicated(heads, fromLast = TRUE)
  heads[same] <- paste0(heads[same], " (", h$text[same], ")")
  titles <- ifelse(is.na(h$label), h$text, paste0(h$text, ": ", h$label))
  body <- as.data.frame(unclass(m), check.names = FALSE)
  names(body) <- c(heads, "Total")
  table <- cbind(table, body)
  rownames(table) <- NULL
  list(table = table, titles = c(titles, ""),
       rows_capped = other %in% rows, cols_capped = other %in% cols)
}

# The Profiler's results from one column's profile (`summary`, from
# .browse_sql_profile()), its counts (`counts`, from
# .browse_sql_profile_counts(), or NULL for a type that cannot be grouped)
# and its value labels (`vals`, Code and Text, or NULL): the summary as
# Measure and Value; the counts with their labels and per cent of rows; the
# codes found without a value label; and whether the counts hold every
# value, as they stop at 1,000
#' @noRd
.browse_profile <- function(summary, counts = NULL, vals = NULL) {
  big <- function(n) formatC(n, format = "f", digits = 0, big.mark = ",")
  total <- summary$TotalRows[1]
  present <- summary$Present[1]
  if (is.na(present)) present <- 0
  share <- function(n) {
    paste0(big(n), if (total > 0) sprintf(" (%.1f%%)", 100 * n / total))
  }
  labelled <- !is.null(vals) && nrow(vals) > 0
  with_label <- function(x) {
    x <- .browse_text(x)
    label <- .browse_value_labels(x, vals)
    .browse_cut(ifelse(is.na(label), x, paste0(x, " (", label, ")")), 60)
  }

  out <- NULL
  unlabelled <- character()
  complete <- FALSE
  if (!is.null(counts)) {
    value <- .browse_text(counts$ProfileValue)
    out <- data.frame(Value = ifelse(is.na(value), "NA", .browse_cut(value)),
                      stringsAsFactors = FALSE)
    if (labelled) {
      label <- .browse_value_labels(value, vals)
      out$Label <- ifelse(!is.na(label), label,
                          ifelse(is.na(value), "", "(no label)"))
      unlabelled <- value[!is.na(value) & is.na(label)]
    }
    out$Rows <- counts$ProfileCount
    out$`Per cent` <- if (total > 0) {
      100 * counts$ProfileCount / total
    } else rep(NA_real_, nrow(counts))
    complete <- nrow(counts) < 1000
  }

  distinct <- summary$DistinctValues[1]
  measures <- c(
    Rows = big(total),
    `Missing (NULL)` = share(total - present),
    `Blank text` = if (!is.na(summary$Blank[1])) share(summary$Blank[1]),
    `Distinct values` = if (!is.na(distinct)) big(distinct),
    Minimum = if (!is.na(distinct) && present > 0) with_label(summary$MinValue[1]),
    Maximum = if (!is.na(distinct) && present > 0) with_label(summary$MaxValue[1]),
    `Value labels` = if (labelled) big(nrow(vals)),
    `Codes without labels` = if (labelled && !is.null(counts)) {
      n <- length(unlabelled)
      paste0(
        if (!n) "None" else {
          paste0(big(n), " code", if (n != 1) "s", ": ",
                 paste(utils::head(unlabelled, 10), collapse = ", "),
                 if (n > 10) paste0(" and ", big(n - 10), " more"))
        },
        if (!complete) " among the 1,000 most common values"
      )
    }
  )
  list(summary = data.frame(Measure = names(measures), Value = unname(measures),
                            stringsAsFactors = FALSE),
       counts = out, unlabelled = unlabelled, complete = complete)
}

# The Profiler's profile of the whole table: a row for each column profiled,
# the first of `cols` (.browse_sql_columns()), one for each of `results`.
# Each result holds a column's summary and counts (.browse_sql_profile()
# and .browse_sql_profile_counts(), the counts read only for a column with
# value labels), or its error. Labels come from `lbl` (.lbl_table()).
# Returns the table, the table's rows, and whether codes without labels
# were looked for among the 1,000 most common values alone for any column.
#' @noRd
.browse_table_profile <- function(cols, results, lbl) {
  cols <- cols[seq_along(results), , drop = FALSE]
  key <- tolower(cols$col)
  big <- function(n) formatC(n, format = "f", digits = 0, big.mark = ",")
  one <- function(r, vals) {
    labelled <- !is.null(vals) && nrow(vals) > 0
    out <- list(total = NA_real_, missing = NA_real_, blank = NA_real_,
                distinct = NA_real_, min = NA_character_, max = NA_character_,
                labels = if (labelled) nrow(vals) else NA_real_,
                unlabelled = NA_real_, codes = "", capped = FALSE,
                error = NA_character_)
    if (!is.null(r$error)) {
      out$error <- r$error
      return(out)
    }
    s <- r$summary
    p <- .browse_profile(s, r$counts, vals)
    m <- stats::setNames(p$summary$Value, p$summary$Measure)
    total <- s$TotalRows[1]
    share <- function(n) if (is.na(n) || !total) NA_real_ else 100 * n / total
    present <- if (is.na(s$Present[1])) 0 else s$Present[1]
    out$total <- total
    out$missing <- share(total - present)
    out$blank <- share(s$Blank[1])
    out$distinct <- s$DistinctValues[1]
    out$min <- unname(m["Minimum"])
    out$max <- unname(m["Maximum"])
    if (labelled && !is.null(r$counts)) {
      u <- p$unlabelled
      out$unlabelled <- length(u)
      out$codes <- paste0(paste(utils::head(u, 5), collapse = ", "),
                          if (length(u) > 5) paste0(" and ", big(length(u) - 5),
                                                    " more"))
      out$capped <- !p$complete
    }
    out
  }
  parts <- Map(one, results, lapply(key, function(k) lbl$values[[k]]))
  get <- function(name, type) {
    vapply(parts, function(x) x[[name]], type, USE.NAMES = FALSE)
  }
  label <- unname(lbl$label[key])
  table <- data.frame(
    Column      = as.character(cols$col),
    Description = ifelse(is.na(label), "", label),
    Type        = .browse_type(cols$dtype, cols$charlen, cols$numprec,
                               cols$numscale),
    `Missing %` = get("missing", numeric(1)),
    `Blank %`   = get("blank", numeric(1)),
    `Distinct values` = get("distinct", numeric(1)),
    Minimum     = .browse_cut(get("min", character(1)), 40),
    Maximum     = .browse_cut(get("max", character(1)), 40),
    `Value labels` = get("labels", numeric(1)),
    `Codes without labels` = get("unlabelled", numeric(1)),
    Codes       = get("codes", character(1)),
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  error <- get("error", character(1))
  if (any(!is.na(error))) {
    table$Note <- ifelse(is.na(error), "", paste("Could not be read:", error))
  }
  total <- get("total", numeric(1))
  list(table = table, rows = total[!is.na(total)][1],
       capped = any(get("capped", logical(1))))
}

# R code that reads table schema.table of db: a pipe from
# episerver_quickconnect() ("quickconnect"), episerver_connect() and
# episerver_lazytable() calls ("connect"), or the pipe alone from a lazy
# table tb ("none"). `columns` are the columns chosen for select(), in
# backquotes when they are not syntactic R names; with `declare`, the
# quickconnect and none forms prefix their packages and the connect form
# calls library(); `labels` collects with collect_withlabels(), naming the
# label table `label_table` (its schema and name) when it is given,
# otherwise with dplyr's collect().
#' @noRd
.browse_code <- function(db, schema, table, columns = character(),
                         mode = "quickconnect", declare = TRUE,
                         labels = TRUE, label_table = NULL) {

  # Package qualifier for inline (quickconnect / none) code
  q <- function(pkg, fn) if (declare) paste0(pkg, "::", fn) else fn

  # select() pipe segment, only when columns are chosen
  names <- vapply(columns, function(x) deparse(as.name(x), backtick = TRUE),
                  character(1), USE.NAMES = FALSE)
  sel_args <- paste(names, collapse = ", ")
  have_sel <- length(columns) > 0
  dplyr_select <- if (have_sel) {
    paste0("  ", q("dplyr", "select"), "(", sel_args, ") |>\n")
  } else {
    ""
  }
  plain_select <- if (have_sel) {
    paste0("  select(", sel_args, ") |>\n")
  } else {
    ""
  }

  collect_call <- if (!labels) {
    "collect()"
  } else if (is.null(label_table)) {
    "collect_withlabels()"
  } else {
    sprintf('collect_withlabels(lbl_schema = "%s", lbl_table = "%s")',
            label_table[1], label_table[2])
  }
  collect_pkg  <- if (labels) "actepir" else "dplyr"

  # Argument tail shared by quickconnect and lazytable calls
  arg_tail <- if (db == "Analysis" && schema == "dbo") {
    sprintf('"%s"', table)
  } else if (db == "Analysis") {
    sprintf('"%s", schema = "%s"', table, schema)
  } else {
    sprintf('"%s", schema = "%s", db = "%s"', table, schema, db)
  }

  if (mode == "quickconnect") {
    # Inline, package-qualified pipe; no intermediate 'tb'
    paste0(
      "data <- ", q("actepir", "episerver_quickconnect"), "(", arg_tail, ") |>\n",
      dplyr_select,
      "  ", q(collect_pkg, collect_call), "\n"
    )
  } else if (mode == "connect") {
    # Multi-object form with optional library() declarations
    header <- if (declare) "library(actepir)\nlibrary(dplyr)\n\n" else ""
    paste0(
      header,
      "conn <- episerver_connect()\n",
      "tb <- episerver_lazytable(conn, ", arg_tail, ")\n\n",
      "data <- tb |>\n",
      plain_select,
      "  ", collect_call, "\n"
    )
  } else {
    # none: assume 'tb' already exists; emit only the collection pipe
    paste0(
      "data <- tb |>\n",
      dplyr_select,
      "  ", q(collect_pkg, collect_call), "\n"
    )
  }
}

# Opening a folder in Explorer (shell.exec()), or NULL where R has no
# shell.exec(), as off Windows. It is looked up, so that R CMD check on other
# systems does not miss it.
#' @noRd
.browse_open_fn <- function() {
  get0("shell.exec", envir = baseenv(), mode = "function", inherits = FALSE)
}

# The folder whose folders the folder list shows: `folder` when it exists,
# or else the nearest folder above it that does, with forward slashes; NA
# when `folder` is empty or no part of it exists
#' @noRd
.browse_folder_at <- function(folder) {
  folder <- trimws(if (is.null(folder)) "" else folder)
  if (!nzchar(folder)) return(NA_character_)
  repeat {
    if (dir.exists(folder)) return(normalizePath(folder, winslash = "/"))
    up <- dirname(folder)
    if (identical(up, folder)) return(NA_character_)
    folder <- up
  }
}

# The folders in `at`, by name, leaving out hidden and system folders
#' @noRd
.browse_subfolders <- function(at) {
  found <- tryCatch(list.dirs(at, full.names = FALSE, recursive = FALSE),
                    error = function(e) character())
  found <- found[nzchar(found) & !grepl("^[.$]", found)]
  found[order(tolower(found))]
}

# The folder list below the Folder box of Dictionaries of all tables: Up,
# Home and Working directory, then the folders in the folder given (or in
# the nearest folder above it that exists). A click on any of them puts its
# path in the box, through input$bulk_into. Windows' folder dialog is not
# used, as it does not open from the browser's background process.
#' @noRd
.browse_folder_ui <- function(folder) {
  at <- .browse_folder_at(folder)
  slash <- function(path) normalizePath(path, winslash = "/", mustWork = FALSE)
  go <- function(label, path, icon) {
    shiny::tags$button(type = "button", class = "btn btn-sm btn-primary browse-folder",
                       `data-folder` = path, disabled = if (is.null(path)) NA,
                       shiny::icon(icon), label)
  }
  parent <- if (!is.na(at)) dirname(at)
  if (!is.null(parent) && (identical(parent, at) || !dir.exists(parent))) {
    parent <- NULL
  }
  subs <- if (!is.na(at)) .browse_subfolders(at) else character()
  given <- trimws(if (is.null(folder)) "" else folder)
  note <- if (is.na(at)) {
    if (nzchar(given)) "No part of this folder's path exists." else {
      "Give a folder, or start from Home or the working directory."
    }
  } else if (!dir.exists(given)) {
    paste0("This folder does not exist yet; it will be made. The list shows ",
           "the folders in ", at, ".")
  } else if (!length(subs)) {
    "There are no folders in this folder."
  }
  shiny::tagList(
    shiny::div(
      class = "options-row",
      go("Up", if (!is.null(parent)) slash(parent), "arrow-up"),
      go("Home", slash("~"), "house"),
      go("Working directory", slash(getwd()), "folder-open")
    ),
    if (!is.null(note)) shiny::p(class = "epi-note", note),
    if (length(subs)) {
      shiny::div(class = "list-group browse-folders", lapply(subs, function(s) {
        shiny::tags$button(
          type = "button",
          class = "list-group-item list-group-item-action browse-folder",
          `data-folder` = paste0(sub("/$", "", at), "/", s),
          shiny::icon("folder"), s
        )
      }))
    }
  )
}

# Opens `folder` in Explorer, where it can
#' @noRd
.browse_open_dir <- function(folder) {
  open <- .browse_open_fn()
  if (!is.null(open) && dir.exists(folder)) open(normalizePath(folder))
  invisible(NULL)
}

# The dialog of Download > Dictionaries of all tables: the tables and views
# (of the schema chosen, or of the whole database), the files (PDF only with
# Quarto) and the folder, with the folder list (.browse_folder_ui()) below it
#' @noRd
.browse_bulk_dialog <- function(db, schema, has_quarto, folder) {
  files <- shiny::checkboxGroupInput(
    "bulk_formats", "Files",
    choiceNames  = list("HTML", if (has_quarto) "PDF" else {
      "PDF (needs Quarto, which comes with RStudio)"
    }),
    choiceValues = list("html", "pdf"),
    selected     = c("html", if (has_quarto) "pdf"), width = "100%"
  )
  if (!has_quarto) {
    files <- htmltools::tagQuery(files)$find("input")$
      filter(function(x, i) identical(x$attribs$value, "pdf"))$
      addAttrs(disabled = NA)$allTags()
  }
  dialog <- shiny::modalDialog(
    title = "Dictionaries of all tables", easyClose = TRUE,
    shiny::p(
      "Writes the dictionary of every table and view that has labels to a ",
      "folder on this computer, from the label table read. Tables and views ",
      "without labels are left out, and files of the same names are replaced."
    ),
    shiny::radioButtons(
      "bulk_scope", "Tables and views",
      choiceNames  = list(paste0("Of the schema ", db, ".", schema),
                          paste0("Of the whole database ", db,
                                 ", each schema in a folder of its name")),
      choiceValues = list("schema", "db"), width = "100%"
    ),
    files,
    shiny::textInput("bulk_dir", "Folder", value = folder, width = "100%"),
    shiny::uiOutput("bulk_folders"),
    shiny::p(class = "epi-note",
             "A PDF takes a second or two to make, so the dictionaries of a ",
             "large database can take some minutes."),
    footer = shiny::tagList(
      shiny::actionButton("bulk_write", "Write dictionaries",
                          icon = shiny::icon("download"),
                          class = "btn-sm btn-primary"),
      shiny::tags$button(type = "button", class = "btn btn-sm btn-primary",
                         `data-bs-dismiss` = "modal", "Close")
    )
  )
  htmltools::tagQuery(dialog)$find(".modal-dialog")$
    addClass("modal-dialog-scrollable")$allTags()
}

# The dialog that sets the dictionaries' logo through dictionary_logo(): the
# logo shown on the band's Nightshade (logo_shown), a PNG to choose, and the
# actepir logo again
#' @noRd
.browse_logo_dialog <- function() {
  shiny::modalDialog(
    title = "Dictionary logo", easyClose = TRUE,
    shiny::p(
      "The dictionaries show this logo at the right of their band. A logo ",
      "chosen here is kept for later sessions, as ",
      shiny::tags$code("dictionary_logo()"), " keeps it."
    ),
    shiny::uiOutput("logo_shown"),
    shiny::fileInput("logo_file", "Choose a PNG",
                     accept = c(".png", "image/png"), width = "100%"),
    shiny::p(class = "epi-note",
             "A white logo on a transparent background, at least 600 pixels ",
             "wide and under 500 KB, suits the band."),
    footer = shiny::tagList(
      shiny::actionButton("logo_reset", "Use the actepir logo",
                          icon = shiny::icon("rotate-left"),
                          class = "btn-sm btn-primary"),
      shiny::tags$button(type = "button", class = "btn btn-sm btn-primary",
                         `data-bs-dismiss` = "modal", "Close")
    )
  )
}

# The dialog once the dictionaries are written: how many, as which files and
# where; the tables and views left out (.dict_summary()); those that failed;
# and a button that opens the folder, on Windows
#' @noRd
.browse_bulk_done <- function(listed, done, formats, folder, where,
                              can_open = !is.null(.browse_open_fn())) {
  n <- length(done$dictionaries)
  failed <- done$failed
  shiny::modalDialog(
    title = "Dictionaries written", easyClose = TRUE,
    shiny::p(if (n) {
      paste0("Wrote ", n, " dictionar", if (n == 1) "y" else "ies", ", as ",
             paste(toupper(formats), collapse = " and "), ", to ", folder, ".")
    } else {
      "No dictionary was written."
    }),
    shiny::p(.dict_summary(listed, where)),
    if (length(failed)) {
      shiny::tagList(
        shiny::p(class = "epi-note epi-error", "These could not be written:"),
        shiny::tags$ul(lapply(names(failed), function(name) {
          shiny::tags$li(paste0(name, ": ", failed[[name]]))
        }))
      )
    },
    footer = shiny::tagList(
      if (can_open && n) {
        shiny::actionButton("bulk_open", "Open the folder",
                            icon = shiny::icon("folder-open"),
                            class = "btn-sm btn-primary")
      },
      shiny::tags$button(type = "button", class = "btn btn-sm btn-primary",
                         `data-bs-dismiss` = "modal", "Close")
    )
  )
}

# The Help tab: finding a table, where its labels come from, choosing
# columns and the code
#' @noRd
.browse_help <- function() {
  code <- function(x) shiny::tags$code(x, .noWS = "outside")
  shiny::div(
    class = "help",

    shiny::h5("Finding a table"),
    shiny::p(
      "Choose a database, schema and table in the Dataset section of the ",
      "sidebar. The Table list shows each table and view with its ",
      "description. To search the list, click it, press Backspace to clear ",
      "the table shown, then type part of a table's name or description."
    ),
    shiny::p(
      "System databases, databases whose names start with an underscore, ",
      "and the label tables themselves are not listed."
    ),

    shiny::h5("Tables"),
    shiny::p(
      "The Tables tab lists every table and view in the schema. For each, it ",
      "shows:"
    ),
    shiny::tags$ul(
      shiny::tags$li("its description, and whether it is a table or a view;"),
      shiny::tags$li("its approximate number of rows;"),
      shiny::tags$li("its number of columns, and how many have no variable ",
                     "label;"),
      shiny::tags$li("the label table its labels come from;"),
      shiny::tags$li("when it was created, and when its structure last ",
                     "changed.")
    ),
    shiny::p(
      "Click a row to choose that table. Row counts come from SQL Server's ",
      "statistics, so they are approximate, and views have none. Structure ",
      "changed is when the table's columns or definition last changed, not ",
      "its data. If you choose a label table in the sidebar, the label ",
      "columns refer to that label table only."
    ),

    shiny::h5("Columns"),
    shiny::p(
      "The Columns tab lists the table's columns with their descriptions ",
      "(variable labels) and types. When Show value labels is ticked in the ",
      "sidebar, each column's value labels are in a list you can open; ",
      "Expand value labels and Collapse value labels open or close them all. ",
      "A column's notes, when it has any, are in a list in the Notes column, ",
      "numbered notes first, in order."
    ),
    shiny::p(
      "Click rows to choose the columns for your code, and click a row again ",
      "to remove it. Chosen rows are shaded, and the line above the table ",
      "says how many you have chosen. Clear selection removes them all. ",
      "Searching or sorting the table does not change your choice. If you ",
      "choose no columns, the code reads them all."
    ),

    shiny::h5("Copying code"),
    shiny::p(
      "Copy Code, in the panel on the left of the Columns tab, copies R code ",
      "that reads the table and selects the columns you have chosen, ready ",
      "to paste into a script. If you started the browser with ",
      code("episerver_browse(background = FALSE)"), ", the button is called ",
      "Insert Code and puts the code at the cursor in RStudio instead."
    ),
    shiny::p("The settings above the button shape the code:"),
    shiny::tags$ul(
      shiny::tags$li(
        "Connection: quickconnect reads the table with ",
        code("episerver_quickconnect()"), "; connect opens a connection with ",
        code("episerver_connect()"), " and the table with ",
        code("episerver_lazytable()"), "; none leaves out the connection, so ",
        "the code starts from a lazy table called ", code("tb"), " that you ",
        "have already made."
      ),
      shiny::tags$li(
        "Declare packages puts ", code("actepir::"), " and ", code("dplyr::"),
        " before each function, or adds ", code("library()"), " calls when ",
        "Connection is connect."
      ),
      shiny::tags$li(
        "Import with labels reads the data with ",
        code("collect_withlabels()"), ", which applies the same variable and ",
        "value labels the browser shows. If you have chosen a label table in ",
        "the sidebar, the code names it."
      )
    ),

    shiny::h5("Dictionaries"),
    shiny::p(
      "The Download menu, under Copy Code, gives the table's data ",
      "dictionary: its description, notes and linked tables, and every ",
      "column with its labels, including value labels that are not shown."
    ),
    shiny::tags$ul(
      shiny::tags$li(
        "Dictionary as PDF shows up to 50 value labels for each column. It ",
        "needs the Quarto that comes with RStudio."
      ),
      shiny::tags$li(
        "Dictionary as HTML is a single file with every value label and a ",
        "search box, which you can send to others as it is."
      ),
      shiny::tags$li(
        "Copy dictionary for Excel copies the dictionary as a table, with a ",
        "row for each value label, to paste into a worksheet. Codes such as ",
        "01 and labels such as 1-4 keep their exact form."
      ),
      shiny::tags$li(
        "Dictionaries of all tables writes a dictionary for every table and ",
        "view with labels in the schema, or in the whole database, to a ",
        "folder on your computer, as HTML, PDF or both. Type or paste the ",
        "folder, or find it in the list below the box; a folder that does ",
        "not exist is created. Each PDF takes a second or two, so a large ",
        "database can take several minutes."
      ),
      shiny::tags$li(
        "Dictionary logo changes the logo on the dictionaries. Choose a PNG ",
        "file, or go back to the actepir logo. Your choice is kept for later ",
        "sessions, and ", code("dictionary_logo()"), " does the same in the ",
        "console."
      )
    ),
    shiny::p(
      code("episerver_dictionary()"), " makes the same files from a script."
    ),

    shiny::h5("Profiler"),
    shiny::p(
      "The Profiler tab summarises the values in a column, or in every ",
      "column, across all the rows of the table. Choose One column or The ",
      "whole table at the top of the panel, then click Profile. Profiling ",
      "reads the whole table, so it runs only when you click Profile, and a ",
      "large table can take a while."
    ),
    shiny::p(
      "One column: the panel shows the number of rows, missing (NULL) ",
      "values, blank text, distinct values, the minimum and maximum, and any ",
      "codes that have no value label. The table lists the 1,000 most common ",
      "values with their labels, counts and per cents. The first column you ",
      "chose on the Columns tab is offered first."
    ),
    shiny::p(
      "By: choose a second column for a cross tab. Each value of the first ",
      "column is a row, and the 20 most common values of the second are ",
      "columns, with the rest grouped as Other. Show switches between counts ",
      "and per cents of each row or column."
    ),
    shiny::p(
      "The whole table: a row for each column, with its missing values and ",
      "blank text as per cents of the rows, its distinct values, minimum and ",
      "maximum, its number of value labels, and any codes without a value ",
      "label. The columns are read one at a time, and Stop keeps the results ",
      "so far. Click a heading to sort by it, or a row to profile that ",
      "column on its own."
    ),
    shiny::p(
      "Identifiers are left out: columns that are a primary key or unique ",
      "index on their own, and columns whose names match a pattern in ",
      "Columns left out, at the bottom of the panel. Each line of that list ",
      "is a regular expression and is case sensitive: ^ID matches names that ",
      "start with ID, ID$ names that end with ID, and MRN names that contain ",
      "MRN. Start a line with (?i) to ignore case. Save the list keeps your ",
      "changes for later sessions; Restore the defaults goes back to the ",
      "original list."
    ),

    shiny::h5("Collection"),
    shiny::p(
      "The Collection tab is headed by the table's dataset label, or its ",
      "full name when it has none. Below it are the dataset description, a ",
      "point for each paragraph, and the dataset notes, grouped under their ",
      "themes, such as General or Data quality, when they have them. ",
      "Paragraphs and notes are in the order of their codes, and themes in ",
      "their set order. Any linked tables are listed last."
    ),

    shiny::h5("Var Finder"),
    shiny::p(
      "The Var Finder tab searches every schema in the database for columns ",
      "whose names, variable labels or value labels contain the text you ",
      "type, ignoring case. Look in sets where to search. Labels are ",
      "searched as the browser reads them: with Labels table set to ",
      "Automatic, only the label table each table takes its labels from is ",
      "searched. Each kind of search returns at most 1,000 matches. Click a ",
      "result to open its table on the Columns tab."
    ),

    shiny::h5("Where labels come from"),
    shiny::p(
      "Labels are read from the label tables (tables or views) in the ",
      "table's database, in this order:"
    ),
    shiny::tags$ol(
      shiny::tags$li(
        code("ref.DataProperties"), ", which the Stata command sqldta also ",
        "reads, matched by the table's schema and name;"
      ),
      shiny::tags$li(
        "tables named ", code("DataLabels"), ", then ",
        code("DataLabelsNew"), ", then other tables with DataLabels in ",
        "their name, those in the ", code("ref"), " schema first, matched ",
        "by the table's name only."
      )
    ),
    shiny::p(
      "With Labels table set to Automatic, each table takes its labels from ",
      "the first label table that has rows for it, and the bar at the top ",
      "of each tab says which one. Choose a label table to read labels from ",
      "it only. A label table is offered only when it has all the columns ",
      "its layout needs."
    ),

    shiny::h5("Reporting a problem"),
    shiny::p(paste0(
      "The bug button at the bottom of the sidebar prepares a report to send ",
      "to ", .epi_contact, ". It includes your settings and the names of the ",
      "database, table and columns you chose, but no data or labels."
    ))
  )
}


# ── App ─────────────────────────────────────────────────────────────────────

# Internal factory that builds the Shiny app. Runs in the current session for
# foreground mode, or inside the callr background process for background mode.
# The database connection is created here and closed when the app stops.
# `query` replaces the EpiServer connection with any function that takes SQL
# and returns a data frame; tests use it to run the app offline.
#' @noRd
episerver_browse_app <- function(driver = NULL, max_attempts = NULL, ...,
                                 query = NULL) {

  # ── Check dependencies ────────────────────────────────────────────────────
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE) ||
      !requireNamespace("DT", quietly = TRUE)) {
    stop(
      "The 'shiny', 'bslib', and 'DT' packages are required for episerver_browse().\n",
      "Install them with: install.packages(c('shiny', 'bslib', 'DT'))",
      call. = FALSE
    )
  }

  # Foreground (RStudio session) or background process?
  in_rstudio <- tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)

  # ── Establish connection ──────────────────────────────────────────────────
  db <- NULL
  if (is.null(query)) {
    connect_args <- list()
    if (!is.null(driver)) connect_args$driver <- driver
    if (!is.null(max_attempts)) connect_args$max_attempts <- max_attempts
    db <- .epi_connection(c(connect_args, list(...)))
    query <- db$query
  }

  # A query's rows, or its error, so that a query that still fails after the
  # reconnect in .epi_connection() is reported rather than stopping the app
  read <- function(sql) {
    tryCatch(list(data = query(sql), error = NULL),
             error = function(e) list(data = NULL, error = conditionMessage(e)))
  }

  databases <- read(.browse_sql_databases())
  db_choices <- .browse_databases(databases$data$name)

  code_button_label <- if (in_rstudio) "Insert Code" else "Copy Code"

  # The PDF dictionary needs Quarto; without it its item is disabled
  has_quarto <- !is.null(.dict_quarto())

  # Shows each table's dataset label muted after its name in the Table list,
  # and the name alone once chosen. selectize styles the elements by the
  # classes option and item, which a render function must give.
  table_render <- I("{
    option: function(item, escape) {
      var parts = item.label.split('\\t');
      return '<div class=\"option\">' + escape(parts[0]) + (parts.length > 1 ?
        ' <span class=\"browse-label\">' + escape(parts.slice(1).join(' ')) +
        '</span>' : '') + '</div>';
    },
    item: function(item, escape) {
      return '<div class=\"item\">' + escape(item.label.split('\\t')[0]) +
        '</div>';
    }
  }")

  # ── UI ────────────────────────────────────────────────────────────────────
  ui <- bslib::page_navbar(

    title        = "Dataset Browser",
    window_title = "Dataset Browser",
    id           = "view",
    theme        = .epi_theme(),
    fillable     = FALSE,

    header = shiny::tagList(

      .epi_app_style(),

      # Browser-only rules: the dataset labels in the Table list, light on
      # the chosen table's Nightshade, the Collection tab, the line on the
      # columns chosen, the value labels and notes in the columns' table,
      # whose lists scroll once open, the folder list of Dictionaries of all
      # tables, the profile's summary, and the logo on the band's Nightshade
      # in the Dictionary logo dialog
      shiny::tags$style(shiny::HTML("
        .browse-label { color: #777; }
        .selectize-dropdown .option.selected .browse-label { color: inherit;
                                                             opacity: 0.8; }
        .browse-about { max-width: 900px; }
        .browse-about h4 { color: var(--epi-primary); font-weight: 600;
                           font-size: 1.25rem; margin: 4px 0 6px 0; }
        .browse-about h5 { color: var(--epi-primary); font-weight: 600;
                           font-size: 1rem; margin: 16px 0 6px 0; }
        .browse-about h6 { color: var(--epi-accent); font-weight: 600;
                           font-size: 0.95rem; margin: 10px 0 4px 0; }
        .browse-about ul { margin-bottom: 6px; padding-left: 20px; }
        .browse-about li { margin-bottom: 4px; }
        .browse-about p { margin-bottom: 6px; }
        .browse-chosen { color: #555; }
        td.browse-values details summary,
        td.browse-notes details summary { cursor: pointer;
                                          color: var(--epi-accent); }
        td.browse-values details div,
        td.browse-notes details div { max-height: 150px; overflow-y: auto; }
        .browse-folders { max-height: 220px; overflow-y: auto;
                          overscroll-behavior: contain;
                          margin-bottom: var(--epi-gap); }
        .browse-folders .list-group-item { font-size: var(--epi-font-box);
          padding: var(--epi-box-pad-y) var(--epi-box-pad-x); }
        .browse-profile { font-size: var(--epi-font-box);
                          margin-bottom: var(--epi-gap); }
        .browse-profile th { font-weight: 600; padding-right: 12px; }
        .browse-exclude > summary { cursor: pointer; color: var(--epi-accent);
                                    margin-bottom: var(--epi-gap-small); }
        .browse-logo { background: var(--epi-primary); border-radius: 4px;
                       padding: 12px 16px; text-align: right;
                       margin-bottom: var(--epi-gap-small); }
        .browse-logo img { height: 48px; max-width: 100%; }
      ")),

      # A click on a folder in the list of Dictionaries of all tables
      # (.browse_folder_ui()) sends its path, read from the attribute so
      # that a name such as 2024 stays text
      shiny::tags$script(shiny::HTML("
        $(document).on('click', '.browse-folder', function() {
          Shiny.setInputValue('bulk_into', this.getAttribute('data-folder'),
                              {priority: 'event'});
        });
      ")),

      # Clipboard handler for the code button
      .epi_copy_script(),

      # Clicks on the value labels or the notes open and scroll their list
      # without choosing the row. DT's selection handler may be bound to
      # mousedown or pointer events rather than click, so the guard stops the
      # whole pointer-event family in the capture phase, before any delegated
      # handler. stopPropagation() leaves the browser's default actions, so
      # the <details> toggle, scrolling and text selection still work.
      shiny::tags$script(shiny::HTML("
        ['pointerdown', 'mousedown', 'mouseup', 'click'].forEach(function(type) {
          document.addEventListener(type, function(e) {
            var t = e.target;
            if (t && t.closest &&
                t.closest('#col_table td.browse-values, #col_table td.browse-notes')) {
              e.stopPropagation();
            }
          }, true);
        });
      ")),

      # The buttons above the columns' table. Expand and Collapse open and
      # close every value labels list, and work only while value labels
      # are shown; Clear selection works only while rows are chosen.
      shiny::tags$script(shiny::HTML("
        (function() {
          function values() {
            var off = !$('#show_values').prop('checked');
            $('#values_open, #values_close').prop('disabled', off);
          }
          $(document).on('change', '#show_values', values);
          $(values);
          $(document).on('click', '#values_open, #values_close', function() {
            $('#col_table td.browse-values details')
              .prop('open', this.id === 'values_open');
          });
          $(document).on('shiny:inputchanged', function(e) {
            if (e.name === 'col_table_rows_selected') {
              $('#clear_rows').prop('disabled', !(e.value && e.value.length));
            }
          });
        })();
      ")),

      # The tables of the Tables, Columns, Profiler and Var Finder tabs fit
      # the window below their top, so that they need no page
      # scrolling: a script sets --browse-table-height on each, which caps
      # its scrolling body (scrollY)
      shiny::tags$script(shiny::HTML("
        (function() {
          function fit(id) {
            var body = document.querySelector('#' + id + ' .dataTables_scrollBody');
            if (!body || !body.offsetParent) return;
            var top = body.getBoundingClientRect().top;
            for (var el = body.parentElement; el; el = el.parentElement) {
              top += el.scrollTop;
            }
            var room = Math.max(window.innerHeight - top - 16, 200);
            var el = document.getElementById(id);
            el.style.setProperty('--browse-table-height', room + 'px');
            // What wraps the table can add space below it, as a tab's own
            // settings panel does: take the greatest overflow of the areas
            // around it that scroll off the table's height
            var over = 0;
            for (var box = body.parentElement; box; box = box.parentElement) {
              if (/(auto|scroll)/.test(getComputedStyle(box).overflowY) ||
                  box === document.scrollingElement) {
                over = Math.max(over, box.scrollHeight - box.clientHeight);
              }
            }
            if (over > 0) {
              el.style.setProperty('--browse-table-height',
                                   Math.max(room - over, 200) + 'px');
            }
          }
          function soon() {
            setTimeout(function() {
              ['col_table', 'tables_table', 'search_table', 'profile_table']
                .forEach(fit);
            }, 0);
          }
          $(window).on('resize', soon);
          $(document).on('shown.bs.tab shiny:value init.dt', soon);
        })();
      ")),

      # The Var Finder tab's button, or Enter in its box, sends the box's
      # text as input$search_run, as the box's own value reaches the server
      # only once typing pauses
      shiny::tags$script(shiny::HTML("
        (function() {
          function run() {
            Shiny.setInputValue('search_run', $('#search_text').val(),
                                {priority: 'event'});
          }
          $(document).on('click', '#search_go', run);
          $(document).on('keydown', '#search_text', function(e) {
            if (e.key === 'Enter') run();
          });
        })();
      ")),

      # The problem report, and the page shown once the app stops
      .epi_report_script(),
      .epi_stopped_script("Dataset Browser", "Open Dataset Browser",
                          "episerver_browse"),

      # Info bar: the table, its columns and where its labels came from
      shiny::conditionalPanel("input.view !== 'help'",
                              shiny::uiOutput("info_bar"))
    ),

    # The resize handle is moved off the scrollbar by .epi_app_style()
    sidebar = bslib::sidebar(
      id = "settings", width = 340, open = "desktop", resizable = TRUE,
      bslib::accordion(
        id = "settings_sections", open = TRUE, multiple = TRUE,
        bslib::accordion_panel(
          "Dataset", value = "dataset",
          shiny::selectInput(
            "db", "Database",
            choices  = db_choices,
            selected = if ("Analysis" %in% db_choices) "Analysis" else db_choices[1],
            width    = "100%"
          ),
          shiny::selectInput("schema", "Schema", choices = NULL,
                             width = "100%"),
          shiny::selectizeInput(
            "table", "Table", choices = NULL, width = "100%",
            options = list(render = table_render,
                           placeholder = "No tables or views")
          )
        ),
        bslib::accordion_panel(
          "Labels", value = "labels",
          .epi_with_help(
            shiny::selectInput("labels_source", "Labels table",
                               choices = c("Automatic" = "auto"),
                               width = "100%"),
            "Automatic takes each table's labels from the first label table ",
            "that holds rows for it: ref.DataProperties, then tables named ",
            "DataLabels, DataLabelsNew and others whose name holds ",
            "DataLabels, each in ref before other schemas. Choosing a label ",
            "table reads the labels from it alone."
          ),
          shiny::checkboxInput("show_values", "Show value labels",
                               value = TRUE)
        )
      ),
      shiny::div(
        class = "epi-actions",
        .epi_bug_button()
      )
    ),

    # Every table and view of the schema, with its rows, dates and labels.
    # A click on one chooses it. The browser opens on this tab, the first.
    bslib::nav_panel(
      "Tables", value = "tables",
      shiny::div(
        class = "options-row",
        shiny::span(class = "browse-chosen",
                    shiny::textOutput("tables_note", inline = TRUE))
      ),
      DT::dataTableOutput("tables_table")
    ),

    # The columns, with the code and the dictionary in the tab's own panel,
    # as the code reads the columns chosen here, and buttons above the table
    # for their value labels and the rows chosen. The Download menu is
    # placed with a fixed position, so that the panel does not clip it.
    bslib::nav_panel(
      "Columns", value = "columns",
      .epi_tab_settings(
        "columns_settings",
        shiny::tagList(
          .epi_with_help(
            shiny::radioButtons(
              "insert_mode", "Connection",
              choices  = c("quickconnect", "connect", "none"),
              selected = "quickconnect"
            ),
            "quickconnect reads the table with episerver_quickconnect(); ",
            "connect opens a connection with episerver_connect() and the ",
            "table with episerver_lazytable(); none gives the pipe alone, ",
            "from a lazy table tb made earlier. Import with labels uses ",
            "collect_withlabels(), which applies the variable and value ",
            "labels of the label table the browser reads."
          ),
          shiny::checkboxInput("declare_pkgs", "Declare packages",
                               value = TRUE),
          shiny::checkboxInput("use_labels", "Import with labels",
                               value = TRUE),
          shiny::div(
            class = "epi-stack",
            shiny::actionButton(
              "insert_code", code_button_label,
              icon  = shiny::icon("terminal"),
              class = "btn-sm btn-primary"
            ),
            # Download: the table's dictionary, and those of all tables
            shiny::div(
              class = "dropdown",
              shiny::tags$button(
                id = "dict_download", type = "button",
                class = "btn btn-sm btn-primary dropdown-toggle",
                `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                `data-bs-popper-config` = '{"strategy":"fixed"}',
                shiny::icon("download"), "Download"
              ),
              shiny::tags$ul(
                class = "dropdown-menu",
                # Shiny enables a download link once it has its address, so
                # without Quarto the PDF's item is not a link
                shiny::tags$li(if (has_quarto) {
                  shiny::downloadLink("dict_pdf", "Dictionary as PDF",
                                      class = "dropdown-item")
                } else {
                  shiny::tags$span(class = "dropdown-item disabled",
                                   `aria-disabled` = "true",
                                   "Dictionary as PDF (needs Quarto)")
                }),
                shiny::tags$li(shiny::downloadLink(
                  "dict_html", "Dictionary as HTML", class = "dropdown-item"
                )),
                shiny::tags$li(shiny::actionLink(
                  "dict_excel", "Copy dictionary for Excel",
                  class = "dropdown-item"
                )),
                shiny::tags$li(shiny::tags$hr(class = "dropdown-divider")),
                shiny::tags$li(shiny::actionLink(
                  "dict_bulk", "Dictionaries of all tables\u2026",
                  class = "dropdown-item"
                )),
                shiny::tags$li(shiny::actionLink(
                  "dict_logo", "Dictionary logo\u2026",
                  class = "dropdown-item"
                ))
              )
            )
          )
        ),
        shiny::div(
          class = "options-row",
          shiny::tags$button(
            id = "values_open", type = "button",
            class = "btn btn-sm btn-primary",
            shiny::icon("angles-down"), "Expand value labels"
          ),
          shiny::tags$button(
            id = "values_close", type = "button",
            class = "btn btn-sm btn-primary",
            shiny::icon("angles-up"), "Collapse value labels"
          ),
          shiny::tags$button(
            id = "clear_rows", type = "button", disabled = NA,
            class = "btn btn-sm btn-primary action-button",
            shiny::icon("xmark"), "Clear selection"
          ),
          shiny::span(class = "browse-chosen",
                      shiny::textOutput("chosen_note", inline = TRUE))
        ),
        DT::dataTableOutput("col_table")
      )
    ),

    # The Profiler: one column's profile, or a cross tab of it by another,
    # or a profile of every column, read only when Profile is clicked. Its
    # panel holds the choice of one column or the whole table, the column,
    # By, the profile's summary and the columns left out. The lists draw
    # their options over the page, as the panel is only as tall as its
    # contents. Stop shows while the whole table is profiled.
    bslib::nav_panel(
      "Profiler", value = "profile",
      .epi_tab_settings(
        "profile_settings",
        shiny::tagList(
          shiny::radioButtons("profile_mode", "Profile",
                              choices = c("One column" = "column",
                                          "The whole table" = "table")),
          shiny::conditionalPanel(
            "input.profile_mode !== 'table'",
            shiny::selectizeInput("profile_column", "Column", choices = NULL,
                                  width = "100%",
                                  options = list(dropdownParent = "body")),
            shiny::selectizeInput("profile_by", "By", choices = c(None = ""),
                                  width = "100%",
                                  options = list(dropdownParent = "body"))
          ),
          shiny::conditionalPanel(
            "input.profile_mode !== 'table' && input.profile_by",
            shiny::radioButtons("profile_show", "Show",
                                choices = c("Rows" = "rows",
                                            "Per cent of row" = "row",
                                            "Per cent of column" = "column"))
          ),
          shiny::div(
            class = "epi-stack",
            shiny::actionButton("profile_go", "Profile",
                                icon = shiny::icon("chart-column"),
                                class = "btn-sm btn-primary"),
            shiny::conditionalPanel(
              "output.profile_running",
              shiny::actionButton("profile_stop", "Stop",
                                  icon = shiny::icon("stop"),
                                  class = "btn-sm btn-primary")
            )
          ),
          shiny::conditionalPanel("input.profile_mode !== 'table'",
                                  shiny::uiOutput("profile_summary")),
          shiny::tags$details(
            class = "browse-exclude",
            shiny::tags$summary(shiny::textOutput("profile_left_out",
                                                  inline = TRUE)),
            shiny::textAreaInput("profile_exclude", NULL, rows = 8,
                                 width = "100%", resize = "vertical"),
            shiny::uiOutput("profile_exclude_note"),
            shiny::div(
              class = "epi-stack",
              shiny::actionButton("profile_exclude_save", "Save the list",
                                  icon = shiny::icon("floppy-disk"),
                                  class = "btn-sm btn-primary"),
              shiny::actionButton("profile_exclude_reset",
                                  "Restore the defaults",
                                  icon = shiny::icon("rotate-left"),
                                  class = "btn-sm btn-primary")
            )
          )
        ),
        shiny::div(
          class = "options-row",
          shiny::span(class = "browse-chosen",
                      shiny::textOutput("profile_note", inline = TRUE))
        ),
        DT::dataTableOutput("profile_table")
      )
    ),

    # The table's description, notes and linked tables, apart from its
    # columns, as some tables have many notes
    bslib::nav_panel("Collection", value = "collection",
                     shiny::uiOutput("about")),

    # Var Finder: the columns of the database that a name, label or value
    # label finds. A click on one opens its table.
    bslib::nav_panel(
      "Var Finder", value = "search",
      .epi_tab_settings(
        "search_settings",
        shiny::tagList(
          shiny::textInput("search_text", "Find", width = "100%"),
          shiny::checkboxGroupInput(
            "search_in", "Look in",
            choices  = c("Column names" = "names",
                         "Variable labels" = "labels",
                         "Value labels" = "values"),
            selected = c("names", "labels", "values"), width = "100%"
          ),
          shiny::div(
            class = "epi-stack",
            shiny::tags$button(id = "search_go", type = "button",
                               class = "btn btn-sm btn-primary",
                               shiny::icon("magnifying-glass"), "Search")
          )
        ),
        shiny::div(
          class = "options-row",
          shiny::span(class = "browse-chosen",
                      shiny::textOutput("search_note", inline = TRUE))
        ),
        DT::dataTableOutput("search_table")
      )
    ),

    bslib::nav_panel("Help", value = "help", .browse_help()),

    # The actepir lockup at the right of the top bar. The app runs in a
    # browser tab, which closes it; no Done button is needed.
    bslib::nav_spacer(),
    bslib::nav_item(.epi_logo(class = "epi-logo"))
  )

  # ── Server ────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    no_sources <- data.frame(TABLE_SCHEMA = character(),
                             TABLE_NAME = character(),
                             Layout = character(), stringsAsFactors = FALSE)
    source_names <- function(s) {
      if (nrow(s)) paste0(s$TABLE_SCHEMA, ".", s$TABLE_NAME) else character()
    }
    or <- function(x, default) if (is.null(x)) default else x

    # ── Schemas of the database ────────────────────────────────────────────
    shiny::observeEvent(input$db, {
      schemas <- read(.browse_sql_schemas(input$db))$data
      choices  <- if (length(schemas$TABLE_SCHEMA)) schemas$TABLE_SCHEMA else "dbo"
      selected <- if ("dbo" %in% choices) "dbo" else choices[1]
      shiny::updateSelectInput(session, "schema",
                               choices = choices, selected = selected)
    })

    # ── Label tables of the database ───────────────────────────────────────
    sources <- shiny::reactive({
      shiny::req(input$db)
      s <- read(.lbl_sql_sources(input$db))
      if (is.null(s$data)) s$data <- no_sources
      s
    })

    shiny::observeEvent(sources(), {
      found <- source_names(sources()$data)
      current <- shiny::isolate(input$labels_source)
      shiny::updateSelectInput(
        session, "labels_source",
        choices  = c("Automatic" = "auto", stats::setNames(found, found)),
        selected = if (!is.null(current) && current %in% found) current else "auto"
      )
    })

    # The label tables looked in: all of them in order (Automatic), or the
    # one chosen. A choice left from another database counts as Automatic
    # until the list is updated.
    auto <- shiny::reactive({
      chosen <- or(input$labels_source, "auto")
      chosen == "auto" || !chosen %in% source_names(sources()$data)
    })
    active <- shiny::reactive({
      s <- sources()$data
      if (auto()) s else s[source_names(s) == input$labels_source, ,
                           drop = FALSE]
    })

    # ── Tables and views of the schema, with their dataset labels ──────────
    tables <- shiny::reactive({
      shiny::req(input$db, input$schema)
      read(.browse_sql_tables(input$db, input$schema))
    })

    dataset_labels <- shiny::reactive({
      shiny::req(input$db, input$schema)
      src <- active()
      if (!nrow(src)) return(list(labels = .lbl_dataset_labels(NULL)))
      r <- read(.lbl_sql_dataset_labels(input$db, src, input$schema))
      list(labels = .lbl_dataset_labels(r$data), error = r$error)
    })

    # A table to choose in another schema once its list arrives, as a click
    # in the Var Finder tab asks: list(schema, table)
    wanted <- shiny::reactiveVal(NULL)

    # The table chosen stays chosen while it is in the list; otherwise the
    # first table, or the first view
    shiny::observe({
      t <- tables()
      choices <- .browse_table_choices(t$data, dataset_labels()$labels)
      names <- unname(unlist(choices))
      current <- shiny::isolate(input$table)
      w <- shiny::isolate(wanted())
      if (!is.null(w) && identical(w$schema, shiny::isolate(input$schema))) {
        wanted(NULL)
        if (w$table %in% names) current <- w$table
      }
      selected <- if (length(current) && current %in% names) current else names[1]
      shiny::updateSelectizeInput(session, "table", choices = choices,
                                  selected = if (length(names)) selected else character())
    })

    # ── The table chosen ───────────────────────────────────────────────────
    # Only a table in the current list, so that a choice left from another
    # schema is never read
    chosen <- shiny::reactive({
      t <- tables()
      shiny::req(is.null(t$error), input$table)
      i <- match(input$table, t$data$TABLE_NAME)
      shiny::req(!is.na(i))
      list(db = input$db, schema = input$schema, table = input$table,
           type = if (t$data$TABLE_TYPE[i] == "VIEW") "view" else "table")
    })

    columns <- shiny::reactive({
      ch <- chosen()
      read(.browse_sql_columns(ch$db, ch$schema, ch$table))
    })

    # The table's labels other than value labels, from the first label table
    # looked in that holds rows for it: source is that label table (NULL
    # when none does), rows the normalised rows (.lbl_normalise())
    labels <- shiny::reactive({
      ch <- chosen()
      src <- active()
      if (!nrow(src)) return(list(source = NULL, rows = .lbl_normalise(NULL)))
      p <- read(.lbl_sql_pick(ch$db, src, ch$schema, ch$table))
      if (!is.null(p$error)) {
        return(list(source = NULL, rows = .lbl_normalise(NULL),
                    error = paste("The label tables could not be read:",
                                  p$error)))
      }
      i <- p$data$Source[1]
      if (is.na(i) || i == 0) {
        return(list(source = NULL, rows = .lbl_normalise(NULL)))
      }
      source <- src[i, , drop = FALSE]
      r <- read(.lbl_sql_rows(ch$db, source, ch$schema, ch$table))
      list(source = source, rows = .lbl_normalise(r$data),
           error = if (!is.null(r$error)) {
             paste0("The labels could not be read from ", source$TABLE_SCHEMA,
                    ".", source$TABLE_NAME, ": ", r$error)
           })
    })

    # Value labels, read only while they are shown
    values <- shiny::reactive({
      if (!isTRUE(input$show_values)) return(NULL)
      l <- labels()
      if (is.null(l$source)) return(NULL)
      ch <- chosen()
      r <- read(.lbl_sql_rows(ch$db, l$source, ch$schema, ch$table,
                              values = TRUE))
      list(rows = .lbl_normalise(r$data),
           error = if (!is.null(r$error)) {
             paste0("The value labels could not be read from ",
                    l$source$TABLE_SCHEMA, ".", l$source$TABLE_NAME, ": ",
                    r$error)
           })
    })

    lbl <- shiny::reactive({
      .lbl_table(labels()$rows, values()$rows)
    })

    # The table's value labels whether or not they are shown, for the
    # Profiler and the dictionary: those shown, or read for them
    all_values <- shiny::reactive({
      l <- labels()
      if (is.null(l$source)) return(list(rows = .lbl_normalise(NULL)))
      v <- values()
      if (!is.null(v)) return(v)
      ch <- chosen()
      r <- read(.lbl_sql_rows(ch$db, l$source, ch$schema, ch$table,
                              values = TRUE))
      list(rows = .lbl_normalise(r$data),
           error = if (!is.null(r$error)) {
             paste0("The value labels could not be read from ",
                    l$source$TABLE_SCHEMA, ".", l$source$TABLE_NAME, ": ",
                    r$error)
           })
    })

    # ── Info bar ───────────────────────────────────────────────────────────
    output$info_bar <- shiny::renderUI({
      shiny::req(input$db, input$schema)
      where <- paste0(input$db, ".", input$schema)
      failed <- function(...) {
        shiny::div(class = "info-bar epi-error", shiny::p(...))
      }
      t <- tables()
      if (!is.null(t$error)) {
        return(failed("The tables and views of ", where,
                      " could not be read: ", t$error))
      }
      if (!nrow(t$data)) {
        return(shiny::div(class = "info-bar", shiny::p(
          where, " has no tables or views that you can read."
        )))
      }
      ch <- chosen()
      name <- paste(ch$db, ch$schema, ch$table, sep = ".")
      cols <- columns()
      if (!is.null(cols$error)) {
        return(failed("The columns of ", name, " could not be read: ",
                      cols$error))
      }
      s <- sources()
      l <- labels()
      n <- nrow(cols$data)
      warnings <- c(
        if (!is.null(databases$error)) {
          paste("The databases could not be listed:", databases$error)
        },
        if (!is.null(s$error)) {
          paste0("The label tables of ", ch$db, " could not be listed: ",
                 s$error)
        },
        if (!is.null(dataset_labels()$error)) {
          paste("The dataset labels for the Table list could not be read:",
                dataset_labels()$error)
        },
        l$error,
        values()$error
      )
      shiny::div(
        class = "info-bar",
        shiny::p(shiny::strong(name), paste0(" (", ch$type, "), ", n,
                                             " column", if (n != 1) "s")),
        if (is.null(s$error) && (is.null(l$error) || !is.null(l$source))) {
          shiny::p(.browse_labels_note(l$source, active(), auto()))
        },
        lapply(warnings, function(w) shiny::p(class = "epi-warn", w))
      )
    })

    # ── The Collection tab and the columns ─────────────────────────────────
    output$about <- shiny::renderUI({
      ch <- chosen()
      .browse_about(lbl()$dataset, paste(ch$db, ch$schema, ch$table, sep = "."))
    })

    # The table the columns' table was last drawn for, as its rows chosen
    # belong to it until it is drawn again
    col_table_for <- shiny::reactiveVal(NULL)

    output$col_table <- DT::renderDataTable({
      cols <- columns()
      shiny::req(is.null(cols$error))
      col_table_for(chosen())
      show_values <- isTRUE(input$show_values) && !is.null(values())
      d <- .browse_columns(cols$data, lbl(), values = show_values)

      # Width hints to DataTables; the cells of the value labels and the
      # notes are tagged for their CSS and the selection guard
      widths <- c(`#` = 4, Column = 18, Description = 36, Type = 10,
                  `Value labels` = 22, Notes = 10)
      defs <- lapply(intersect(names(widths), names(d)), function(nm) {
        list(width = paste0(widths[[nm]], "%"),
             targets = match(nm, names(d)) - 1L)
      })
      lists <- c(`Value labels` = "browse-values", Notes = "browse-notes")
      for (nm in intersect(names(lists), names(d))) {
        defs <- c(defs, list(list(className = lists[[nm]],
                                  targets = match(nm, names(d)) - 1L)))
      }

      DT::datatable(
        d,
        escape    = FALSE,
        options   = list(
          paging         = FALSE,
          dom            = "ft",
          scrollY        = "var(--browse-table-height, 50vh)",
          scrollCollapse = TRUE,
          ordering       = TRUE,
          autoWidth      = FALSE,
          columnDefs     = defs
        ),
        rownames  = FALSE,
        selection = list(mode = "multiple", target = "row"),
        class     = "compact stripe hover",
        style     = "bootstrap5"
      )
    })

    # ── Columns chosen, and the insert / copy code button ──────────────────
    # The columns chosen, by their rows in the table, which are the rows of
    # columns() whatever the sorting. None while the table is not yet drawn
    # for the table chosen, as when another table is chosen while the
    # Columns tab is hidden. The line above the table counts them, and Clear
    # selection clears them through DT's proxy.
    chosen_columns <- function() {
      if (!identical(col_table_for(), chosen())) return(character())
      cols <- columns()$data
      rows <- input$col_table_rows_selected
      as.character(cols$col[rows[rows <= nrow(cols)]])
    }
    output$chosen_note <- shiny::renderText({
      n <- length(chosen_columns())
      if (!n) return("No columns chosen: the code reads them all.")
      paste0(n, " column", if (n != 1) "s", " chosen for the code.")
    })
    shiny::observeEvent(input$clear_rows, {
      DT::selectRows(DT::dataTableProxy("col_table"), NULL)
    })
    # The code names the label table chosen in Labels table, so that it
    # applies the labels the browser shows
    table_code <- function(ch) {
      label_table <- if (!auto()) {
        c(active()$TABLE_SCHEMA, active()$TABLE_NAME)
      }
      .browse_code(ch$db, ch$schema, ch$table, chosen_columns(),
                   mode    = or(input$insert_mode, "quickconnect"),
                   declare = isTRUE(input$declare_pkgs),
                   labels  = isTRUE(input$use_labels),
                   label_table = label_table)
    }
    shiny::observeEvent(input$insert_code, {
      ch <- tryCatch(chosen(), error = function(e) NULL)
      if (is.null(ch)) {
        shiny::showNotification("Choose a table first.", duration = 6,
                                type = "warning")
        return(invisible(NULL))
      }
      .epi_send_code(session, table_code(ch))
    })

    # Chooses table `table` of `schema` in the sidebar, through the Table
    # list once the schema's list arrives when the schema changes
    open_table <- function(schema, table) {
      if (identical(schema, input$schema)) {
        shiny::updateSelectizeInput(session, "table", selected = table)
      } else {
        wanted(list(schema = schema, table = table))
        shiny::updateSelectInput(session, "schema", selected = schema)
      }
    }

    # ── The Tables tab ─────────────────────────────────────────────────────
    # Every table and view of the schema, with when it was made and last
    # altered, its rows, its columns and its labels (.browse_tables_overview()).
    # The queries run only while the tab is shown.
    overview <- shiny::reactive({
      t <- tables()
      shiny::req(is.null(t$error))
      db <- input$db
      schema <- input$schema
      src <- active()
      details <- read(.browse_sql_details(db, schema))
      cols <- read(.browse_sql_schema_columns(db, schema))
      coverage <- if (nrow(src)) read(.browse_sql_coverage(db, src, schema))
      list(
        table = .browse_tables_overview(
          t$data, dataset_labels()$labels,
          or(details$data, data.frame(TableName = character(),
                                      Created = character(),
                                      Altered = character(),
                                      ApproxRows = numeric())),
          or(cols$data, data.frame(TABLE_NAME = character(),
                                   COLUMN_NAME = character())),
          coverage$data, src
        ),
        errors = c(
          if (!is.null(details$error)) {
            paste("The rows and dates could not be read:", details$error)
          },
          if (!is.null(cols$error)) {
            paste("The columns could not be read:", cols$error)
          },
          if (!is.null(coverage$error)) {
            paste("The label tables could not be read:", coverage$error)
          }
        )
      )
    })

    output$tables_note <- shiny::renderText({
      o <- overview()
      n <- nrow(o$table)
      where <- paste0(input$db, ".", input$schema)
      paste(c(
        if (n) {
          paste0(n, " table", if (n != 1) "s", " and view", if (n != 1) "s",
                 " in ", where, ". Click one to choose it.")
        } else {
          paste0(where, " has no tables or views that you can read.")
        },
        o$errors
      ), collapse = " ")
    })

    output$tables_table <- DT::renderDataTable({
      d <- overview()$table
      # Each heading's notes, shown when the pointer is over it
      notes <- c(
        Description = "The table's dataset label",
        Rows = paste("Approximate, from SQL Server's statistics; views have",
                     "none"),
        `Without var labels` = paste("Columns without a variable label in the",
                                 "label table that the labels come from"),
        `Labels from` = paste("The label table that the table's labels come",
                              "from, as the bar above the columns says"),
        Created = "When the table or view was made",
        `Structure changed` = paste("When its definition, such as its",
                                    "columns, last changed; not when its",
                                    "data changed")
      )
      heads <- htmltools::tags$table(htmltools::tags$thead(htmltools::tags$tr(
        lapply(names(d), function(n) {
          htmltools::tags$th(n, title = if (n %in% names(notes)) notes[[n]])
        })
      )))
      row <- match(shiny::isolate(input$table), d$Table)
      DT::formatRound(
        DT::datatable(
          d,
          container = heads,
          options   = list(
            paging         = FALSE,
            dom            = "ft",
            scrollY        = "var(--browse-table-height, 50vh)",
            scrollCollapse = TRUE,
            autoWidth      = FALSE
          ),
          rownames  = FALSE,
          selection = list(mode = "single", target = "row",
                           selected = if (!is.na(row)) row),
          class     = "compact stripe hover",
          style     = "bootstrap5"
        ),
        "Rows", digits = 0
      )
    })

    # A click on a table chooses it; the table chosen is marked in the tab
    tables_proxy <- DT::dataTableProxy("tables_table")
    shiny::observeEvent(input$tables_table_rows_selected, {
      table <- overview()$table$Table[input$tables_table_rows_selected]
      if (length(table) && !identical(table, input$table)) {
        open_table(input$schema, table)
      }
    })
    shiny::observe({
      shiny::req(identical(input$view, "tables"), input$table)
      row <- match(input$table, overview()$table$Table)
      if (!is.na(row) &&
          !identical(as.integer(shiny::isolate(input$tables_table_rows_selected)),
                     row)) {
        DT::selectRows(tables_proxy, row)
      }
    })

    # ── The Var Finder tab ─────────────────────────────────────────────────
    # The columns of the database that the text finds (.browse_search()),
    # from the label tables looked in. The results belong to one database
    # and label table choice, so they are cleared when either changes.
    found <- shiny::reactiveVal(NULL)
    run_search <- function(text) {
      term <- trimws(or(text, ""))
      problem <- if (!nzchar(term)) {
        "Type a name, or a word of a label, to find."
      } else if (!length(input$search_in)) {
        "Choose where to look."
      }
      if (!is.null(problem)) {
        shiny::showNotification(problem, duration = 6, type = "warning")
        return(invisible(NULL))
      }
      db <- input$db
      r <- tryCatch(
        shiny::withProgress(message = "Searching", value = 0.5, {
          .browse_search(query, db, term, input$search_in, active(), auto())
        }),
        error = function(e) list(error = conditionMessage(e))
      )
      found(c(r, list(db = db, term = term)))
    }
    shiny::observeEvent(input$search_run, run_search(input$search_run))
    shiny::observeEvent(list(input$db, input$labels_source), found(NULL),
                        ignoreInit = TRUE)

    output$search_note <- shiny::renderText({
      f <- found()
      if (is.null(f)) {
        return(paste0("Finds the columns of ", or(input$db, "the database"),
                      ", in every schema, whose names, variable labels or ",
                      "value labels hold the text typed, in the labels the ",
                      "browser reads. Click a column to open its table."))
      }
      if (!is.null(f$error)) return(paste("The search failed:", f$error))
      n <- nrow(f$results)
      paste0(
        if (n) {
          paste0(n, " column", if (n != 1) "s", " of ", f$db,
                 if (n != 1) " match \"" else " matches \"", f$term, "\".")
        } else {
          paste0("No column of ", f$db, " matches \"", f$term, "\".")
        },
        if (isTRUE(f$capped)) {
          paste(" Only the first 1,000 matches of each kind were read; more",
                "text finds fewer.")
        },
        if (n) " Click a column to open its table."
      )
    })

    output$search_table <- DT::renderDataTable({
      f <- found()
      shiny::req(f, is.null(f$error))
      # No search box of its own beside the tab's
      DT::datatable(
        f$results,
        options   = list(
          paging         = FALSE,
          dom            = "t",
          scrollY        = "var(--browse-table-height, 50vh)",
          scrollCollapse = TRUE,
          autoWidth      = FALSE
        ),
        rownames  = FALSE,
        selection = list(mode = "single", target = "row"),
        class     = "compact stripe hover",
        style     = "bootstrap5"
      )
    })

    # A click on a column opens its table on the Columns tab
    shiny::observeEvent(input$search_table_rows_selected, {
      r <- found()$results[input$search_table_rows_selected, , drop = FALSE]
      shiny::req(nrow(r) == 1)
      open_table(r$Schema, r$Table)
      bslib::nav_select("view", "columns")
    })

    # ── The Profiler tab ───────────────────────────────────────────────────
    # One column's profile (.browse_profile()), a cross tab of it by
    # another (.browse_crosstab()), or the whole table's profile
    # (.browse_table_profile()), read only when Profile is clicked, as it
    # reads every row of the table. Columns are left out of the lists and of
    # the whole table's profile when they are by themselves a primary key or
    # unique index, or match the list kept in the analyst's settings. The keys are read once the tab
    # has been shown, and then for each table chosen.
    profiler_seen <- shiny::reactiveVal(FALSE)
    shiny::observe({
      if (identical(input$view, "profile")) profiler_seen(TRUE)
    })

    # The list of patterns: the one kept, shown in its box. Save the list
    # uses the list in the box and keeps it for later sessions, when every
    # line is a regular expression; Restore the defaults goes back to them.
    exclude <- shiny::reactiveVal(.browse_exclude_read())
    exclude_invalid <- shiny::reactiveVal(character())
    shiny::updateTextAreaInput(session, "profile_exclude",
                               value = paste(shiny::isolate(exclude()),
                                             collapse = "\n"))
    shiny::observeEvent(input$profile_exclude_save, {
      typed <- .browse_exclude_parse(input$profile_exclude)
      exclude_invalid(typed$invalid)
      if (length(typed$invalid)) {
        shiny::showNotification("The list is not saved: see the lines named.",
                                duration = 6, type = "warning")
        return(invisible(NULL))
      }
      exclude(typed$patterns)
      .browse_exclude_save(typed$patterns)
      shiny::showNotification("The list is saved.", duration = 3,
                              type = "message")
    })
    shiny::observeEvent(input$profile_exclude_reset, {
      .browse_exclude_save(NULL)
      exclude(.browse_exclude_default)
      exclude_invalid(character())
      shiny::updateTextAreaInput(session, "profile_exclude",
                                 value = paste(.browse_exclude_default,
                                               collapse = "\n"))
    })

    keys <- shiny::reactive({
      shiny::req(profiler_seen())
      ch <- chosen()
      read(.browse_sql_keys(ch$db, ch$schema, ch$table))
    })
    # The table's columns, with why each left out is (NA for those kept)
    profiler_columns <- shiny::reactive({
      cols <- columns()$data
      shiny::req(!is.null(cols))
      cols$why <- .browse_excluded(as.character(cols$col), exclude(),
                                   keys()$data$ColumnName)
      cols
    })

    output$profile_left_out <- shiny::renderText({
      n <- sum(!is.na(profiler_columns()$why))
      paste0("Columns left out (", n, ")")
    })
    output$profile_exclude_note <- shiny::renderUI({
      cols <- profiler_columns()
      out <- cols[!is.na(cols$why), , drop = FALSE]
      shiny::tagList(
        if (length(exclude_invalid())) {
          shiny::p(class = "epi-note epi-error",
                   paste0("Not a regular expression, so the list was not saved: ",
                          paste(exclude_invalid(), collapse = ", ")))
        },
        if (!is.null(keys()$error)) {
          shiny::p(class = "epi-note epi-error",
                   paste("The table's keys could not be read:", keys()$error))
        },
        shiny::p(class = "epi-note", paste0(
          if (nrow(out)) {
            paste0("Left out: ", paste0(out$col, " (", out$why, ")",
                                        collapse = ", "), ". ")
          } else "None of this table's columns is left out. ",
          "Each line is a regular expression matched against each column's ",
          "name, with its case: ^ID begins with ID, ID$ ends with ID, and MRN ",
          "holds MRN anywhere. Start a line with (?i) to ignore case. Columns ",
          "that are by themselves a primary key or a unique index (key) are ",
          "left out too. Save the list to use it; it is kept for later ",
          "sessions."
        ))
      )
    })

    # The columns offered: those kept, the first chosen in the Columns tab
    # first; By offers those that can be grouped
    shiny::observe({
      cols <- profiler_columns()
      kept <- cols[is.na(cols$why), , drop = FALSE]
      names <- as.character(kept$col)
      pick <- intersect(chosen_columns(), names)
      current <- shiny::isolate(input$profile_column)
      selected <- if (length(pick)) {
        pick[1]
      } else if (length(current) && current %in% names) {
        current
      } else names[1]
      shiny::updateSelectizeInput(
        session, "profile_column", choices = names,
        selected = if (length(names)) selected else character()
      )
      by <- names[!tolower(kept$dtype) %in% .browse_uncomparable]
      current_by <- shiny::isolate(input$profile_by)
      shiny::updateSelectizeInput(
        session, "profile_by", choices = c(None = "", stats::setNames(by, by)),
        selected = if (length(current_by) && current_by %in% by) current_by else ""
      )
    })

    # One column's profile, and its cross tab by `by` unless that is NULL
    # or empty
    profiled <- shiny::reactiveVal(NULL)
    run_profile <- function(column, by) {
      ch <- chosen()
      cols <- columns()$data
      i <- match(column, cols$col)
      j <- if (nzchar(or(by, ""))) match(by, cols$col)
      problem <- if (is.na(i)) {
        "Choose a column to profile."
      } else if (length(j) && (is.na(j) || j == i)) {
        "Choose a By column other than the column profiled, or None."
      }
      if (!is.null(problem)) {
        shiny::showNotification(problem, duration = 6, type = "warning")
        return(invisible(NULL))
      }
      col <- as.character(cols$col[i])
      dtype <- cols$dtype[i]
      by <- if (length(j)) as.character(cols$col[j])
      shown <- .lbl_table(labels()$rows, all_values()$rows)
      r <- tryCatch(
        shiny::withProgress(message = "Profiling", value = 0.2, {
          summary <- query(.browse_sql_profile(ch$db, ch$schema, ch$table,
                                               col, dtype))
          shiny::incProgress(0.3)
          counts <- if (!tolower(dtype) %in% .browse_uncomparable) {
            query(.browse_sql_profile_counts(ch$db, ch$schema, ch$table,
                                             col, dtype))
          }
          shiny::incProgress(0.3)
          pairs <- if (!is.null(by) && !is.null(counts)) {
            query(.browse_sql_crosstab(ch$db, ch$schema, ch$table, col, dtype,
                                       by, cols$dtype[j]))
          }
          c(.browse_profile(summary, counts, shown$values[[tolower(col)]]),
            list(pairs = pairs))
        }),
        error = function(e) list(error = conditionMessage(e))
      )
      profiled(c(r, list(
        table = ch, column = col, dtype = dtype, by = by,
        label = unname(shown$label[tolower(col)]),
        by_label = if (!is.null(by)) unname(shown$label[tolower(by)]),
        vals = shown$values[[tolower(col)]],
        by_vals = if (!is.null(by)) shown$values[[tolower(by)]]
      )))
    }

    # The whole table, one column at a time: each step profiles a column
    # and asks for the next through invalidateLater(), so that the page
    # stays responsive and Stop is heard between columns. A column's counts
    # are read only when it has value labels, for its codes without labels.
    # The run holds the table, the columns kept, the labels, the results so
    # far and its progress bar; the finished profile is kept apart, so that
    # the steps do not draw the results again.
    whole_run <- shiny::reactiveVal(NULL)
    whole_step <- shiny::reactiveVal(0)
    whole_done <- shiny::reactiveVal(NULL)
    # Whether a run is going, for the condition that shows Stop; an output
    # function takes `...`, as Shiny passes it the output's name and session
    output$profile_running <- function(...) isTRUE(whole_run()$running)
    shiny::outputOptions(output, "profile_running", suspendWhenHidden = FALSE)

    run_whole <- function() {
      if (isTRUE(whole_run()$running)) return(invisible(NULL))
      cols <- profiler_columns()
      kept <- cols[is.na(cols$why), , drop = FALSE]
      if (!nrow(kept)) {
        shiny::showNotification("Every column of the table is left out.",
                                duration = 6, type = "warning")
        return(invisible(NULL))
      }
      run <- list(table = chosen(), cols = kept,
                  left_out = sum(!is.na(cols$why)),
                  lbl = .lbl_table(labels()$rows, all_values()$rows),
                  results = list(), started = Sys.time(), running = TRUE)
      run$progress <- shiny::Progress$new(session, min = 0, max = nrow(kept))
      run$progress$set(message = "Profiling the table", value = 0)
      whole_done(NULL)
      whole_run(run)
      whole_step(whole_step() + 1)
    }
    # Ends the run, keeping the columns profiled so far
    end_whole <- function(w, stopped = FALSE) {
      w$progress$close()
      w$running <- FALSE
      whole_run(w)
      whole_done(list(
        table = w$table, profile = .browse_table_profile(w$cols, w$results,
                                                         w$lbl),
        done = length(w$results), of = nrow(w$cols), left_out = w$left_out,
        stopped = stopped,
        seconds = as.numeric(difftime(Sys.time(), w$started, units = "secs"))
      ))
    }

    shiny::observe({
      whole_step()
      w <- shiny::isolate(whole_run())
      if (is.null(w) || !isTRUE(w$running)) return()
      ch <- shiny::isolate(tryCatch(chosen(), error = function(e) NULL))
      if (!identical(w$table, ch)) {
        end_whole(w, stopped = TRUE)
        return()
      }
      i <- length(w$results) + 1
      col <- as.character(w$cols$col[i])
      dtype <- w$cols$dtype[i]
      w$progress$set(value = i - 1,
                     detail = sprintf("column %d of %d: %s", i,
                                      nrow(w$cols), col))
      vals <- w$lbl$values[[tolower(col)]]
      w$results[[i]] <- tryCatch(
        list(
          summary = query(.browse_sql_profile(ch$db, ch$schema, ch$table,
                                              col, dtype)),
          counts = if (!is.null(vals) && nrow(vals) &&
                       !tolower(dtype) %in% .browse_uncomparable) {
            query(.browse_sql_profile_counts(ch$db, ch$schema, ch$table,
                                             col, dtype))
          }
        ),
        error = function(e) list(error = conditionMessage(e))
      )
      if (i >= nrow(w$cols)) {
        end_whole(w)
      } else {
        whole_run(w)
        shiny::invalidateLater(0)
      }
    })

    shiny::observeEvent(input$profile_stop, {
      w <- whole_run()
      if (isTRUE(w$running)) end_whole(w, stopped = TRUE)
    })

    shiny::observeEvent(input$profile_go, {
      if (identical(input$profile_mode, "table")) {
        run_whole()
      } else {
        run_profile(input$profile_column, input$profile_by)
      }
    })

    # The whole table's profile, while its table is the table chosen
    whole_profile <- shiny::reactive({
      w <- whole_done()
      ch <- tryCatch(chosen(), error = function(e) NULL)
      if (is.null(w) || !identical(w$table, ch)) NULL else w
    })

    # A click on a column of the whole table's profile profiles it alone
    shiny::observeEvent(input$profile_table_rows_selected, {
      shiny::req(identical(input$profile_mode, "table"))
      w <- whole_profile()
      col <- w$profile$table$Column[input$profile_table_rows_selected]
      shiny::req(length(col) == 1)
      shiny::updateRadioButtons(session, "profile_mode", selected = "column")
      shiny::updateSelectizeInput(session, "profile_column", selected = col)
      shiny::updateSelectizeInput(session, "profile_by", selected = "")
      run_profile(col, NULL)
    })

    # The profile of a column of the table chosen, or NULL
    profile <- shiny::reactive({
      p <- profiled()
      ch <- tryCatch(chosen(), error = function(e) NULL)
      if (is.null(p) || !identical(p$table, ch)) NULL else p
    })
    # Its cross tab, shown as Show asks, or NULL; NA when it has too many
    # pairs of values to show
    crosstab <- shiny::reactive({
      p <- profile()
      if (is.null(p) || is.null(p$pairs)) return(NULL)
      if (nrow(p$pairs) > 50000) return(NA)
      .browse_crosstab(p$pairs, p$vals, p$by_vals,
                       show = or(input$profile_show, "rows"))
    })

    output$profile_note <- shiny::renderText({
      if (identical(input$profile_mode, "table")) return(whole_note())
      p <- profile()
      if (is.null(p)) {
        return(paste("Choose a column, and a By column for a cross tab, then",
                     "click Profile. A profile reads every row of the table,",
                     "so a large table can take a while."))
      }
      name <- paste(p$table$db, p$table$schema, p$table$table, p$column,
                    sep = ".")
      if (!is.null(p$error)) {
        return(paste0("The profile of ", name, " could not be read: ", p$error))
      }
      named <- function(x, label) {
        paste0(x, if (length(label) && !is.na(label)) paste0(" (", label, ")"))
      }
      x <- crosstab()
      paste0(
        "Profile of ", named(name, p$label),
        if (!is.null(p$by)) paste0(" by ", named(p$by, p$by_label)),
        ".",
        if (is.null(p$counts)) {
          paste0(" Values of type ", p$dtype, " cannot be counted.")
        } else if (identical(x, NA)) {
          paste(" The two columns have more than 50,000 pairs of values, too",
                "many for a cross tab; choose a By column with fewer values.")
        } else if (!is.null(x)) {
          paste0(
            if (x$rows_capped) {
              paste(" The 1,000 most common values are listed; the rest are",
                    "in Other values.")
            },
            if (x$cols_capped) {
              paste0(" The 20 most common values of ", p$by, " are ",
                     "columns; the rest are in Other.")
            }
          )
        } else if (!p$complete) {
          " The 1,000 most common values are listed."
        }
      )
    })

    # The note on the whole table's profile: the column being read, or the
    # rows, the columns profiled and left out, and the time taken
    whole_note <- function() {
      r <- whole_run()
      if (isTRUE(r$running)) {
        return(sprintf(paste("Profiling column %d of %d. Stop keeps the",
                             "columns profiled so far."),
                       length(r$results) + 1, nrow(r$cols)))
      }
      w <- whole_profile()
      if (is.null(w)) {
        return(paste("Click Profile to profile every column of the table,",
                     "except those left out. Each column is read in turn over",
                     "every row of the table, so a large table can take a",
                     "while."))
      }
      big <- function(n) formatC(n, format = "f", digits = 0, big.mark = ",")
      secs <- round(w$seconds)
      failed <- if (!is.null(w$profile$table$Note)) {
        sum(nzchar(w$profile$table$Note))
      } else 0
      paste0(
        "Profile of ", paste(w$table$db, w$table$schema, w$table$table,
                             sep = "."),
        if (!is.na(w$profile$rows)) paste0(", ", big(w$profile$rows), " rows"),
        ": ",
        if (w$stopped) {
          sprintf("stopped after %d of %d columns", w$done, w$of)
        } else {
          paste0(w$done, " column", if (w$done != 1) "s")
        },
        if (w$left_out) paste0(", ", w$left_out, " left out"),
        ", in ",
        if (secs < 60) paste(secs, "s") else {
          sprintf("%d min %d s", secs %/% 60, secs %% 60)
        },
        ".",
        if (failed) {
          paste0(" ", failed, " column", if (failed != 1) "s", " could not ",
                 "be read; see Note.")
        },
        if (w$profile$capped) {
          paste(" Codes without labels are looked for among each column's",
                "1,000 most common values.")
        },
        " Click a column to profile it alone."
      )
    }

    output$profile_summary <- shiny::renderUI({
      p <- profile()
      shiny::req(p, is.null(p$error))
      htmltools::tags$table(
        class = "table table-sm browse-profile",
        htmltools::tags$tbody(lapply(seq_len(nrow(p$summary)), function(i) {
          htmltools::tags$tr(htmltools::tags$th(p$summary$Measure[i]),
                             htmltools::tags$td(p$summary$Value[i]))
        }))
      )
    })

    output$profile_table <- DT::renderDataTable({
      options <- list(paging = FALSE, dom = "ft", autoWidth = FALSE,
                      scrollY = "var(--browse-table-height, 50vh)",
                      scrollCollapse = TRUE)
      # The whole table: a row for each column, which a click profiles
      # alone; each heading's meaning in its title
      if (identical(input$profile_mode, "table")) {
        w <- whole_profile()
        shiny::req(w)
        t <- w$profile$table
        titles <- c(
          `Missing %` = paste("Rows where the column is NULL, as a per cent",
                              "of the table's rows"),
          `Blank %` = paste("Rows of text that is empty or only spaces, as a",
                            "per cent of the table's rows"),
          `Distinct values` = paste("The number of different values, not",
                                    "counting NULL"),
          Minimum = "The least value, with its value label",
          Maximum = "The greatest value, with its value label",
          `Value labels` = "The number of the column's value labels",
          `Codes without labels` = paste("The number of codes found without a",
                                         "value label, among the column's",
                                         "1,000 most common values"),
          Codes = paste("The codes found without a value label, the most",
                        "common first")
        )
        heads <- htmltools::tags$table(htmltools::tags$thead(
          htmltools::tags$tr(lapply(names(t), function(n) {
            htmltools::tags$th(n, title = if (n %in% names(titles)) titles[[n]])
          }))
        ))
        table <- DT::datatable(t, container = heads,
                               options = c(options, list(scrollX = TRUE)),
                               rownames = FALSE,
                               selection = list(mode = "single",
                                                target = "row"),
                               class = "compact stripe hover",
                               style = "bootstrap5")
        table <- DT::formatRound(table, c("Missing %", "Blank %"), digits = 1)
        return(DT::formatRound(table, c("Distinct values", "Value labels",
                                        "Codes without labels"), digits = 0))
      }
      p <- profile()
      shiny::req(p, is.null(p$error), p$counts)
      x <- crosstab()
      shiny::req(!identical(x, NA))
      if (is.null(x)) {
        table <- DT::datatable(p$counts, options = options, rownames = FALSE,
                               selection = "none",
                               class = "compact stripe hover",
                               style = "bootstrap5")
        return(DT::formatRound(DT::formatRound(table, "Rows", digits = 0),
                               "Per cent", digits = 1))
      }
      # A cross tab: each heading's code and label in its title; counts
      # whole, per cents to one decimal place
      heads <- htmltools::tags$table(htmltools::tags$thead(htmltools::tags$tr(
        lapply(seq_along(x$table), function(k) {
          n <- names(x$table)[k]
          title <- c(rep("", ncol(x$table) - length(x$titles)), x$titles)[k]
          htmltools::tags$th(n, title = if (nzchar(title)) title)
        })
      )))
      numbers <- names(x$table)[vapply(x$table, is.numeric, logical(1))]
      DT::formatRound(
        DT::datatable(x$table, container = heads,
                      options = c(options, list(scrollX = TRUE,
                                                ordering = FALSE)),
                      rownames = FALSE, selection = "none",
                      class = "compact stripe hover", style = "bootstrap5"),
        numbers, digits = if (identical(input$profile_show, "rows") ||
                              is.null(input$profile_show)) 0 else 1
      )
    })

    # ── Dictionary ─────────────────────────────────────────────────────────
    # The dictionary of the table chosen, from the labels shown, with its
    # value labels whether or not they are shown
    dictionary <- function() {
      ch <- chosen()
      cols <- columns()
      if (!is.null(cols$error)) stop(cols$error, call. = FALSE)
      l <- labels()
      v <- all_values()
      if (!is.null(v$error)) stop(v$error, call. = FALSE)
      .dict_assemble(ch, cols$data, rbind(l$rows, v$rows), l$source)
    }
    dict_download <- function(format) {
      shiny::downloadHandler(
        filename = function() paste0(chosen()$table, "_dictionary.", format),
        content  = function(file) {
          tryCatch(.dict_write(dictionary(), format, file), error = function(e) {
            shiny::showNotification(
              paste("The dictionary could not be made:", conditionMessage(e)),
              duration = 10, type = "error"
            )
            stop(e)
          })
        }
      )
    }
    output$dict_pdf  <- dict_download("pdf")
    output$dict_html <- dict_download("html")
    # The links are in a closed menu: they need their addresses while hidden
    for (id in c("dict_pdf", "dict_html")) {
      shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
    }

    # Copy dictionary for Excel: the dictionary as data, as an HTML table
    # that Excel pastes with its codes as written, and as text
    shiny::observeEvent(input$dict_excel, {
      shiny::req(chosen())
      x <- tryCatch(.dict_excel(.dict_flat(dictionary())),
                    error = function(e) e)
      if (inherits(x, "error")) {
        shiny::showNotification(
          paste("The dictionary could not be made:", conditionMessage(x)),
          duration = 10, type = "error"
        )
        return(invisible(NULL))
      }
      .epi_copy_text(session, x$text, "Dictionary copied to the clipboard",
                     html = x$html)
    })

    # Dictionary logo: sets or removes the logo through dictionary_logo(),
    # the one stored setting, and says what it warned of. The file name the
    # analyst chose stands for the upload's temporary path in its messages.
    logo_changed <- shiny::reactiveVal(0)
    logo_said <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$dict_logo, {
      logo_said(NULL)
      shiny::showModal(.browse_logo_dialog())
    })
    shiny::observeEvent(input$logo_file, {
      f <- input$logo_file
      warned <- character()
      r <- tryCatch(
        withCallingHandlers(
          suppressMessages(dictionary_logo(f$datapath)),
          warning = function(w) {
            warned <<- c(warned, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) e
      )
      logo_said(if (inherits(r, "error")) {
        list(error = gsub(f$datapath, f$name, conditionMessage(r),
                          fixed = TRUE))
      } else {
        list(set = f$name, warnings = warned)
      })
      logo_changed(logo_changed() + 1)
    })
    shiny::observeEvent(input$logo_reset, {
      suppressMessages(dictionary_logo(remove = TRUE))
      logo_said(NULL)
      logo_changed(logo_changed() + 1)
    })
    output$logo_shown <- shiny::renderUI({
      logo_changed()
      logo <- .dict_logo()
      info <- if (identical(logo$ext, "png")) .png_info(logo$path)
      said <- logo_said()
      shiny::tagList(
        shiny::div(class = "browse-logo",
                   if (!is.null(logo)) {
                     shiny::tags$img(src = .dict_logo_src(logo), alt = logo$alt)
                   }),
        shiny::p(class = "epi-note", if (is.null(info)) {
          "The dictionaries show the actepir logo."
        } else {
          sprintf("The dictionaries show your logo, %s by %s pixels.",
                  info$width, info$height)
        }),
        if (!is.null(said$error)) {
          shiny::p(class = "epi-note epi-error", said$error)
        },
        lapply(said$warnings, function(w) {
          shiny::p(class = "epi-note epi-warn", w)
        })
      )
    })

    # The dictionaries of every table and view with labels in the schema
    # or the database, written to a folder on this computer, from the label
    # table read. The folder is kept for the next time.
    bulk_dir <- shiny::reactiveVal(file.path(getwd(), "dictionaries"))
    shiny::observeEvent(input$dict_bulk, {
      shiny::req(input$db, input$schema)
      shiny::showModal(.browse_bulk_dialog(input$db, input$schema,
                                           !is.null(.dict_quarto()), bulk_dir()))
    })
    # The folder list follows the Folder box, once typing pauses, as a
    # network folder can be slow to list; a click in it fills the box
    bulk_typed <- shiny::debounce(shiny::reactive(input$bulk_dir), 300)
    output$bulk_folders <- shiny::renderUI({
      .browse_folder_ui(or(bulk_typed(), bulk_dir()))
    })
    shiny::observeEvent(input$bulk_into, {
      shiny::updateTextInput(session, "bulk_dir", value = input$bulk_into)
    })
    shiny::observeEvent(input$bulk_write, {
      formats <- intersect(c("html", if (!is.null(.dict_quarto())) "pdf"),
                           input$bulk_formats)
      folder <- trimws(or(input$bulk_dir, ""))
      problem <- if (!length(formats)) {
        "Choose HTML, PDF or both."
      } else if (!nzchar(folder)) {
        "Give a folder for the dictionaries."
      } else if (!dir.exists(folder) &&
                 !dir.create(folder, recursive = TRUE, showWarnings = FALSE)) {
        paste0("The folder ", folder, " could not be made.")
      }
      if (!is.null(problem)) {
        shiny::showNotification(problem, duration = 8, type = "warning")
        return(invisible(NULL))
      }
      bulk_dir(folder)
      db <- input$db
      schema <- if (identical(input$bulk_scope, "db")) NULL else input$schema
      where <- paste(c(db, schema), collapse = ".")
      label <- if (!auto()) {
        c(db = db, schema = active()$TABLE_SCHEMA, table = active()$TABLE_NAME)
      }
      shiny::removeModal()
      result <- tryCatch(
        shiny::withProgress(message = "Writing dictionaries", value = 0, {
          listed <- .dict_tables(query, db, schema, label)
          done <- .dict_write_all(
            query, .dict_targets(listed, db, label), formats, folder,
            by_schema = is.null(schema), keep_going = TRUE,
            step = function(i, n) {
              shiny::incProgress(1 / n, detail = paste(i, "of", n))
            }
          )
          list(listed = listed, done = done)
        }),
        error = function(e) {
          shiny::showNotification(
            paste("The dictionaries could not be written:", conditionMessage(e)),
            duration = 10, type = "error"
          )
          NULL
        }
      )
      if (!is.null(result)) {
        shiny::showModal(.browse_bulk_done(result$listed, result$done, formats,
                                           folder, where))
      }
    })
    shiny::observeEvent(input$bulk_open, .browse_open_dir(bulk_dir()))

    # ── Problem report ─────────────────────────────────────────────────────
    # The bug button sends what the browser knows (bug_client); the report
    # adds the browser's state, the settings and the R session
    report <- function(what) {
      ch <- tryCatch(chosen(), error = function(e) NULL)
      s <- tryCatch(sources(), error = function(e) NULL)
      l <- if (!is.null(ch)) tryCatch(labels(), error = function(e) NULL)
      found <- if (is.null(s)) "not read" else if (!is.null(s$error)) {
        paste("not read:", s$error)
      } else if (!nrow(s$data)) "none" else {
        paste(source_names(s$data), collapse = ", ")
      }
      errors <- c(
        databases$error,
        if (!is.null(ch)) c(tables()$error, columns()$error, l$error,
                            tryCatch(values()$error, error = function(e) NULL))
      )
      ids <- c("view", "db", "schema", "table", "labels_source",
               "show_values", "insert_mode", "declare_pkgs", "use_labels",
               "col_table_rows_selected", "profile_mode", "profile_column",
               "profile_by", "profile_show")
      .epi_problem_report(
        "Dataset Browser", "Browser", what,
        state = c(
          "Databases listed" = if (!is.null(databases$error)) "none read"
            else length(db_choices),
          "Label tables" = found,
          "Labels" = if (is.null(ch)) "no table chosen" else
            if (is.null(l)) "not read" else
              .browse_labels_note(l$source, active(), auto()),
          "Errors" = if (length(errors)) paste(errors, collapse = "; ")
            else "none",
          "Code button" = code_button_label
        ),
        inputs = stats::setNames(lapply(ids, function(id) input[[id]]), ids),
        code = if (!is.null(ch)) table_code(ch),
        session = .epi_session_report(input$bug_client)
      )
    }

    .epi_report_server(input, output, session, "Dataset Browser", report,
                       paste("the settings and the names of the database,",
                             "table and columns chosen, but no data or labels"))

    # In this R session the app holds the console, so closing its browser
    # tab stops it; a background app keeps running for the next opening
    if (in_rstudio) session$onSessionEnded(shiny::stopApp)

  }

  # App-level cleanup. A graceful disconnect only happens in foreground mode:
  # in a background worker, dbDisconnect() on a wedged ODBC connection can
  # hang R shutdown, so cleanup there is left to process exit instead.
  shiny::shinyApp(
    ui      = ui,
    server  = server,
    onStart = function() {
      if (in_rstudio && !is.null(db)) {
        shiny::onStop(db$disconnect)
      }
    }
  )

}
