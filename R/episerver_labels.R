# The label tables of an EpiServer database and the rows they hold for a
# table. Two layouts are read:
# - DataProperties (ref.DataProperties, the label table sqldta reads): rows
#   filed by TableSchema and TableName, with ColumnName, PropertyType,
#   PropertyCode and PropertyText;
# - DataLabels (ref.DataLabels, dbo.DataLabels and other tables whose name
#   holds DataLabels): rows filed by Dataset, the table's name alone, with
#   VarName, LabelType, DataCode, LabelName and DataType.
# A table takes its labels from the first label table, in the order of
# .lbl_sql_sources(), that holds rows for it, so that a table keeps its labels
# while the labels move from DataLabels to DataProperties. Rows are read in
# the shape of DataProperties and normalised by .lbl_normalise().

# Columns each layout must have for its label table to be offered. DataType
# is required of DataLabels tables, as before DataProperties, but not read.
#' @noRd
.lbl_layouts <- list(
  properties = c("TableSchema", "TableName", "ColumnName", "PropertyType",
                 "PropertyCode", "PropertyText"),
  labels     = c("Dataset", "VarName", "LabelType", "LabelName", "DataCode",
                 "DataType")
)

# Row types, by their name in either layout in lower case, as read: the names
# of DataProperties, the 1.0.0 names of sqldta (DataLabel) and the names of
# the DataLabels layout (Opt, Var, NoteVar, NoteDta, DtaLabel). Other types,
# such as VariableFormat (Stata display formats), are not read.
#' @noRd
.lbl_types <- c(
  valuelabel = "ValueLabel",    opt = "ValueLabel",
  variablelabel = "VariableLabel", var = "VariableLabel",
  variablenote = "VariableNote",  notevar = "VariableNote",
  datasetnote = "DatasetNote",    notedta = "DatasetNote",
  datasetlabel = "DatasetLabel",  datalabel = "DatasetLabel",
  dtalabel = "DatasetLabel",
  link = "Link"
)


# ── SQL ─────────────────────────────────────────────────────────────────────

# An identifier in brackets, or a name of several parts, each in brackets
#' @noRd
.sql_ident <- function(...) {
  paste0("[", gsub("]", "]]", c(...), fixed = TRUE), "]", collapse = ".")
}

# A string literal; N'...' when unicode is TRUE
#' @noRd
.sql_quote <- function(x, unicode = FALSE) {
  paste0(if (unicode) "N", "'", gsub("'", "''", x, fixed = TRUE), "'")
}

# The label tables of database db that have every column of their layout,
# tables or views, in the order a table's labels are looked for:
# ref.DataProperties, then tables named DataLabels, then DataLabelsNew, then
# others whose name holds DataLabels, each in ref before other schemas.
# Returns TABLE_SCHEMA, TABLE_NAME and Layout ("properties" or "labels").
#' @noRd
.lbl_sql_sources <- function(db) {
  has_all <- function(cols) {
    sprintf(
      "(SELECT COUNT(DISTINCT c.COLUMN_NAME)
          FROM %s.INFORMATION_SCHEMA.COLUMNS c
          WHERE c.TABLE_SCHEMA = t.TABLE_SCHEMA
            AND c.TABLE_NAME   = t.TABLE_NAME
            AND c.COLUMN_NAME IN (%s)) = %d",
      .sql_ident(db), paste(.sql_quote(cols), collapse = ", "), length(cols)
    )
  }
  sprintf(
    "SELECT t.TABLE_SCHEMA, t.TABLE_NAME,
       CASE WHEN t.TABLE_NAME = 'DataProperties' THEN 'properties'
            ELSE 'labels' END AS Layout
     FROM %s.INFORMATION_SCHEMA.TABLES t
     WHERE (t.TABLE_SCHEMA = 'ref' AND t.TABLE_NAME = 'DataProperties'
            AND %s)
        OR (t.TABLE_NAME LIKE '%%DataLabels%%'
            AND %s)
     ORDER BY
       CASE WHEN t.TABLE_NAME = 'DataProperties' THEN 0
            WHEN t.TABLE_NAME = 'DataLabels'     THEN 1
            WHEN t.TABLE_NAME = 'DataLabelsNew'  THEN 2
            ELSE 3 END,
       CASE WHEN t.TABLE_SCHEMA = 'ref' THEN 0 ELSE 1 END,
       t.TABLE_SCHEMA, t.TABLE_NAME",
    .sql_ident(db), has_all(.lbl_layouts$properties),
    has_all(.lbl_layouts$labels)
  )
}

# The condition that selects the rows of table schema.table from a label
# table of the layout given. DataProperties rows are compared with Unicode
# literals and DataLabels rows with plain ones, as sqldta does, so that an
# index on either can serve the comparison.
#' @noRd
.lbl_where <- function(layout, schema, table) {
  if (layout == "properties") {
    paste("TableSchema =", .sql_quote(schema, TRUE),
          "AND TableName =", .sql_quote(table, TRUE))
  } else {
    paste("Dataset =", .sql_quote(table))
  }
}

# Which of the label tables `sources` (from .lbl_sql_sources()) is the first
# to hold rows for table schema.table: Source is its row number in
# `sources`, or 0 when none does
#' @noRd
.lbl_sql_pick <- function(db, sources, schema, table) {
  whens <- vapply(seq_len(nrow(sources)), function(i) {
    sprintf("WHEN EXISTS (SELECT 1 FROM %s WHERE %s) THEN %d",
            .sql_ident(db, sources$TABLE_SCHEMA[i], sources$TABLE_NAME[i]),
            .lbl_where(sources$Layout[i], schema, table), i)
  }, character(1))
  paste0("SELECT CASE\n  ", paste(whens, collapse = "\n  "),
         "\n  ELSE 0 END AS Source")
}

# The rows of table schema.table in one label table (a row of `sources`),
# with the columns named as in DataProperties. values chooses value labels
# (TRUE) or every other type (FALSE), as a table can have many thousands of
# value labels.
#' @noRd
.lbl_sql_rows <- function(db, source, schema, table, values = FALSE) {
  properties <- source$Layout == "properties"
  columns <- if (properties) {
    "ColumnName, PropertyType, PropertyCode, PropertyText"
  } else {
    paste("VarName AS ColumnName, LabelType AS PropertyType,",
          "DataCode AS PropertyCode, LabelName AS PropertyText")
  }
  type <- if (properties) "PropertyType" else "LabelType"
  value_types <- if (properties) {
    .sql_quote("ValueLabel", TRUE)
  } else {
    paste(.sql_quote(c("Opt", "ValueLabel")), collapse = ", ")
  }
  sprintf(
    "SELECT %s
     FROM %s
     WHERE %s
       AND %s %sIN (%s)",
    columns,
    .sql_ident(db, source$TABLE_SCHEMA, source$TABLE_NAME),
    .lbl_where(source$Layout, schema, table),
    type, if (values) "" else "NOT ", value_types
  )
}

# The dataset labels that the label tables `sources` hold for the tables of
# schema, as Source (the row number in `sources`), TableName and Label. Each
# label table but the last also gives every table it holds rows for, with
# no Label, so that .lbl_dataset_labels() can take each table's label from
# the first label table that holds rows for it. Names and labels take the
# database's collation, so that label tables of different collations can be
# read together.
#' @noRd
.lbl_sql_dataset_labels <- function(db, sources, schema) {
  parts <- vapply(seq_len(nrow(sources)), function(i) {
    from <- .sql_ident(db, sources$TABLE_SCHEMA[i], sources$TABLE_NAME[i])
    properties <- sources$Layout[i] == "properties"
    in_schema <- paste("TableSchema =", .sql_quote(schema, TRUE))
    labels <- if (properties) {
      sprintf(
        "SELECT %d AS Source, TableName COLLATE DATABASE_DEFAULT AS TableName,
           PropertyText COLLATE DATABASE_DEFAULT AS Label
         FROM %s
         WHERE %s
           AND PropertyType IN (N'DatasetLabel', N'DataLabel')",
        i, from, in_schema)
    } else {
      sprintf(
        "SELECT %d AS Source, Dataset COLLATE DATABASE_DEFAULT AS TableName,
           LabelName COLLATE DATABASE_DEFAULT AS Label
         FROM %s
         WHERE LabelType IN ('DtaLabel', 'DatasetLabel', 'DataLabel')",
        i, from)
    }
    held <- if (i < nrow(sources)) {
      sprintf(
        "SELECT DISTINCT %d, %s COLLATE DATABASE_DEFAULT,
           CAST(NULL AS nvarchar(1))
         FROM %s%s",
        i, if (properties) "TableName" else "Dataset", from,
        if (properties) paste0("\n         WHERE ", in_schema) else "")
    }
    paste(c(labels, held), collapse = "\nUNION ALL\n")
  }, character(1))
  paste(parts, collapse = "\nUNION ALL\n")
}


# ── Rows ────────────────────────────────────────────────────────────────────

# Label rows in one shape: Column (as stored), Key (the column in lower case,
# for matching, as SQL Server matches names), Type (a name of .lbl_types),
# Code and Text, trimmed, with rows of other types and rows without text
# dropped. Value labels keep the first row for each column and code, and
# variable and dataset labels the first for each column.
#' @noRd
.lbl_normalise <- function(rows) {
  empty <- data.frame(Column = character(), Key = character(),
                      Type = character(), Code = character(),
                      Text = character(), stringsAsFactors = FALSE)
  if (is.null(rows) || !nrow(rows)) return(empty)
  text <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x)] <- ""
    x
  }
  out <- data.frame(
    Column = text(rows$ColumnName),
    Type   = unname(.lbl_types[tolower(text(rows$PropertyType))]),
    Code   = text(rows$PropertyCode),
    Text   = text(rows$PropertyText),
    stringsAsFactors = FALSE
  )
  out$Key <- tolower(out$Column)
  out <- out[!is.na(out$Type) & nzchar(out$Text), , drop = FALSE]
  single <- out$Type %in% c("VariableLabel", "DatasetLabel")
  key <- paste(out$Type, out$Key,
               ifelse(out$Type == "ValueLabel", out$Code, ""),
               ifelse(single | out$Type == "ValueLabel", "", out$Text),
               sep = "\r")
  out <- out[!duplicated(key), c("Column", "Key", "Type", "Code", "Text")]
  rownames(out) <- NULL
  out
}

# Each table's dataset label from the rows of .lbl_sql_dataset_labels(): the
# label in the first label table that holds rows for the table, trimmed.
# Named by the table's name in lower case, as SQL Server matches names;
# tables without a label are left out.
#' @noRd
.lbl_dataset_labels <- function(rows) {
  if (is.null(rows) || !nrow(rows)) {
    return(stats::setNames(character(), character()))
  }
  key <- tolower(trimws(as.character(rows$TableName)))
  label <- trimws(as.character(rows$Label))
  label[is.na(label)] <- ""
  first <- stats::ave(rows$Source, key, FUN = min)
  keep <- rows$Source == first & nzchar(label)
  out <- stats::setNames(label[keep], key[keep])
  out[!duplicated(names(out))]
}

# The order of codes: numbers first, by value, then other codes as text
#' @noRd
.lbl_code_order <- function(code) {
  num <- suppressWarnings(as.numeric(code))
  order(is.na(num), num, code, method = "radix")
}

# The labels of a table from normalised rows, with its value labels in
# `values` when they were read separately: per column (Key) its label, notes
# and value labels, and for the table its label, notes and linked tables.
# Notes with a code come first, in the order of their codes (numerically
# when every code of the column's notes is a number), then the others in the
# order of the rows, as sqldta applies them.
#' @noRd
.lbl_table <- function(rows, values = NULL) {
  by_type <- function(df, type) df[df$Type == type, , drop = FALSE]
  in_order <- function(df) {
    num <- suppressWarnings(as.numeric(df$Code))
    coded <- nzchar(df$Code)
    numeric <- all(!is.na(num[coded]))
    df$Text[order(!coded, if (numeric) num else df$Code, seq_len(nrow(df)),
                  method = "radix")]
  }

  labels <- by_type(rows, "VariableLabel")
  notes  <- by_type(rows, "VariableNote")
  vals   <- by_type(if (is.null(values)) rows else values, "ValueLabel")
  vals   <- vals[.lbl_code_order(vals$Code), , drop = FALSE]
  links  <- by_type(rows, "Link")

  list(
    label  = stats::setNames(labels$Text, labels$Key),
    notes  = lapply(split(notes, notes$Key), in_order),
    values = split(vals[c("Code", "Text")], vals$Key),
    dataset = list(
      label = by_type(rows, "DatasetLabel")$Text[1],
      notes = in_order(by_type(rows, "DatasetNote")),
      links = data.frame(Key = links$Column, Table = links$Text,
                         stringsAsFactors = FALSE)
    )
  )
}
