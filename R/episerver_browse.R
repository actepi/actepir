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
#' open and close, and the columns, the dataset notes and the help in tabs
#' on the top bar. The
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
#' columns, and its dataset label, dataset notes and linked tables. They are
#' read from the label tables of the table's database, which may be tables
#' or views:
#' * `ref.DataProperties`, in the layout that sqldta reads (`TableSchema`,
#'   `TableName`, `ColumnName`, `PropertyType`, `PropertyCode`,
#'   `PropertyText`), with rows filed by the table's schema and name;
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
#' The *Collection* tab holds the table's description (its dataset label),
#' its notes and its linked tables, apart from the columns, as some tables
#' have many notes.
#'
#' **Code**
#'
#' The code button gives R code that reads the table, with a `select()` of
#' the columns chosen. In foreground mode it is labelled *Insert Code* and
#' inserts the code at the cursor with `rstudioapi::insertText()`. A
#' background process has no connection to the RStudio editor, so in
#' background mode it is labelled *Copy Code* and copies the code to the
#' clipboard. The *Code* section sets the form of the code:
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
#' *Download*, beside the code button, gives the table's data dictionary as a
#' PDF or a standalone HTML file, as [episerver_dictionary()] writes them,
#' from the labels of the label table read. The PDF needs the Quarto that
#' comes with RStudio; without it, its item is disabled. *Dictionaries of all
#' tables*, in the same menu, writes the dictionaries of every table and view
#' with labels in the schema, or in the whole database, to a folder typed or
#' found in the dialog's list of folders, as HTML, PDF or both.
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
# notes when any column has notes. `lbl` is from .lbl_table(); columns are
# matched to its labels in lower case, as SQL Server matches names.
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
      paste(esc(n), collapse = "<br>")
    }, character(1))
  }
  out
}

# The Collection tab: the table's dataset label (its description), notes
# and linked tables (dataset, from .lbl_table()), or a line saying it has
# none
#' @noRd
.browse_about <- function(dataset) {
  label <- dataset$label
  has_label <- length(label) && !is.na(label)
  links <- dataset$links
  if (!has_label && !length(dataset$notes) && !nrow(links)) {
    return(shiny::div(
      class = "browse-about",
      shiny::p("This table has no description, notes or linked tables.")
    ))
  }
  shiny::div(
    class = "browse-about",
    if (has_label) shiny::h5(label),
    lapply(dataset$notes, shiny::p),
    if (nrow(links)) {
      shiny::p(paste0(
        "Linked tables: ",
        paste0(links$Table, " (on ", links$Key, ")", collapse = ", "), "."
      ))
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
      "Choose a database, a schema and a table or view in the Dataset ",
      "section. The Table list holds the tables and views of the schema, ",
      "each with its dataset label. To find a table by its name or its ",
      "label, click the list, delete the table shown with Backspace and ",
      "type. SQL Server's system databases, databases whose name begins ",
      "with an underscore, and the label tables are not listed."
    ),

    shiny::h5("Labels"),
    shiny::p(
      "A table's labels are read from the label tables of its database, ",
      "which may be tables or views, in this order:"
    ),
    shiny::tags$ol(
      shiny::tags$li(
        code("ref.DataProperties"), ", which the Stata command sqldta also ",
        "reads, with rows filed by the table's schema and name;"
      ),
      shiny::tags$li(
        "tables named ", code("DataLabels"), ", then ",
        code("DataLabelsNew"), ", then others whose name holds DataLabels, ",
        "each in the ", code("ref"), " schema before others, with rows ",
        "filed by the table's name alone."
      )
    ),
    shiny::p(
      "With Labels table set to Automatic, each table takes its labels ",
      "from the first label table that holds rows for it, and the bar ",
      "above the columns says which. Choosing a label table reads the ",
      "labels from it alone. A label table is offered only if it has every ",
      "column of its layout."
    ),
    shiny::p(
      "The Description of a column is its variable label. Show value labels, ",
      "on by default, adds its value labels, which are read only while ",
      "shown; Expand value labels and Collapse value labels above the table ",
      "open and close every list. Notes on columns are shown when any ",
      "column has them, those with a code first, in code order."
    ),
    shiny::p(
      "The Collection tab holds the table's description (its dataset ",
      "label), its notes and its linked tables."
    ),

    shiny::h5("Choosing columns"),
    shiny::p(
      "Click rows of the table to choose columns for the code, and click a ",
      "row again to drop it. Rows chosen are shaded, with a bar at their ",
      "left, and the line above the table counts them. Clear selection ",
      "drops them all. The search box and the headings search and sort the ",
      "table without changing the columns chosen."
    ),

    shiny::h5("Code"),
    shiny::p(
      "Copy Code copies R code that reads the table, with a ",
      code("select()"), " of the columns chosen. When the browser runs in ",
      "the R session (", code("episerver_browse(background = FALSE)"),
      "), the button is Insert Code and puts the code at the cursor. The ",
      "Code section sets its form:"
    ),
    shiny::tags$ul(
      shiny::tags$li(
        "quickconnect: a pipe from ", code("episerver_quickconnect()"), ";"
      ),
      shiny::tags$li(
        "connect: a connection from ", code("episerver_connect()"),
        " and the table from ", code("episerver_lazytable()"), ";"
      ),
      shiny::tags$li("none: the pipe alone, from a lazy table ",
                     code("tb"), " made earlier.")
    ),
    shiny::p(
      "Declare packages adds ", code("actepir::"), " and ", code("dplyr::"),
      " to the calls, or ", code("library()"), " calls to the connect form. ",
      "Import with labels collects the data with ",
      code("collect_withlabels()"), ", which applies the variable and value ",
      "labels of the label table the browser reads. When Labels table names ",
      "a label table, the code names it too."
    ),

    shiny::h5("Dictionary"),
    shiny::p(
      "Download, beside the code button, gives the table's data dictionary ",
      "as a PDF or an HTML file: its description, notes and linked tables, ",
      "and its columns with the labels from the label table read, value ",
      "labels included whether or not they are shown. The PDF shows the ",
      "first 50 value labels of a column; the HTML file shows them all, with ",
      "a search box, and can be sent on as it is. The PDF needs the Quarto ",
      "that comes with RStudio. ", code("episerver_dictionary()"),
      " gives the same files in a script."
    ),
    shiny::p(
      "Dictionaries of all tables, in the same menu, writes the dictionaries ",
      "of every table and view with labels in the schema, or in the whole ",
      "database, to a folder on this computer, as HTML, PDF or both. Tables ",
      "and views without labels are left out. A PDF takes a second or two, ",
      "so a large database can take some minutes."
    ),
    shiny::p(
      "Type or paste the folder, or find it in the list below the box: click ",
      "a folder to open it, or Up, Home or Working directory. A folder that ",
      "does not exist is made, so a new folder's name can be typed at the end."
    ),

    shiny::h5("Problems"),
    shiny::p(paste0(
      "The bug button at the foot of the sidebar prepares a report to send ",
      "to ", .epi_contact, ". It holds the settings and the names of the ",
      "database, table and columns chosen, but no data or labels."
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
      # columns chosen, the value labels in the columns' table, whose list
      # scrolls once open, and the folder list of Dictionaries of all tables
      shiny::tags$style(shiny::HTML("
        .browse-label { color: #777; }
        .selectize-dropdown .option.selected .browse-label { color: inherit;
                                                             opacity: 0.8; }
        .browse-about { max-width: 900px; }
        .browse-about h5 { color: var(--epi-primary); font-weight: 600;
                           margin: 4px 0 6px 0; }
        .browse-about p { margin-bottom: 6px; }
        .browse-chosen { color: #555; }
        td.browse-values details summary { cursor: pointer;
                                           color: var(--epi-accent); }
        td.browse-values details div { max-height: 150px; overflow-y: auto; }
        .browse-folders { max-height: 220px; overflow-y: auto;
                          overscroll-behavior: contain;
                          margin-bottom: var(--epi-gap); }
        .browse-folders .list-group-item { font-size: var(--epi-font-box);
          padding: var(--epi-box-pad-y) var(--epi-box-pad-x); }
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

      # Clicks on the value labels open and scroll their list without
      # choosing the row. DT's selection handler may be bound to mousedown or
      # pointer events rather than click, so the guard stops the whole
      # pointer-event family in the capture phase, before any delegated
      # handler. stopPropagation() leaves the browser's default actions, so
      # the <details> toggle, scrolling and text selection still work.
      shiny::tags$script(shiny::HTML("
        ['pointerdown', 'mousedown', 'mouseup', 'click'].forEach(function(type) {
          document.addEventListener(type, function(e) {
            var t = e.target;
            if (t && t.closest && t.closest('#col_table td.browse-values')) {
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

      # The columns' table fits the window below its top, so that it needs
      # no page scrolling: a script sets --browse-table-height, which caps
      # its scrolling body (scrollY)
      shiny::tags$script(shiny::HTML("
        (function() {
          function fit() {
            var body = document.querySelector('#col_table .dataTables_scrollBody');
            if (!body || !body.offsetParent) return;
            var top = body.getBoundingClientRect().top;
            for (var el = body.parentElement; el; el = el.parentElement) {
              top += el.scrollTop;
            }
            var room = Math.max(window.innerHeight - top - 16, 200);
            document.getElementById('col_table')
              .style.setProperty('--browse-table-height', room + 'px');
          }
          function soon() { setTimeout(fit, 0); }
          $(window).on('resize', soon);
          $(document).on('shown.bs.tab shiny:value init.dt', soon);
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
        ),
        bslib::accordion_panel(
          "Code", value = "code",
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
                               value = TRUE)
        )
      ),
      shiny::div(
        class = "epi-actions",
        shiny::actionButton(
          "insert_code", code_button_label,
          icon  = shiny::icon("terminal"),
          class = "btn-sm btn-primary"
        ),
        # Download: the table's dictionary, in a menu that opens upwards
        shiny::div(
          class = "dropup",
          shiny::tags$button(
            id = "dict_download", type = "button",
            class = "btn btn-sm btn-primary dropdown-toggle",
            `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
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
            shiny::tags$li(shiny::tags$hr(class = "dropdown-divider")),
            shiny::tags$li(shiny::actionLink(
              "dict_bulk", "Dictionaries of all tables\u2026",
              class = "dropdown-item"
            ))
          )
        ),
        .epi_bug_button()
      )
    ),

    # The columns, with buttons for their value labels and the rows chosen
    bslib::nav_panel(
      "Columns", value = "columns",
      shiny::div(
        class = "options-row",
        shiny::tags$button(
          id = "values_open", type = "button", class = "btn btn-sm btn-primary",
          shiny::icon("angles-down"), "Expand value labels"
        ),
        shiny::tags$button(
          id = "values_close", type = "button", class = "btn btn-sm btn-primary",
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
    ),

    # The table's description, notes and linked tables, apart from its
    # columns, as some tables have many notes
    bslib::nav_panel("Collection", value = "collection",
                     shiny::uiOutput("about")),

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

    # The table chosen stays chosen while it is in the list; otherwise the
    # first table, or the first view
    shiny::observe({
      t <- tables()
      choices <- .browse_table_choices(t$data, dataset_labels()$labels)
      names <- unname(unlist(choices))
      current <- shiny::isolate(input$table)
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
      .browse_about(lbl()$dataset)
    })

    output$col_table <- DT::renderDataTable({
      cols <- columns()
      shiny::req(is.null(cols$error))
      show_values <- isTRUE(input$show_values) && !is.null(values())
      d <- .browse_columns(cols$data, lbl(), values = show_values)

      # Width hints to DataTables; the value labels' cells are tagged for
      # their CSS and the selection guard
      widths <- c(`#` = 4, Column = 18, Description = 38, Type = 10,
                  `Value labels` = 15, Notes = 15)
      defs <- lapply(intersect(names(widths), names(d)), function(nm) {
        list(width = paste0(widths[[nm]], "%"),
             targets = match(nm, names(d)) - 1L)
      })
      if ("Value labels" %in% names(d)) {
        defs <- c(defs, list(list(className = "browse-values",
                                  targets = match("Value labels", names(d)) - 1L)))
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
    # columns() whatever the sorting. The line above the table counts them,
    # and Clear selection clears them through DT's proxy.
    chosen_columns <- function() {
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

    # ── Dictionary ─────────────────────────────────────────────────────────
    # The dictionary of the table chosen, from the labels shown. Its value
    # labels are read for it when they are not shown.
    dictionary <- function() {
      ch <- chosen()
      cols <- columns()
      if (!is.null(cols$error)) stop(cols$error, call. = FALSE)
      l <- labels()
      rows <- l$rows
      if (!is.null(l$source)) {
        v <- values()
        if (is.null(v)) {
          r <- read(.lbl_sql_rows(ch$db, l$source, ch$schema, ch$table,
                                  values = TRUE))
          if (!is.null(r$error)) stop(r$error, call. = FALSE)
          v <- list(rows = .lbl_normalise(r$data))
        }
        rows <- rbind(rows, v$rows)
      }
      .dict_assemble(ch, cols$data, rows, l$source)
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
               "col_table_rows_selected")
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
