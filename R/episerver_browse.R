#' Interactively Browse EpiServer Database Structure
#'
#' @description
#' Launches an interactive browser that allows users to explore databases,
#' schemas, tables, and column metadata on the EpiServer. The browser provides
#' cascading dropdown selectors, a searchable metadata table with selectable
#' columns, label integration, and generation of ready-made connection code.
#'
#' By default the app runs in a background R process using
#' \code{callr::r_bg()}, following the technique described by Will Landau
#' (\url{https://wlandau.github.io/posts/2020-12-18-non-blocking-app/}). The
#' main R console therefore remains free while the browser is open. Set
#' \code{background = FALSE} for the original blocking gadget behaviour.
#'
#' The app opens in the system web browser. It is registered as an RStudio
#' Addin and can be launched from the Addins menu (\emph{Open EpiServer
#' Browser}), or called directly from the console.
#'
#' @param background Logical. If \code{TRUE} (default), the app runs in a
#'   background R process and the console remains free. If \code{FALSE}, the
#'   app runs as a blocking gadget in the current session.
#' @param port Integer or \code{NULL}. The local port for the background app.
#'   If \code{NULL} (default), the background process chooses a free port
#'   itself, retrying on collision.
#' @param driver Character string or \code{NULL}. ODBC driver to use for the
#'   connection. If \code{NULL} (default), the function will automatically select
#'   the appropriate driver using \code{episerver_serverdetails("driver")}.
#' @param max_attempts Integer to specify the number of connection attempts.
#'   Defaults to \code{episerver_connect} default.
#'
#' @return In background mode, invisibly returns the \code{callr} process
#'   handle. In foreground mode, returns nothing.
#'
#' @details
#' The browser provides three cascading selectors (Database, Schema, Table) and
#' displays column metadata for the selected table. Users can:
#'
#' \itemize{
#'   \item Browse available databases, schemas, and tables
#'   \item View column names, data types, descriptions (from labels), and other
#'     metadata in a searchable table
#'   \item Select individual columns via checkboxes to generate a
#'     \code{dplyr::select()} statement in the output code
#'   \item Toggle \strong{Import with labels} to switch between
#'     \code{collect()} and \code{collect_withlabels()} in the output code
#'   \item Choose a labels table source (auto-detected from the database) to
#'     enrich the column display with variable descriptions
#'   \item Toggle \strong{Show value labels} to append a collapsible list of
#'     value labels (DataCode = LabelName) for each column that has them
#'   \item Toggle between \strong{quickconnect} mode (an inline
#'     \code{episerver_quickconnect()} pipe), \strong{connect} mode
#'     (separate \code{episerver_connect()} + \code{episerver_lazytable()}
#'     calls with optional \code{library()} declarations), and \strong{none}
#'     (emit only the collection pipe, assuming a \code{tb} lazy table
#'     already exists)
#'   \item Toggle \strong{Declare packages} to control namespacing: when on,
#'     inline modes use \code{pkg::} prefixes and connect mode adds
#'     \code{library()} calls; when off, calls are bare
#'   \item Adjust the table height and expanded-levels height with the
#'     sliders under Table sizing
#'   \item Generate the connection and collection code via the code button
#'   \item Click \strong{Done} to close the browser
#' }
#'
#' The code button adapts to the mode. In foreground mode it is labelled
#' \strong{Insert Code} and inserts at the cursor position via
#' \code{rstudioapi::insertText()}. A background process has no connection to
#' the RStudio editor, so in background mode the button is labelled
#' \strong{Copy Code} and places the generated code on the clipboard instead.
#'
#' In background mode the app keeps running when its browser tab is closed.
#' Calling the function again while the app is running reopens the existing
#' app rather than starting a second process, after first verifying it is
#' still serving; a process that is alive but no longer
#' responding is killed and replaced automatically. Stop the background app
#' with the Done button or \code{\link{episerver_browse_stop}};
#' it is also stopped automatically when the R session exits. Process
#' supervision is attempted but falls back gracefully where blocked by system
#' policy (a message is shown once per launch). After an abnormal R crash an
#' orphaned background process can be ended from Task Manager (Rscript.exe).
#'
#' The function requires \code{shiny}, \code{miniUI}, and \code{DT}, plus
#' \code{callr} for background mode. These are listed as \code{Suggests}
#' dependencies. If the background process cannot be spawned in a restricted
#' environment, use \code{background = FALSE}.
#'
#' @note
#' \itemize{
#'   \item Requires an active network connection to the EpiServer
#'   \item The initial connection may take a few seconds to establish
#'   \item Database/schema/table visibility is limited by the user's permissions
#'   \item System databases (tempdb, master, msdb) and any database whose
#'     name begins with an underscore are hidden from the Database dropdown
#'   \item The database connection is validated before each query and
#'     re-established automatically if it has been dropped
#'   \item The labels dropdown only offers tables containing the required
#'     label columns (Dataset, VarName, LabelType, LabelName, DataCode,
#'     DataType); older formats are hidden, and column listings fall back to
#'     an unlabelled view if a labels join fails
#'   \item Tables with DataLabels in the name are excluded from the Table
#'     dropdown
#'   \item Label descriptions are only shown when a labels table is detected
#'     in the selected database
#' }
#'
#' @seealso
#' \code{\link{episerver_browse_stop}} for stopping the background app,
#' \code{\link{episerver_info}} for non-interactive metadata queries,
#' \code{\link{collect_withlabels}} for collecting data with labels applied
#'
#' @keywords episerver database metadata interactive
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch in a background process; console stays free
#' episerver_browse()
#'
#' # Original blocking gadget behaviour
#' episerver_browse(background = FALSE)
#'
#' # Stop the background app
#' episerver_browse_stop()
#' }
#'
#' @author Warren Holroyd
#'
episerver_browse <- function(background = TRUE, port = NULL,
                             driver = NULL, max_attempts = NULL) {

  .run_app(
    key          = "browse",
    title        = "EpiServer Browser",
    fn           = "episerver_browse",
    factory      = "episerver_browse_app",
    factory_args = list(driver = driver, max_attempts = max_attempts),
    background   = background,
    port         = port
  )

}


#' Stop the Background EpiServer Browser
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

  .stop_app("browse", "EpiServer Browser")

}


# Internal factory that builds the Shiny app. Runs in the current session for
# foreground mode, or inside the callr background process for background mode.
# The database connection is created here and closed when the app stops.
#' @noRd
episerver_browse_app <- function(driver = NULL, max_attempts = NULL) {

  # ── Check dependencies ────────────────────────────────────────────────────
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("miniUI", quietly = TRUE) ||
      !requireNamespace("DT", quietly = TRUE)) {
    stop(
      "The 'shiny', 'miniUI', and 'DT' packages are required for episerver_browse().\n",
      "Install them with: install.packages(c('shiny', 'miniUI', 'DT'))",
      call. = FALSE
    )
  }

  # Foreground (RStudio session) or background process?
  in_rstudio <- tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)

  # ── Establish connection ──────────────────────────────────────────────────
  connect_args <- list()
  if (!is.null(driver)) connect_args$driver <- driver
  if (!is.null(max_attempts)) connect_args$max_attempts <- max_attempts
  db <- .epi_connection(connect_args)

  # Helper: run a query and return a data frame, or an empty data frame if the
  # query still fails after the reconnect-and-retry in .epi_connection(), so a
  # failed query does not stop the app.
  run_query <- function(sql) {
    tryCatch(db$query(sql), error = function(e) data.frame())
  }

  # ── Fetch available databases ─────────────────────────────────────────────
  # Databases hidden from the Database dropdown. Extend as required.
  excluded_dbs <- c("tempdb", "master", "msdb", "model")

  # Relative column widths (percent) for the metadata table. Adjust to taste;
  # names must match the display columns. Widths are hints to DataTables and
  # unused shares are redistributed when the Levels column is hidden.
  col_widths <- c(Column = 20, Description = 40, Type = 10, Levels = 30)

  db_list <- run_query(
    "SELECT name FROM sys.databases WHERE state_desc = 'ONLINE' ORDER BY name"
  )
  if (nrow(db_list) > 0) {
    db_list <- db_list[
      !(db_list$name %in% excluded_dbs) &
        !startsWith(db_list$name, "_"),
      ,
      drop = FALSE
    ]
  }
  if (nrow(db_list) == 0) db_list <- data.frame(name = "Analysis")

  code_button_label <- if (in_rstudio) "Insert Code" else "Copy Code"

  # ── UI ────────────────────────────────────────────────────────────────────
  ui <- miniUI::miniPage(

    miniUI::gadgetTitleBar(
      "EpiServer Browser",
      left  = NULL,
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),

    miniUI::miniContentPanel(

      .epi_app_style(),

      # Browser-only rules: labels selector, insert mode, value-label cells
      shiny::tags$style(shiny::HTML("
        :root {
          --epi-levels-max:   150px;    /* expanded level list height (slider) */
        }
        .labels-slot { border-left: 3px solid var(--epi-accent);
                       padding-left: 10px; }
        .labels-slot .control-label { color: var(--epi-accent); }
        #labels_source { background-color: #f7f4fb;
                         border-color: var(--epi-accent); }
        #insert_mode { margin-bottom: 0; }
        td.levels-cell details summary { cursor: pointer;
                                         color: var(--epi-accent);
                                         font-size: 11px; }
        td.levels-cell details div { font-size: 11px; color: #555;
                                     padding-top: 2px;
                                     max-height: var(--epi-levels-max);
                                     overflow-y: auto; }
      ")),

      # Clipboard handler used when running outside RStudio (background mode)
      .epi_copy_script(),

      # Restrict row selection to the Column/Description/Type cells. DT's
      # selection handler may be bound to mousedown or pointer events rather
      # than click, so the guard intercepts the whole pointer-event family in
      # the capture phase, which runs before any delegated handler regardless
      # of where it is bound. stopPropagation() does not affect browser
      # default actions, so the <details> toggle, inner scrolling, and text
      # selection are unaffected.
      shiny::tags$script(shiny::HTML("
        ['pointerdown', 'mousedown', 'mouseup', 'click'].forEach(function(type) {
          document.addEventListener(type, function(e) {
            var t = e.target;
            if (!t || !t.closest) return;
            if (t.closest('td.levels-cell') ||
                t.closest('#col_table details')) {
              e.stopPropagation();
            }
          }, true);
        });
      ")),

      # Live-update the expanded levels max-height from its slider
      shiny::tags$script(shiny::HTML("
        Shiny.addCustomMessageHandler('actepir_levels_height', function(px) {
          document.documentElement.style.setProperty('--epi-levels-max',
                                                      px + 'px');
        });
      ")),

      # Selector row: Database / Schema / Table
      shiny::div(
        class = "selector-row",
        shiny::selectInput(
          "db", "Database",
          choices  = db_list$name,
          selected = if ("Analysis" %in% db_list$name) "Analysis" else db_list$name[1],
          width = "100%"
        ),
        shiny::selectInput(
          "schema", "Schema",
          choices  = NULL,
          width = "100%"
        ),
        shiny::selectInput(
          "table", "Table",
          choices  = NULL,
          width = "100%"
        ),
        shiny::div(
          class = "labels-slot",
          shiny::selectInput(
            "labels_source", "Labels table",
            choices = NULL,
            width   = "100%"
          )
        )
      ),

      # Info bar
      shiny::uiOutput("info_bar"),

      # Options row
      shiny::div(
        class = "options-row",
        shiny::actionButton(
          "insert_code", code_button_label,
          icon  = shiny::icon("terminal"),
          class = "btn-sm btn-primary"
        ),
        shiny::radioButtons(
          "insert_mode", NULL,
          choices  = c("quickconnect", "connect", "none"),
          selected = "quickconnect",
          inline   = TRUE
        ),
        shiny::checkboxInput(
          "declare_pkgs", "Declare packages",
          value = TRUE
        ),
        # Spacer pushes the label options to the right of the row
        shiny::div(style = "flex: 1 1 auto;"),
        shiny::checkboxInput(
          "use_labels", "Import with labels",
          value = TRUE
        ),
        shiny::checkboxInput(
          "show_levels", "Show value labels",
          value = FALSE
        )
      ),

      # Sizing options (collapsible to keep the top area uncluttered)
      shiny::tags$details(
        style = "margin-bottom: 10px;",
        shiny::tags$summary(
          "Table sizing",
          style = "cursor: pointer; font-size: 12px; color: var(--epi-accent);"
        ),
        shiny::div(
          style = "display: flex; gap: 25px; flex-wrap: wrap; padding-top: 8px;",
          shiny::sliderInput(
            "table_height", "Table height (px)",
            min = 200, max = 900, value = 360, step = 20, width = "260px"
          ),
          shiny::sliderInput(
            "levels_height", "Expanded levels height (px)",
            min = 60, max = 400, value = 150, step = 10, width = "260px"
          )
        )
      ),

      # Column metadata table (with row selection)
      shiny::div(
        style = "margin-top: 4px;",
        DT::dataTableOutput("col_table")
      )

    )
  )

  # ── Server ────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    # Relay the expanded-levels height slider to the CSS variable
    shiny::observeEvent(input$levels_height, {
      session$sendCustomMessage("actepir_levels_height", input$levels_height)
    })

    # Reactive values
    rv <- shiny::reactiveValues(
      table_count = 0,
      col_data    = data.frame(),
      tables      = character(0),
      tick        = 0
    )

    # ── Schema list (updates when db changes) ──────────────────────────────
    shiny::observeEvent(input$db, {
      req_db <- gsub("'", "''", input$db)
      schemas <- run_query(sprintf(
        "SELECT DISTINCT TABLE_SCHEMA
         FROM [%s].INFORMATION_SCHEMA.TABLES
         ORDER BY TABLE_SCHEMA",
        req_db
      ))
      choices  <- if (nrow(schemas) > 0) schemas$TABLE_SCHEMA else "dbo"
      selected <- if ("dbo" %in% choices) "dbo" else choices[1]
      shiny::updateSelectInput(session, "schema",
                               choices = choices, selected = selected)
    })

    # ── Labels table detection (updates when db changes) ───────────────────
    shiny::observeEvent(input$db, {
      req_db <- gsub("'", "''", input$db)
      labels_tables <- run_query(sprintf(
        "SELECT t.TABLE_SCHEMA, t.TABLE_NAME
         FROM [%s].INFORMATION_SCHEMA.TABLES t
         WHERE t.TABLE_NAME LIKE '%%DataLabels%%'
           AND t.TABLE_TYPE = 'BASE TABLE'
           AND (SELECT COUNT(DISTINCT c.COLUMN_NAME)
                FROM [%s].INFORMATION_SCHEMA.COLUMNS c
                WHERE c.TABLE_SCHEMA = t.TABLE_SCHEMA
                  AND c.TABLE_NAME   = t.TABLE_NAME
                  AND c.COLUMN_NAME IN ('Dataset', 'VarName', 'LabelType',
                                        'LabelName', 'DataCode', 'DataType')
               ) = 6
         ORDER BY
           CASE WHEN t.TABLE_NAME = 'DataLabels'    THEN 0
                WHEN t.TABLE_NAME = 'DataLabelsNew' THEN 1
                ELSE 2 END,
           CASE WHEN t.TABLE_SCHEMA = 'ref' THEN 0 ELSE 1 END,
           t.TABLE_SCHEMA, t.TABLE_NAME",
        req_db, req_db
      ))

      if (nrow(labels_tables) > 0) {
        labels_choices <- paste0(labels_tables$TABLE_SCHEMA, ".",
                                 labels_tables$TABLE_NAME)
        default <- if ("ref.DataLabels" %in% labels_choices) {
          "ref.DataLabels"
        } else {
          labels_choices[1]
        }
        shiny::updateSelectInput(session, "labels_source",
                                 choices  = labels_choices,
                                 selected = default)
      } else {
        shiny::updateSelectInput(session, "labels_source",
                                 choices  = c("(none found)" = ""),
                                 selected = "")
      }
    })

    # ── Table list (updates when schema changes) ───────────────────────────
    shiny::observeEvent(list(input$db, input$schema), {
      shiny::req(input$db, input$schema)
      req_db     <- gsub("'", "''", input$db)
      req_schema <- gsub("'", "''", input$schema)
      tables <- run_query(sprintf(
        "SELECT TABLE_NAME
         FROM [%s].INFORMATION_SCHEMA.TABLES
         WHERE TABLE_SCHEMA = '%s'
           AND TABLE_TYPE   = 'BASE TABLE'
           AND TABLE_NAME NOT LIKE '%%DataLabels%%'
         ORDER BY TABLE_NAME",
        req_db, req_schema
      ))
      rv$table_count <- nrow(tables)
      choices <- if (nrow(tables) > 0) tables$TABLE_NAME else character(0)
      rv$tables <- choices
      rv$tick   <- rv$tick + 1
      shiny::updateSelectInput(session, "table", choices = choices)
    })

    # ── Info bar ───────────────────────────────────────────────────────────
    output$info_bar <- shiny::renderUI({
      shiny::req(input$db, input$schema)
      shiny::div(
        class = "info-bar",
        shiny::HTML(sprintf(
          "<strong>%s</strong>.%s &mdash; %s object(s) found",
          input$db, input$schema, rv$table_count
        ))
      )
    })

    # ── Column metadata table ──────────────────────────────────────────────
    output$col_table <- DT::renderDataTable({
      shiny::req(input$db, input$schema, input$table)

      # Redraw when the table list changes, even if the selected table name
      # is unchanged (common when databases share table names), and never
      # query a table that is not in the current list
      rv$tick
      shiny::req(input$table %in% rv$tables)

      # Take reactive dependency on labels_source so table redraws on change
      lbl_source <- input$labels_source

      req_db     <- gsub("'", "''", input$db)
      req_schema <- gsub("'", "''", input$schema)
      req_table  <- gsub("'", "''", input$table)

      # Determine if a valid labels table is selected
      has_labels <- !is.null(lbl_source) && nzchar(lbl_source) &&
                    grepl("\\.", lbl_source)

      # Plain (no join) column listing; also the fallback when a labels join
      # fails against a structurally incompatible table
      sql_plain <- sprintf(
        "SELECT
           ORDINAL_POSITION         AS ord,
           COLUMN_NAME              AS col,
           CAST(NULL AS VARCHAR(1)) AS descr,
           DATA_TYPE                AS dtype,
           CHARACTER_MAXIMUM_LENGTH AS charlen,
           NUMERIC_PRECISION        AS numprec,
           NUMERIC_SCALE            AS numscale
         FROM [%s].INFORMATION_SCHEMA.COLUMNS
         WHERE TABLE_SCHEMA = '%s'
           AND TABLE_NAME   = '%s'
         ORDER BY ORDINAL_POSITION",
        req_db, req_schema, req_table
      )

      if (has_labels) {
        lbl_parts  <- strsplit(lbl_source, ".", fixed = TRUE)[[1]]
        lbl_schema <- gsub("'", "''", lbl_parts[1])
        lbl_table  <- gsub("'", "''", lbl_parts[2])

        sql <- sprintf(
          "SELECT
             c.ORDINAL_POSITION         AS ord,
             c.COLUMN_NAME              AS col,
             l.LabelName                AS descr,
             c.DATA_TYPE                AS dtype,
             c.CHARACTER_MAXIMUM_LENGTH AS charlen,
             c.NUMERIC_PRECISION        AS numprec,
             c.NUMERIC_SCALE            AS numscale
           FROM [%s].INFORMATION_SCHEMA.COLUMNS c
           LEFT JOIN [%s].[%s].[%s] l
             ON  l.VarName          = c.COLUMN_NAME
             AND l.Dataset          = c.TABLE_NAME
             AND LOWER(l.LabelType) = 'var'
           WHERE c.TABLE_SCHEMA = '%s'
             AND c.TABLE_NAME   = '%s'
           ORDER BY c.ORDINAL_POSITION",
          req_db,
          req_db, lbl_schema, lbl_table,
          req_schema, req_table
        )
      } else {
        sql <- sql_plain
      }

      cols <- run_query(sql)

      # The column listing must never depend on the labels join succeeding:
      # a table always has columns, so an empty result from the join query
      # means the join itself failed
      if (has_labels && nrow(cols) == 0) {
        cols <- run_query(sql_plain)
      }

      # Fold length/precision detail into the type string
      type_str <- cols$dtype
      has_char <- !is.na(cols$charlen)
      type_str[has_char] <- paste0(
        type_str[has_char], "(",
        ifelse(cols$charlen[has_char] == -1, "max",
               as.character(cols$charlen[has_char])),
        ")"
      )
      has_prec  <- !is.na(cols$numprec) & !has_char
      has_scale <- has_prec & !is.na(cols$numscale) & cols$numscale > 0
      type_str[has_scale] <- paste0(
        type_str[has_scale], "(",
        cols$numprec[has_scale], ",",
        cols$numscale[has_scale], ")"
      )

      cols_display <- data.frame(
        `#`         = cols$ord,
        Column      = cols$col,
        Description = ifelse(is.na(cols$descr), "",
                             htmltools::htmlEscape(cols$descr)),
        Type        = type_str,
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )

      # Optionally append collapsed value labels per column
      show_levels <- isTRUE(input$show_levels)
      if (show_levels && has_labels && nrow(cols_display) > 0) {

        sql_opts <- sprintf(
          "SELECT VarName, DataCode, LabelName
           FROM [%s].[%s].[%s]
           WHERE Dataset          = '%s'
             AND LOWER(LabelType) = 'opt'
           ORDER BY VarName,
             CASE WHEN ISNUMERIC(DataCode) = 1
                  THEN CAST(DataCode AS FLOAT) ELSE 999999 END,
             DataCode",
          req_db, lbl_schema, lbl_table, req_table
        )
        opts <- run_query(sql_opts)

        if (nrow(opts) > 0) {
          lvl_html <- vapply(
            split(opts, opts$VarName),
            function(d) {
              body <- paste0(
                htmltools::htmlEscape(d$DataCode), " = ",
                htmltools::htmlEscape(d$LabelName),
                collapse = "<br>"
              )
              sprintf(
                "<details><summary>%d level%s</summary><div>%s</div></details>",
                nrow(d), if (nrow(d) == 1) "" else "s", body
              )
            },
            character(1)
          )
          cols_display$Levels <- ifelse(
            cols_display$Column %in% names(lvl_html),
            lvl_html[cols_display$Column],
            ""
          )
        } else {
          cols_display$Levels <- ""
        }
      }

      rv$col_data <- cols_display

      # Hide the ordinal column; apply configured width hints; tag the Levels
      # column for the CSS and the selection guard
      col_defs <- list(list(visible = FALSE, targets = 0))
      for (nm in intersect(names(col_widths), names(cols_display))) {
        col_defs <- c(col_defs, list(list(
          width   = paste0(col_widths[[nm]], "%"),
          targets = which(names(cols_display) == nm) - 1
        )))
      }
      if ("Levels" %in% names(cols_display)) {
        col_defs <- c(col_defs, list(list(
          className = "levels-cell",
          targets   = which(names(cols_display) == "Levels") - 1
        )))
      }

      DT::datatable(
        cols_display,
        escape    = FALSE,
        options   = list(
          paging         = FALSE,
          dom            = "ft",
          scrollY        = paste0(if (is.null(input$table_height)) 360
                                  else input$table_height, "px"),
          scrollCollapse = TRUE,
          ordering       = TRUE,
          autoWidth      = FALSE,
          columnDefs     = col_defs
        ),
        rownames  = FALSE,
        selection = list(mode = "multiple", target = "row"),
        class     = "compact stripe hover",
        style     = "bootstrap"
      )
    })

    # ── Insert / copy code button ──────────────────────────────────────────
    shiny::observeEvent(input$insert_code, {
      shiny::req(input$db, input$schema, input$table)

      # Selected columns (if any rows selected)
      selected_rows <- input$col_table_rows_selected
      if (length(selected_rows) > 0 && nrow(rv$col_data) > 0) {
        sel_cols <- rv$col_data[["Column"]][selected_rows]
      } else {
        sel_cols <- character(0)
      }
      sel_args <- paste(sel_cols, collapse = ", ")

      declare  <- isTRUE(input$declare_pkgs)
      # Package qualifier for inline (quickconnect / none) code
      q <- function(pkg, fn) if (declare) paste0(pkg, "::", fn) else fn

      # dplyr::select() pipe segment, only when columns are actually chosen
      have_sel <- length(sel_cols) > 0
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

      collect_call <- if (isTRUE(input$use_labels)) {
        "collect_withlabels()"
      } else {
        "collect()"
      }

      # Argument tail shared by quickconnect and lazytable calls
      arg_tail <- if (input$db == "Analysis" && input$schema == "dbo") {
        sprintf('"%s"', input$table)
      } else if (input$db == "Analysis") {
        sprintf('"%s", schema = "%s"', input$table, input$schema)
      } else {
        sprintf('"%s", schema = "%s", db = "%s"',
                input$table, input$schema, input$db)
      }

      if (input$insert_mode == "quickconnect") {

        # Inline, package-qualified pipe; no intermediate 'tb'
        code <- paste0(
          "data <- ", q("actepir", "episerver_quickconnect"), "(", arg_tail, ") |>\n",
          dplyr_select,
          "  ", q("actepir", collect_call), "\n"
        )

      } else if (input$insert_mode == "connect") {

        # Multi-object form with optional library() declarations
        header <- if (declare) "library(actepir)\nlibrary(dplyr)\n\n" else ""
        code <- paste0(
          header,
          "conn <- episerver_connect()\n",
          "tb <- episerver_lazytable(conn, ", arg_tail, ")\n\n",
          "data <- tb |>\n",
          plain_select,
          "  ", collect_call, "\n"
        )

      } else {

        # none: assume 'tb' already exists; emit only the collection pipe
        code <- paste0(
          "data <- tb |>\n",
          dplyr_select,
          "  ", q("actepir", collect_call), "\n"
        )

      }

      .epi_send_code(session, code)
    })

    # ── Done button ────────────────────────────────────────────────────────
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })

  }

  # App-level cleanup. A graceful disconnect only happens in foreground mode:
  # in a background worker, dbDisconnect() on a wedged ODBC connection can
  # hang R shutdown, so cleanup there is left to process exit instead.
  shiny::shinyApp(
    ui      = ui,
    server  = server,
    onStart = function() {
      if (in_rstudio) {
        shiny::onStop(db$disconnect)
      }
    }
  )

}
