#' Interactively Browse EpiServer Database Structure
#'
#' @description
#' Launches an interactive browser in the RStudio Viewer pane that allows users
#' to explore databases, schemas, tables, and column metadata on the EpiServer.
#' The browser provides cascading dropdown selectors, a searchable metadata
#' table with selectable columns, label integration, and the ability to generate
#' and insert connection code at the cursor position.
#'
#' This function is registered as an RStudio Addin and can be launched from the
#' Addins menu, or called directly from the console.
#'
#' @param driver Character string or \code{NULL}. ODBC driver to use for the
#'   connection. If \code{NULL} (default), the function will automatically select
#'   the appropriate driver using \code{episerver_serverdetails("driver")}.
#' @param max_attempts Integer to specify the number of connection attempts.
#'   Defaults to \code{episerver_connect} default.
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
#'   \item Toggle between \strong{quickconnect} mode (single
#'     \code{episerver_quickconnect()} call) and \strong{connect} mode
#'     (separate \code{episerver_connect()} + \code{episerver_lazytable()} calls)
#'   \item Click \strong{Insert Code} to generate ready-made connection and
#'     collection code at the cursor position
#'   \item Click \strong{Done} to close the browser
#' }
#'
#' The function requires \code{shiny}, \code{miniUI}, and \code{DT} to be
#' installed. These are listed as \code{Suggests} dependencies -- if missing,
#' the function will prompt the user to install them.
#'
#' @note
#' \itemize{
#'   \item Requires an active network connection to the EpiServer
#'   \item The initial connection may take a few seconds to establish
#'   \item The RStudio Viewer pane must be available (will not work in plain R)
#'   \item Database/schema/table visibility is limited by the user's permissions
#'   \item Label descriptions are only shown when a labels table is detected
#'     in the selected database
#' }
#'
#' @seealso
#' \code{\link{episerver_info}} for non-interactive metadata queries,
#' \code{\link{episerver_connect}} for establishing database connections,
#' \code{\link{episerver_lazytable}} for creating lazy table references,
#' \code{\link{collect_withlabels}} for collecting data with labels applied
#'
#' @keywords episerver database metadata interactive
#'
#' @inheritDotParams episerver_connect encrypt trust_certificate
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the browser
#' episerver_browse()
#' }
#'
#' @author Warren Holroyd
#'
episerver_browse <- function(driver = NULL, max_attempts = NULL, ...) {
  
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
  
  # ── Establish connection ──────────────────────────────────────────────────
  connect_args <- list()
  if (!is.null(driver)) connect_args$driver <- driver
  if (!is.null(max_attempts)) connect_args$max_attempts <- max_attempts
  conn <- do.call(episerver_connect, c(connect_args, list(...)))
  
  # Helper: run a query and return a data frame
  run_query <- function(sql) {
    tryCatch(
      DBI::dbGetQuery(conn, sql),
      error = function(e) data.frame()
    )
  }
  
  # ── Fetch available databases ─────────────────────────────────────────────
  db_list <- run_query(
    "SELECT name FROM sys.databases WHERE state_desc = 'ONLINE' ORDER BY name"
  )
  if (nrow(db_list) == 0) db_list <- data.frame(name = "Analysis")
  
  # ── UI ────────────────────────────────────────────────────────────────────
  ui <- miniUI::miniPage(
    
    miniUI::gadgetTitleBar(
      "EpiServer Browser",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    
    miniUI::miniContentPanel(
      
      shiny::tags$style(shiny::HTML("
        .gadget-content { padding: 10px; }
        .selector-row { display: flex; gap: 10px; margin-bottom: 10px; }
        .selector-row .form-group { flex: 1; margin-bottom: 0; }
        .info-bar {
          background: #f0f4f8; border-left: 3px solid #337ab7;
          padding: 8px 12px; margin-bottom: 10px; font-size: 12px;
          color: #555;
        }
        .options-row {
          display: flex; align-items: center; gap: 15px;
          margin-bottom: 10px; flex-wrap: wrap;
        }
        .options-row .form-group { margin-bottom: 0; }
        .radio-inline { margin-top: 0; padding-top: 0; }
        #insert_mode { margin-bottom: 0; }
        table.dataTable thead th { background: #337ab7; color: #fff; }
        table.dataTable tbody tr.selected { background-color: #d9edf7 !important; }
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
        )
      ),
      
      # Info bar
      shiny::uiOutput("info_bar"),
      
      # Options row
      shiny::div(
        class = "options-row",
        shiny::actionButton(
          "insert_code", "Insert Code",
          icon  = shiny::icon("terminal"),
          class = "btn-sm btn-primary"
        ),
        shiny::radioButtons(
          "insert_mode", NULL,
          choices  = c("quickconnect", "connect"),
          selected = "quickconnect",
          inline   = TRUE
        ),
        shiny::checkboxInput(
          "use_labels", "Import with labels",
          value = TRUE
        ),
        shiny::selectInput(
          "labels_source", "Labels table",
          choices = NULL,
          width   = "220px"
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
    
    # Reactive values
    rv <- shiny::reactiveValues(
      table_count = 0,
      col_data    = data.frame()
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
        "SELECT TABLE_SCHEMA, TABLE_NAME
         FROM [%s].INFORMATION_SCHEMA.TABLES
         WHERE TABLE_NAME IN ('DataLabels', 'DataLabelsNew')
           AND TABLE_TYPE = 'BASE TABLE'
         ORDER BY
           CASE WHEN TABLE_NAME = 'DataLabels' THEN 0 ELSE 1 END,
           CASE WHEN TABLE_SCHEMA = 'ref' THEN 0 ELSE 1 END,
           TABLE_SCHEMA",
        req_db
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
         ORDER BY TABLE_NAME",
        req_db, req_schema
      ))
      rv$table_count <- nrow(tables)
      if (nrow(tables) > 0) {
        choices <- tables$TABLE_NAME
      } else {
        choices <- character(0)
      }
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
      
      # Take reactive dependency on labels_source so table redraws on change
      lbl_source <- input$labels_source
      
      req_db     <- gsub("'", "''", input$db)
      req_schema <- gsub("'", "''", input$schema)
      req_table  <- gsub("'", "''", input$table)
      
      # Determine if a valid labels table is selected
      has_labels <- !is.null(lbl_source) && nzchar(lbl_source) &&
        grepl("\\.", lbl_source)
      
      if (has_labels) {
        lbl_parts  <- strsplit(lbl_source, ".", fixed = TRUE)[[1]]
        lbl_schema <- gsub("'", "''", lbl_parts[1])
        lbl_table  <- gsub("'", "''", lbl_parts[2])
        
        sql <- sprintf(
          "SELECT
             c.ORDINAL_POSITION          AS [#],
             c.COLUMN_NAME               AS [Column],
             l.LabelName                 AS [Description],
             c.DATA_TYPE                 AS [Type],
             c.CHARACTER_MAXIMUM_LENGTH  AS [Max Length],
             c.NUMERIC_PRECISION         AS [Precision],
             c.NUMERIC_SCALE             AS [Scale]
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
        sql <- sprintf(
          "SELECT
             ORDINAL_POSITION          AS [#],
             COLUMN_NAME               AS [Column],
             CAST(NULL AS VARCHAR(1))  AS [Description],
             DATA_TYPE                 AS [Type],
             CHARACTER_MAXIMUM_LENGTH  AS [Max Length],
             NUMERIC_PRECISION         AS [Precision],
             NUMERIC_SCALE             AS [Scale]
           FROM [%s].INFORMATION_SCHEMA.COLUMNS
           WHERE TABLE_SCHEMA = '%s'
             AND TABLE_NAME   = '%s'
           ORDER BY ORDINAL_POSITION",
          req_db, req_schema, req_table
        )
      }
      
      cols <- run_query(sql)
      rv$col_data <- cols
      
      DT::datatable(
        cols,
        options   = list(
          paging         = FALSE,
          dom            = "ft",
          scrollY        = "40vh",
          scrollCollapse = TRUE,
          ordering       = TRUE,
          columnDefs     = list(
            list(visible = FALSE, targets = 0)
          )
        ),
        rownames  = FALSE,
        selection = list(mode = "multiple", target = "row"),
        class     = "compact stripe hover",
        style     = "bootstrap"
      )
    })
    
    # ── Insert code button ─────────────────────────────────────────────────
    shiny::observeEvent(input$insert_code, {
      shiny::req(input$db, input$schema, input$table)
      
      # ── Selected columns (if any rows selected) ──────────────────────────
      selected_rows <- input$col_table_rows_selected
      if (length(selected_rows) > 0 && nrow(rv$col_data) > 0) {
        sel_cols    <- rv$col_data[["Column"]][selected_rows]
        select_line <- paste0("  select(",
                              paste(sel_cols, collapse = ", "),
                              ") |>\n")
      } else {
        select_line <- ""
      }
      
      # ── Collect function ─────────────────────────────────────────────────
      collect_fn <- if (isTRUE(input$use_labels)) {
        "collect_withlabels()"
      } else {
        "collect()"
      }
      
      # ── Collection nub ───────────────────────────────────────────────────
      collect_nub <- paste0("data <- tb |>\n",
                            select_line,
                            "  ", collect_fn, "\n")
      
      # ── Connection code ──────────────────────────────────────────────────
      gc_line <- "invisible(gc())\n"
      
      if (input$insert_mode == "quickconnect") {
        
        qc_code <- if (input$db == "Analysis" && input$schema == "dbo") {
          sprintf('tb <- episerver_quickconnect("%s")', input$table)
        } else if (input$db == "Analysis") {
          sprintf('tb <- episerver_quickconnect("%s", schema = "%s")',
                  input$table, input$schema)
        } else {
          sprintf('tb <- episerver_quickconnect("%s", schema = "%s", db = "%s")',
                  input$table, input$schema, input$db)
        }
        code <- paste0(gc_line, qc_code, "\n", gc_line, "\n", collect_nub)
        
      } else {
        
        tbl_code <- if (input$db == "Analysis" && input$schema == "dbo") {
          sprintf('tb <- episerver_lazytable(conn, "%s")', input$table)
        } else if (input$db == "Analysis") {
          sprintf('tb <- episerver_lazytable(conn, "%s", schema = "%s")',
                  input$table, input$schema)
        } else {
          sprintf('tb <- episerver_lazytable(conn, "%s", schema = "%s", db = "%s")',
                  input$table, input$schema, input$db)
        }
        code <- paste0(gc_line,
                       "conn <- episerver_connect()\n",
                       gc_line,
                       tbl_code, "\n\n",
                       collect_nub)
        
      }
      
      if (rstudioapi::isAvailable()) {
        rstudioapi::insertText(text = code)
      } else {
        message("Code:\n", code)
      }
    })
    
    # ── Done button ────────────────────────────────────────────────────────
    shiny::observeEvent(input$done, {
      tryCatch(
        if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn),
        error = function(e) invisible(NULL)
      )
      shiny::stopApp()
    })
    
    # Clean up connection if gadget is closed via X button
    session$onSessionEnded(function() {
      tryCatch(
        if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn),
        error = function(e) invisible(NULL)
      )
    })
    
  }
  
  # ── Launch ────────────────────────────────────────────────────────────────
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 550))
  
}