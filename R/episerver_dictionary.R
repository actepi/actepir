#' View Data Dictionary for an EpiServer Dataset
#'
#' @description
#' Generates an interactive, searchable data dictionary for a specified dataset
#' on the EpiServer and displays it in the RStudio Viewer pane. The dictionary
#' combines column metadata from \code{INFORMATION_SCHEMA} with variable and
#' value labels from the labels table (e.g. \code{ref.DataLabels}).
#'
#' Unlike \code{\link{episerver_browse}}, this function does not block the R
#' console. The dictionary remains visible in the Viewer pane while the user
#' continues working in their script or console.
#'
#' @param dataset Character string. The name of the dataset to generate a
#'   dictionary for. This must match the \code{Dataset} value in the labels
#'   table. Required.
#' @param db Character string. The database name. Defaults to \code{"Analysis"}.
#' @param schema Character string. The schema containing the dataset. Defaults
#'   to \code{"dbo"}.
#' @param lbl_table Character string. The name of the labels table. Defaults to
#'   \code{"DataLabels"}.
#' @param lbl_schema Character string. The schema containing the labels table.
#'   Defaults to \code{"ref"}.
#' @param driver Character string or \code{NULL}. ODBC driver to use for the
#'   connection. If \code{NULL} (default), the function will automatically select
#'   the appropriate driver using \code{episerver_serverdetails("driver")}.
#' @param max_attempts Integer to specify the number of connection attempts.
#'   Defaults to \code{episerver_connect} default.
#'
#' @return Invisibly returns the dictionary as a data frame. The primary effect
#'   is to display the dictionary in the RStudio Viewer pane.
#'
#' @details
#' The dictionary displays one row per variable, showing the column name, its
#' description (from \code{LabelType = 'Var'}), the SQL data type, and a
#' formatted list of factor levels (from \code{LabelType = 'Opt'}) where they
#' exist. The table is fully searchable and sortable via DataTables.
#'
#' The function opens and closes its own database connection, and wraps the
#' connection call with \code{invisible(gc())} to mitigate Type 29 ODBC
#' corruption issues.
#'
#' @note
#' \itemize{
#'   \item Requires the \code{DT} and \code{htmltools} packages
#'   \item Requires an active network connection to the EpiServer
#'   \item The Viewer pane display is non-blocking -- the R console remains
#'     available for other work
#'   \item Calling the function again with a different dataset will replace the
#'     current dictionary in the Viewer pane
#' }
#'
#' @seealso
#' \code{\link{episerver_browse}} for interactive server browsing,
#' \code{\link{episerver_info}} for non-interactive metadata queries,
#' \code{\link{create_dictionary}} for extracting labels from in-memory data
#'
#' @keywords episerver database metadata dictionary
#'
#' @importFrom DBI dbGetQuery dbDisconnect dbIsValid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # View dictionary for the APC dataset
#' episerver_dictionary("APC")
#'
#' # View dictionary for a dataset in a non-default location
#' episerver_dictionary("ED", lbl_table = "DataLabelsNew", lbl_schema = "dbo")
#'
#' # Store the dictionary data frame for further use
#' dict_df <- episerver_dictionary("APC")
#' }
#'
#' @author Warren Holroyd
#'
episerver_dictionary <- function(dataset,
                                 db = "Analysis",
                                 schema = "dbo",
                                 lbl_table = "DataLabels",
                                 lbl_schema = "ref",
                                 driver = NULL,
                                 max_attempts = NULL) {
  
  # ── Check dependencies ────────────────────────────────────────────────────
  if (!requireNamespace("DT", quietly = TRUE) ||
      !requireNamespace("htmltools", quietly = TRUE)) {
    stop(
      "The 'DT' and 'htmltools' packages are required for episerver_dictionary().\n",
      "Install them with: install.packages(c('DT', 'htmltools'))",
      call. = FALSE
    )
  }
  
  # ── Validate inputs ──────────────────────────────────────────────────────
  if (is_invalid(dataset) || !is.character(dataset) || dataset == "") {
    stop("Function requires argument 'dataset' to be supplied as a non-empty character string.",
         call. = FALSE)
  }
  
  # ── Establish connection ──────────────────────────────────────────────────
  invisible(gc())
  connect_args <- list()
  if (!is.null(driver)) connect_args$driver <- driver
  if (!is.null(max_attempts)) connect_args$max_attempts <- max_attempts
  conn <- do.call(episerver_connect, connect_args)
  invisible(gc())
  
  on.exit({
    if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn)
  })
  
  # ── Sanitise inputs ──────────────────────────────────────────────────────
  db_safe         <- gsub("'", "''", db)
  schema_safe     <- gsub("'", "''", schema)
  dataset_safe    <- gsub("'", "''", dataset)
  lbl_table_safe  <- gsub("'", "''", lbl_table)
  lbl_schema_safe <- gsub("'", "''", lbl_schema)
  
  # ── Fetch column metadata with variable labels ──────────────────────────
  sql_cols <- sprintf(
    "SELECT
       c.ORDINAL_POSITION,
       c.COLUMN_NAME,
       v.LabelName         AS Description,
       c.DATA_TYPE,
       c.CHARACTER_MAXIMUM_LENGTH,
       c.NUMERIC_PRECISION,
       c.NUMERIC_SCALE
     FROM [%s].INFORMATION_SCHEMA.COLUMNS c
     LEFT JOIN [%s].[%s].[%s] v
       ON  v.Dataset          = '%s'
       AND v.VarName          = c.COLUMN_NAME
       AND LOWER(v.LabelType) = 'var'
     WHERE c.TABLE_SCHEMA = '%s'
       AND c.TABLE_NAME   = '%s'
     ORDER BY c.ORDINAL_POSITION",
    db_safe,
    db_safe, lbl_schema_safe, lbl_table_safe,
    dataset_safe,
    schema_safe, dataset_safe
  )
  
  cols <- DBI::dbGetQuery(conn, sql_cols)
  
  if (nrow(cols) == 0) {
    warning(
      sprintf("No columns found for [%s].[%s].[%s]. Check the table name or your permissions.",
              db, schema, dataset),
      call. = FALSE
    )
    return(invisible(data.frame()))
  }
  
  # ── Fetch value labels (factor levels) ──────────────────────────────────
  sql_opts <- sprintf(
    "SELECT
       VarName,
       DataCode,
       LabelName
     FROM [%s].[%s].[%s]
     WHERE Dataset          = '%s'
       AND LOWER(LabelType) = 'opt'
     ORDER BY VarName, CAST(
       CASE WHEN ISNUMERIC(DataCode) = 1 THEN DataCode ELSE '999999' END
       AS INT
     )",
    db_safe, lbl_schema_safe, lbl_table_safe,
    dataset_safe
  )
  
  opts <- DBI::dbGetQuery(conn, sql_opts)
  
  # ── Build formatted levels strings ──────────────────────────────────────
  if (nrow(opts) > 0) {
    levels_by_var <- tapply(
      seq_len(nrow(opts)),
      opts$VarName,
      function(idx) {
        paste0(
          opts$DataCode[idx], " = ", opts$LabelName[idx],
          collapse = "<br>"
        )
      },
      simplify = FALSE
    )
  } else {
    levels_by_var <- list()
  }
  
  # ── Assemble dictionary data frame ──────────────────────────────────────
  # Build type string with size info
  type_str <- cols$DATA_TYPE
  has_char_len <- !is.na(cols$CHARACTER_MAXIMUM_LENGTH)
  type_str[has_char_len] <- paste0(
    type_str[has_char_len], "(",
    ifelse(cols$CHARACTER_MAXIMUM_LENGTH[has_char_len] == -1,
           "max",
           as.character(cols$CHARACTER_MAXIMUM_LENGTH[has_char_len])),
    ")"
  )
  has_precision <- !is.na(cols$NUMERIC_PRECISION) & !has_char_len
  has_scale     <- has_precision & !is.na(cols$NUMERIC_SCALE) & cols$NUMERIC_SCALE > 0
  type_str[has_scale] <- paste0(
    type_str[has_scale], "(",
    cols$NUMERIC_PRECISION[has_scale], ",",
    cols$NUMERIC_SCALE[has_scale], ")"
  )
  
  # Look up levels for each column
  levels_str <- vapply(cols$COLUMN_NAME, function(cn) {
    if (cn %in% names(levels_by_var)) {
      levels_by_var[[cn]]
    } else {
      ""
    }
  }, character(1), USE.NAMES = FALSE)
  
  dict <- data.frame(
    `#`           = cols$ORDINAL_POSITION,
    Column        = cols$COLUMN_NAME,
    Description   = ifelse(is.na(cols$Description), "", cols$Description),
    Type          = type_str,
    Levels        = levels_str,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  
  # ── Render as DT widget ─────────────────────────────────────────────────
  title_html <- sprintf(
    '<span style="font-weight:600; font-size:14px; color:#333;">%s</span>
     <span style="font-size:12px; color:#777;"> &mdash; %s.%s.%s &mdash; %d columns</span>',
    dataset, db, schema, dataset, nrow(dict)
  )
  
  widget <- DT::datatable(
    dict,
    escape    = FALSE,
    rownames  = FALSE,
    options   = list(
      pageLength     = 25,
      lengthMenu     = c(10, 25, 50, 100),
      dom            = "lpfrtip",
      ordering       = TRUE,
      scrollY        = "65vh",
      scrollCollapse = TRUE,
      columnDefs     = list(
        list(targets = 4, width = "35%"),
        list(targets = 0, width = "30px")
      )
    ),
    class = "compact stripe hover"
  ) |>
    DT::formatStyle(
      "Levels",
      fontSize = "11px",
      color    = "#555"
    ) |>
    DT::formatStyle(
      "Description",
      fontStyle = "italic",
      color     = "#337ab7"
    )
  
  # ── Build self-contained HTML page ──────────────────────────────────────
  page <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
                       Roboto, Helvetica, Arial, sans-serif;
          margin: 12px;
          background: #fff;
        }
        .dict-title {
          text-align: center;
          padding: 8px 0 4px 0;
          font-size: 14px;
          font-weight: 600;
          color: #333;
        }
        .dict-title span {
          font-weight: 400;
          font-size: 12px;
          color: #777;
        }
      "))
    ),
    htmltools::div(
      class = "dict-title",
      htmltools::HTML(title_html)
    ),
    widget
  )
  
  tmp_file <- file.path(
    normalizePath(tempdir(), winslash = "/"),
    paste0("actepir_dict_", dataset, ".html")
  )
  
  htmltools::save_html(page, file = tmp_file)
  
  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(tmp_file)
  } else {
    utils::browseURL(tmp_file)
  }
  
  invisible(dict)
  
}