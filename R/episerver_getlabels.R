#' Download labels from EpiServer for a given dataset
#'
#' This function will allow the user to download the variable and value labels of a table on EpiServer, from the label tables of its database, as the Dataset Browser finds them.  The function will always search EpiServer and cannot be used for other servers of file locations.
#'
#' By default the labels come from the first label table of the table's database that holds rows for it: `ref.DataProperties`, then tables named `DataLabels`, then `DataLabelsNew`, then others whose name holds DataLabels, each in the `ref` schema before others (see [episerver_browse()]). A table therefore keeps its labels while they move from `ref.DataLabels` to `ref.DataProperties`.
#'
#' Giving any of `lbl_table`, `lbl_schema` and `lbl_db` reads the labels from that one label table instead, in either layout. Almost all non-administrative users will not need to use the lbl_* arguments.
#'
#' Rows from `ref.DataProperties` are returned in the layout of `DataLabels`. Only variable labels and value labels are returned; notes, display formats, dataset labels and linked tables are not. Codes and labels are trimmed of spaces, label types are read in any case, and only the first label of a column, or of a column's code, is kept. A message says when no label table holds labels for the table.
#'
#' @param dataset The table whose labels are wanted: a lazy table from EpiServer, such as one from \code{\link{episerver_quickconnect}} or \code{\link{episerver_lazytable}}, or the name of a table in `Analysis.dbo`, as character.  Mandatory.
#' @param lbl_table The name of one label table to read the labels from, in place of the label table chosen automatically. Argument should be
#'   supplied as character. If not supplied while `lbl_schema` or `lbl_db` is, will default to `DataLabels`.
#' @param lbl_schema The schema of one label table to read the labels from. Argument should be
#'   supplied as character. If not supplied while `lbl_table` or `lbl_db` is, will default to `ref`.
#' @param lbl_db The database of one label table to read the labels from. Argument should be
#'   supplied as character. If not supplied while `lbl_table` or `lbl_schema` is, will default to the table's database.
#' @param driver Character string or \code{NULL}. ODBC driver to use for the connection.
#'   If \code{NULL} (default), the function will automatically select the appropriate
#'   driver using \code{episerver_serverdetails("driver")}. See \code{\link[odbc]{odbcListDrivers}}
#'   to identify which drivers you have access to.
#' @param max_attempts Integer to specify the number of attempts which will be made
#'   to connect to the server. Defaults to \code{episerver_connect} default.  Workaround
#'   for concurrency bug in RStudio.
#' @keywords episerver
#'
#' @return A tibble of the labels in the layout of `DataLabels`: `Dataset` (the table's name), `VarName` (the column), `LabelType` (`Var` for a variable label, `Opt` for a value label), `DataCode` (the code of a value label) and `LabelName` (the label). Variable labels come first, then value labels in code order.
#'
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr tbl
#' @importFrom dplyr collect
#' @importFrom dbplyr in_catalog
#' @importFrom magrittr %>%
#'
#' @inheritDotParams episerver_connect encrypt trust_certificate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Import the labels of APC in Analysis.dbo, from the label table that
#' # holds them
#' labels = episerver_getlabels("APC")
#'
#' # Import the labels of a lazy table
#' labels = episerver_quickconnect("ED") %>%
#'   episerver_getlabels()
#'
#' # Import APC labels from one label table, in a non-default location
#' labels = episerver_getlabels(
#'   dataset = "APC",
#'   lbl_table = "DataLabelsNew",
#'   lbl_schema = "dbo",
#'   lbl_db = "HospitalAPC"
#'   )
#' }
#'

episerver_getlabels <- function(dataset, lbl_table = NULL, lbl_schema = NULL, lbl_db = NULL, driver = NULL, max_attempts = NULL, ...) {

  # check required arguments present
  if (is_invalid(dataset)) {
    stop("Function requires argument 'dataset' to be supplied")
  }

  # A lazy table brings its connection; a table's name needs one, which is
  # closed when the labels have been read
  if (inherits(dataset, "tbl_sql")) {

    parts <- .lbl_table_path(dataset)
    conn  <- dataset$src$con

  } else if (is.character(dataset)) {

    parts <- dataset
    conn  <- episerver_connect(driver = driver, max_attempts = max_attempts, ...)
    on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
            add = TRUE)

  } else {

    stop("The supplied arguments do not resolve to a valid labels file. Labels cannot be loaded")

  }

  target <- .lbl_target(parts, lbl_db, lbl_schema, lbl_table)
  .lbl_get(function(sql) DBI::dbGetQuery(conn, sql),
           target$db, target$schema, target$table, target$label)

}


# The database, schema and name of the table that a lazy table reads, from
# the first table path in its query
#' @noRd
.lbl_table_path <- function(dataset) {

  # Recursive function to find the 'x' with class 'dbplyr_table_path'
  find_dbplyr_table_path <- function(query_structure) {

    # Check if the current structure is a list and has an element named 'x'
    if (is.list(query_structure) && "x" %in% names(query_structure)) {

      # Access the 'x' element
      x_element <- query_structure$x

      # Check if the class of 'x' is 'dbplyr_table_path'
      if ("dbplyr_table_path" %in% class(x_element)) {
        return(x_element)
      }

      # If 'x' is a list, continue searching recursively
      return(find_dbplyr_table_path(x_element))
    }

    # Return NULL if no match is found
    return(NULL)
  }

  table_info <- find_dbplyr_table_path(dataset$lazy_query)

  # Remove any brackets, quotes and backquotes, and split by "."
  table_parts <- if (is.null(table_info)) character() else {
    strsplit(gsub('[]["`]', "", table_info), "\\.")[[1]]
  }

  if (length(table_parts) != 3) {
    stop("The structure of lazy_query does not match expected format. Labels cannot be applied.")
  }

  table_parts

}


# The table whose labels are wanted, from its database, schema and name
# (`parts`, from a lazy table) or its name alone (in the default database and
# schema), and the one label table that the lbl_* arguments name, if any, as
# c(db, schema, table), each part defaulting to the table's database, ref
# and DataLabels as before the labels lookup
#' @noRd
.lbl_target <- function(parts, lbl_db = NULL, lbl_schema = NULL, lbl_table = NULL) {

  if (length(parts) == 1) {
    parts <- c(episerver_serverdetails("default_db"),
               episerver_serverdetails("default_schema"), parts)
  }

  given <- !c(is_invalid(lbl_db), is_invalid(lbl_schema), is_invalid(lbl_table))
  label <- if (any(given)) {
    c(db     = if (given[1]) lbl_db else parts[1],
      schema = if (given[2]) lbl_schema else "ref",
      table  = if (given[3]) lbl_table else "DataLabels")
  }

  list(db = parts[1], schema = parts[2], table = parts[3], label = label)

}
