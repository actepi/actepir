#' Explore EpiServer Database Structure
#'
#' This function allows the user to explore the structure of databases on the
#' EpiServer. When called without a \code{dataset} argument, it returns a list
#' of all tables and views in the specified database and schema. When a
#' \code{dataset} is supplied, it returns column-level metadata for that table.
#'
#' @param db Character string. The database name to explore. Defaults to
#'   \code{"Analysis"}.
#' @param schema Character string. The schema name to explore. Defaults to
#'   \code{"dbo"}.
#' @param dataset Character string or \code{NULL}. The name of a specific table
#'   to inspect. If \code{NULL} (default), all tables in the schema are listed.
#' @param driver Character string or \code{NULL}. ODBC driver to use for the
#'   connection. If \code{NULL} (default), the function will automatically select
#'   the appropriate driver using \code{episerver_serverdetails("driver")}.
#' @param max_attempts Integer to specify the number of connection attempts.
#'   Defaults to \code{episerver_connect} default.
#'
#' @return A data frame. If \code{dataset} is \code{NULL}, a data frame with
#'   columns \code{table_catalog}, \code{table_schema}, \code{table_name}, and
#'   \code{table_type}. If \code{dataset} is supplied, a data frame with columns
#'   including \code{column_name}, \code{ordinal_position}, \code{data_type},
#'   \code{character_maximum_length}, \code{numeric_precision},
#'   \code{numeric_scale}, \code{is_nullable}, and \code{column_default}.
#'
#' @details
#' This function queries the \code{INFORMATION_SCHEMA} views on the EpiServer to
#' return structural metadata. It is intended as a convenience function for users
#' who need to explore what is available without writing SQL directly.
#'
#' When listing tables, both \code{BASE TABLE} and \code{VIEW} types are
#' returned. The results are ordered alphabetically by table name or by ordinal
#' column position, respectively.
#'
#' @note
#' \itemize{
#'   \item The user must have read access to the target database
#'   \item Results are limited to objects visible to the authenticated user
#'   \item The connection is opened and closed within the function
#' }
#'
#' @seealso
#' \code{\link{episerver_connect}} for establishing database connections,
#' \code{\link{episerver_lazytable}} for creating lazy table references
#'
#' @keywords episerver database metadata
#'
#' @importFrom DBI dbGetQuery dbDisconnect dbIsValid
#'
#' @inheritDotParams episerver_connect encrypt trust_certificate
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # List all tables in the default Analysis.dbo namespace
#' episerver_info()
#'
#' # List all tables in a different schema
#' episerver_info(schema = "ref")
#'
#' # Get column metadata for a specific table
#' episerver_info(dataset = "APC")
#'
#' # Explore table columns in a non-default database
#' episerver_info(db = "Avicenna", dataset = "LGBTIQA")
#' }
#'
#' @author Warren Holroyd
#'
episerver_info <- function(db = "Analysis", schema = "dbo", dataset = NULL,
                           driver = NULL, max_attempts = NULL, ...) {
  
  # Establish connection
  connect_args <- list()
  if (!is.null(driver)) connect_args$driver <- driver
  if (!is.null(max_attempts)) connect_args$max_attempts <- max_attempts
  conn <- do.call(episerver_connect, c(connect_args, list(...)))
  
  # Ensure connection is closed on exit
  on.exit({
    if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn)
  })
  
  if (is_invalid(dataset)) {
    
    # ── Table listing mode ──────────────────────────────────────────────
    sql <- sprintf(
      "SELECT
         TABLE_CATALOG   AS table_catalog,
         TABLE_SCHEMA    AS table_schema,
         TABLE_NAME      AS table_name,
         TABLE_TYPE      AS table_type
       FROM [%s].INFORMATION_SCHEMA.TABLES
       WHERE TABLE_SCHEMA = '%s'
       ORDER BY TABLE_NAME",
      db, schema
    )
    
  } else {
    
    # ── Column metadata mode ────────────────────────────────────────────
    sql <- sprintf(
      "SELECT
         TABLE_NAME                AS Dataset,
         COLUMN_NAME               AS VarName,
         ORDINAL_POSITION          AS Position,
         DATA_TYPE                 AS DataType,
         CHARACTER_MAXIMUM_LENGTH  AS CharLen,
         NUMERIC_PRECISION         AS NumPrecision,
         NUMERIC_SCALE             AS NumScale
       FROM [%s].INFORMATION_SCHEMA.COLUMNS
       WHERE TABLE_SCHEMA = '%s'
         AND TABLE_NAME   = '%s'
       ORDER BY ORDINAL_POSITION",
      db, schema, dataset
    )
    
  }
  
  result <- DBI::dbGetQuery(conn, sql)
  
  # Warn if nothing returned
  if (nrow(result) == 0) {
    if (is_invalid(dataset)) {
      warning(
        sprintf("No tables found in [%s].[%s]. Check the database/schema name or your permissions.", db, schema),
        call. = FALSE
      )
    } else {
      warning(
        sprintf("No columns found for [%s].[%s].[%s]. Check the table name or your permissions.", db, schema, dataset),
        call. = FALSE
      )
    }
  }
  
  return(result)
  
}