#' Collect in dbplyr pipe with labels
#'
#' This function is a wraparound function which combines the `episerver_getlabels` and `dplyr::collect` functions and feeds them to the `applydatalabels` function.  It will allow the user to collect the result of a lazy query with associated labels when the query is placed against an EpiServer location.  The function will always search EpiServer and cannot be used for other servers of file locations.
#'
#' The labels are those of the table the lazy query reads, from the first label table of its database that holds rows for it, as in the Dataset Browser (see [episerver_getlabels()]). Variable labels and value labels are applied; notes and the labels of linked tables are not. Almost all non-administrative users will not need to use the lbl_* arguments.
#'
#' @param dataset A lazy query on an EpiServer table, such as one from
#'   \code{\link{episerver_quickconnect}} or \code{\link{episerver_lazytable}}.
#' @inheritParams episerver_getlabels
#'
#' @keywords episerver
#'
#' @importFrom dplyr collect
#'
#' @inheritDotParams episerver_connect encrypt trust_certificate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # One-step process: apply labels as part of collect()
#' # statement when a dbplyr lazy object is passed to it
#' dat = episerver_quickconnect("APC") %>%
#'   collect_withlabels()
#'
#' # One-step process: apply labels as part of collect()
#' # statement but source labels from one label table
#' dat = episerver_quickconnect("APC", db = "HospitalAPC") %>%
#'   collect_withlabels(
#'     lbl_table = "DataLabelsNew",
#'     lbl_schema = "dbo",
#'     lbl_db = "HospitalAPC"
#'     )
#' }
#'
collect_withlabels <- function(dataset, lbl_db = NULL, lbl_schema = NULL, lbl_table = NULL, driver = NULL, max_attempts = NULL, ...) {

  return(
    applydatalabels(
      data   = dplyr::collect(dataset),
      labels = episerver_getlabels(dataset, lbl_table, lbl_schema, lbl_db,driver=driver,max_attempts=max_attempts, ...)
    )
  )

}
