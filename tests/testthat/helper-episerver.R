# tests/testthat/helper-episerver.R
#
# Skips for tests that need something only the ACT Government network provides.
# Both decide by probing, never by detecting the environment, so one mechanism
# covers a cloud session, CI and an EpiServer outage on an analyst's machine.

# TCP probe of the EpiServer host and port, cached for the test run.
# A closure holds the result so the probe costs at most one timeout per run.
episerver_reachable <- local({

  cached <- NULL

  function() {

    if (!is.null(cached)) return(cached)

    host <- episerver_serverdetails("server")
    port <- suppressWarnings(as.integer(episerver_serverdetails("port")))

    ok <- FALSE
    if (!is.na(host) && nzchar(host) && !is.na(port)) {
      # Unresolvable host, refused port and dead route all land in try-error;
      # timeout caps the dead-route case at 3 seconds.
      con <- try(
        suppressWarnings(
          socketConnection(host = host, port = port, server = FALSE,
                           blocking = TRUE, open = "r+", timeout = 3)
        ),
        silent = TRUE
      )
      if (!inherits(con, "try-error")) {
        close(con)
        ok <- TRUE
      }
    }

    cached <<- ok
    ok
  }

})

#' Skip unless EpiServer answers on its port
#' @noRd
skip_if_no_episerver <- function() {

  if (!episerver_reachable()) {
    testthat::skip("EpiServer is not reachable from this machine")
  }

  invisible(TRUE)
}

#' Skip unless a SQL Server ODBC driver is installed
#'
#' episerver_serverdetails("driver") returns NA when odbcListDrivers() holds no
#' SQL Server driver, which is the case off the ACT Government SOE.
#' @noRd
skip_if_no_sqlserver_driver <- function() {

  driver <- episerver_serverdetails("driver")

  if (is.na(driver) || !nzchar(driver)) {
    testthat::skip("No SQL Server ODBC driver is installed")
  }

  invisible(TRUE)
}
