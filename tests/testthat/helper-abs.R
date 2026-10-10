# tests/testthat/helper-abs.R
#
# Skip for tests that download boundaries from the ABS boundary service. Like
# skip_if_no_episerver(), it decides by probing, never by detecting the
# environment: one small request for the state layer's description, through
# the download path and proxy settings the calculator uses, with the result
# cached for the test run.

abs_reachable <- local({

  cached <- NULL

  function() {

    if (!is.null(cached)) return(cached)

    withr::local_options(timeout = 10)
    ok <- tryCatch({
      meta <- .dsr_fetch_json(paste0(.dsr_abs_service, "/STE/MapServer/0?f=json"))
      length(meta$fields) > 0
    }, error = function(e) FALSE)

    cached <<- ok
    ok
  }

})

#' Skip unless the ABS boundary service answers
#' @noRd
skip_if_no_abs <- function() {

  if (!abs_reachable()) {
    testthat::skip("The ABS boundary service is not reachable from this machine")
  }

  invisible(TRUE)
}
