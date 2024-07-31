#'------------------------------------------------------------------------------
#' Switch off ACTGOV proxy
#' 
#' Useful for quickly nullifying Sys.setenv() proxy variables.
#' 
#' @keywords proxy
#' 
#' @export
#' 
#' @examples 
#' proxy_actgov_off()
#'------------------------------------------------------------------------------

proxy_actgov_off <- function() {

  Sys.setenv(http_proxy       = "")
  Sys.setenv(http_proxy_user  = "")
  Sys.setenv(https_proxy      = "")
  Sys.setenv(https_proxy_user = "")
  
}
