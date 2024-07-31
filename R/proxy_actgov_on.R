#' Switch on ACTGOV proxy
#' 
#' Useful for quickly setting Sys.setenv() proxy variables to the ACTGOV proxy server.
#' 
#' @param proxyaddress The proxy address.  Argument should be supplied as character.  If left null, a known default will be applied.
#' 
#' @keywords proxy
#' 
#' @export
#' 
#' @examples 
#' proxy_actgov_on()

proxy_actgov_on <- function(proxyaddress = NULL) {
  
  if(is.null(proxyaddress)){
    proxyaddress = "http://proxy.act.gov.au:9090"
  }
  
  Sys.setenv(http_proxy       = proxyaddress)
  Sys.setenv(http_proxy_user  = proxyaddress)
  Sys.setenv(https_proxy      = proxyaddress)
  Sys.setenv(https_proxy_user = proxyaddress)
  
}
