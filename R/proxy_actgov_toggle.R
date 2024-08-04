#' Toggle ACTGOV proxy
#' 
#' Useful for quickly setting Sys.setenv() proxy variables to the ACTGOV proxy server.
#' 
#' @param toggle The instruction to switch on or off.  Argument should be supplied as character. Valid values are 'on' or 'off'.
#' @param proxyaddress The proxy address.  Argument should be supplied as character.  If left null, a known default will be applied.  Not required for toggling off.
#' 
#' @keywords proxy
#' 
#' @export
#' 
#' @examples 
#' proxy_actgov_toggle("on")
#' 
proxy_actgov_toggle <- function(toggle, proxyaddress = NULL) {
  
  toggle = tolower(toggle)
  
  # check required arguments present
  if (!toggle %in% c("on","off") ) {
    stop("toggle must be set to 'on' or 'off'")
  }
  
  if(is_invalid(proxyaddress)){
    proxyaddress = "http://proxy.on.act.gov.au:9090"
  } 
  
  if (toggle == "off") {

    Sys.setenv(http_proxy       = "")
    Sys.setenv(http_proxy_user  = "")
    Sys.setenv(https_proxy      = "")
    Sys.setenv(https_proxy_user = "")
    
  } else if (toggle == "on") {
   
    Sys.setenv(http_proxy       = proxyaddress)
    Sys.setenv(http_proxy_user  = proxyaddress)
    Sys.setenv(https_proxy      = proxyaddress)
    Sys.setenv(https_proxy_user = proxyaddress)
    
  }
  
}
