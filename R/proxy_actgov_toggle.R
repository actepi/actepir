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
  
  proxy_vars <- c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY", "ALL_PROXY")
  user_vars <- c("http_proxy_user", "https_proxy_user")
  
  if (toggle == "off") {

    for(i in 1:length(proxy_vars)) {
      do.call(Sys.setenv, list("") |> `names<-`(proxy_vars[i]))
    }
    for(i in 1:length(user_vars)) {
      do.call(Sys.setenv, list("") |> `names<-`(user_vars[i]))
    }
    
  } else if (toggle == "on") {
    
    # Set proxy variables programmatically
    for(i in 1:length(proxy_vars)) {
      do.call(Sys.setenv, list(proxyaddress) |> `names<-`(proxy_vars[i]))
    }
    for(i in 1:length(user_vars)) {
      do.call(Sys.setenv, list(":") |> `names<-`(user_vars[i]))
    }
    
  }
  
}
