#' Function return ACTHD color
#'
#' @export
acthd_get_color <- function(color="bgs blue") {
  
  getElement(actepir::.acthd_cols(),color)
  
}

