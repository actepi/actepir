#' Function return ACTHD color
#'
#' @param color Name of the colour to return. See [acthd_list_colors()] for
#'   available names.
#'
#' @export
acthd_get_color <- function(color="bgs blue") {
  
  getElement(actepir::.acthd_cols(),color)
  
}

