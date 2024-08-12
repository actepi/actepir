#' Function to extract ACTHD colors as hex codes
#'
#' @param ... Character names of acthd_colors 
#'
#' @keywords internal
#' 
#' @export
.acthd_cols <- function(...) {
  
  acthd_colors <- .acthd_colors()
  
  cols <- c(...)
  
  if (is.null(cols))
    return (acthd_colors)
  
  acthd_colors[cols]
}
