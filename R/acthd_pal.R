#' Return function to interpolate ACTHD color palette
#'
#' @param palette Character name of palette in acthd_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @importFrom grDevices colorRampPalette
#' 
acthd_pal <- function(palette = "blue_turq", reverse = FALSE, ...) {
  
  acthd_palettes <- acthd_palettes()
  
  pal <- acthd_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  grDevices::colorRampPalette(pal, ...)
}
