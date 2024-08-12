#' Color scale constructor for ACT Health Directorate palettes
#' 
#' This function allows the user to apply pre-defined colour palettes to a ggplot color argument.
#' 
#' @inherit .acthd_palettes details
#'
#' @param palette Character name of ACT palette. See 'details' for valid palette names.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param ... Additional arguments passed to `discrete_scale()` or `scale_color_gradientn()`, used respectively when discrete is `TRUE` or `FALSE`.
#'
#' @keywords gpplot2
#'
#' @importFrom ggplot2 discrete_scale
#' @importFrom ggplot2 scale_fill_gradientn
#' 
#' @export
#' 
#' @examples 
#' # Color by numeric variable with web-compliant purples palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'   geom_point(size = 4, alpha = .6) +
#'   acthd_ggplot_color(
#'     palette = "web_purples", 
#'     discrete = FALSE
#'     )
#'   
#' # Fill by discrete variable with different palette + remove legend (guide)
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'   geom_bar() +
#'   acthd_ggplot_color(
#'     palette = "spectral", 
#'     guide = "none"
#'     )
#'      
acthd_ggplot_color <- function(palette = "blue_turq", discrete = TRUE, reverse = FALSE, ...) {
  
  pal <- .acthd_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("acthd_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}
