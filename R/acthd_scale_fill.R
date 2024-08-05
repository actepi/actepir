#' Fill scale constructor for ACT Health Directorate palettes
#' 
#' This function allows the user to apply pre-defined colour palettes to a ggplot fill argument.
#' 
#' Palettes are derived from a standard set of colours, sourced from the [ACT Health Directorate Brand & Graphics Standards Manual](https://actgovernment.sharepoint.com/sites/intranet-ACTHealth/SitePages/Branding,%20Templates%20and%20resources.aspx) (May 2024) or derived from the [ACT Website Design System](https://github.com/ACTGov-Design-System/ACT-Website-Design-System).
#' 
#' ## Valid palette names
#' 
#' **Official ACTHD palettes:** 
#' -  `blue-white`
#' -  `blue-turq`
#' -  `blue-darkgrey`
#' 
#' **Official ACTGOV gradients:** 
#' -  `darkgreys`
#' -  `navy_pink`
#' -  `red_orange`
#' -  `green_yellow`
#' -  `pink_red`
#' 
#' **Semi-official (implied from web standards):** 
#' -  `web_purples`
#' -  `web_purple_lightgrey`
#' -  `web_purple_darkgrey`
#' -  `web_greys`
#' 
#' **Unofficial (necessary for some visualisation/mapping):** 
#' -  `lightgreys`
#' -  `fullgreys`
#' -  `spectral`
#' -  `red_yellow_green`
#' -  `red_yellow_aqua`
#' -  `blue_lightgrey`
#' -  `turq_lightgrey`
#' -  `turq_white`
#'
#' Users should stick to the official colours where possible, using the unofficial colours only when the underlying data necessitates it.
#' 
#' Approach taken from [Dr Simon Jackson](https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2).
#'
#' @param palette Character name of ACT palette. See 'details' for valid palette names.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param ... Additional arguments passed to `discrete_scale()` or `scale_fill_gradientn()`, used respectively when discrete is `TRUE` or `FALSE`.
#'
#' @keywords gpplot2
#'
#' @importFrom ggplot2 discrete_scale
#' @importFrom ggplot2 scale_fill_gradientn
#' 
#' @export
#' 
#' @examples 
#  # Fill by discrete variable with different palette + remove legend (guide)
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'   geom_bar() +
#'   scale_fill_acthd(
#'     palette = "spectral", 
#'     guide = "none"
#'     ) +
#'   theme_minimal()
#'
acthd_scale_fill <- function(palette = "blue_turq", discrete = TRUE, reverse = FALSE, ...) {
  pal <- acthd_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("acthd_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}