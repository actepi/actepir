#' Function to store/return ACTHD palettes
#'
#' @keywords internal
#' 
#' @details
#' Palettes are derived from a standard set of colours, sourced from the [ACT Health Directorate Brand & Graphics Standards Manual](https://actgovernment.sharepoint.com/sites/intranet-ACTHealth/SitePages/Branding,%20Templates%20and%20resources.aspx) (May 2024) or derived from the [ACT Website Design System](https://github.com/ACTGov-Design-System/ACT-Website-Design-System).
#' 
#' ## Valid palette names
#' 
#' **Official ACTHD primary colour gradients:** 
#' -  `blue_white`
#' -  `blue_turq`
#' -  `blue_darkgrey`
#' 
#' **Official ACTHD secondary colours:** 
#' -  `spectral`
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
#' -  `web_purple_midgrey`
#' -  `web_purple_darkgrey`
#' -  `web_greys`
#' -  `web_blues`
#' 
#' **Unofficial (necessary for some visualisation/mapping):** 
#' -  `lightgreys`
#' -  `fullgreys`
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
#' @export
.acthd_palettes <- function(palette = "blue_turq", reverse = FALSE, ...) {
  
  palettes <- list(
    # Official listed in Brand & Graphics Standards Manual
    # ACTHD primary gradients
    `blue_white`           = .acthd_cols("bgs blue", "bgs white"),
    `blue_turq`            = .acthd_cols("bgs blue", "bgs turq"),
    `blue_darkgrey`        = .acthd_cols("bgs blue", "bgs darkgrey"),
    
    # ACTHD secondary colours
    `spectral`             = .acthd_cols("bgs blue", "bgs aqua", "bgs turq", "bgs green", "bgs yellow", "bgs orange", "bgs red", "bgs purple"),
    
    # ACTGOV gradients
    `darkgreys`            = .acthd_cols("bgs darkgrey", "bgs midgrey"),
    `navy_pink`            = .acthd_cols("bgs navy", "bgs purple"),
    `red_orange`           = .acthd_cols("bgs red", "bgs orange"),
    `green_yellow`         = .acthd_cols("bgs green", "bgs yellow"),
    `pink_red`             = .acthd_cols("bgs purple", "bgs red"),
    
    # Semi-official (implied from web standards)
    `web_purples`          = .acthd_cols("web purple", "web lightpurple"),
    `web_purple_lightgrey` = .acthd_cols("web purple", "web lightgrey"),
    `web_purple_midgrey`   = .acthd_cols("web purple", "bgs midgrey"),
    `web_purple_darkgrey`  = .acthd_cols("web purple", "web darkgrey"),
    
    `web_greys`            = .acthd_cols("web darkgrey", "web lightgrey"),
    `web_blues`            = .acthd_cols("bgs blue", "web lightblue"),
    
    # Unofficial (necessary for some visualisation/mapping)
    `lightgreys`           = .acthd_cols("bgs midgrey", "web lightgrey"),    
    `fullgreys`            = .acthd_cols("bgs darkgrey", "web lightgrey"),
    `red_yellow_green`     = .acthd_cols("bgs red", "bgs yellow", "bgs green"),
    `red_yellow_aqua`      = .acthd_cols("bgs red", "bgs yellow", "bgs aqua"),
    `blue_lightgrey`       = .acthd_cols("bgs blue", "web lightgrey"),
    `turq_lightgrey`       = .acthd_cols("bgs turq", "web lightgrey"),
    `turq_white`           = .acthd_cols("bgs turq", "bgs white")
    )
  
  return(palettes)
  
}
