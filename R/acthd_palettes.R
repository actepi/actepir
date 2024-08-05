#' Function to store/return ACTHD palettes
#'
acthd_palettes <- function(palette = "blue_turq", reverse = FALSE, ...) {
  
  palettes <- list(
    # Official listed in Brand & Graphics Standards Manual
    # ACTHD primary gradients
    `blue_white`    = acthd_cols("bgs blue", "bgs white"),
    `blue_turq`     = acthd_cols("bgs blue", "bgs turq"),
    `blue_darkgrey` = acthd_cols("bgs blue", "bgs darkgrey"),
    
    # ACTGOV gradients
    `darkgreys`    = acthd_cols("bgs darkgrey", "bgs midgrey"),
    `navy_pink`    = acthd_cols("bgs navy", "bgs purple"),
    `red_orange`   = acthd_cols("bgs red", "bgs orange"),
    `green_yellow` = acthd_cols("bgs green", "bgs yellow"),
    `pink_red`     = acthd_cols("bgs purple", "bgs red"),
    
    # Semi-official (implied from web standards)
    `web_purples`          = acthd_cols("web purple", "web lightpurple"),
    `web_purple_lightgrey` = acthd_cols("web purple", "web lightgrey"),
    `web_purple_darkgrey`  = acthd_cols("web purple", "web darkgrey"),
    `web_greys`            = acthd_cols("web darkgrey", "web lightgrey"),
    
    # Unofficial (necessary for some visualisation/mapping)
    `lightgreys`       = acthd_cols("bgs midgrey", "web lightgrey"),    
    `fullgreys`        = acthd_cols("bgs darkgrey", "web lightgrey"),
    `spectral`         = acthd_cols("bgs blue", "bgs aqua", "bgs turq", "bgs green", "bgs yellow", "bgs orange", "bgs red", "bgs purple"),
    `red_yellow_green` = acthd_cols("bgs red", "bgs yellow", "bgs green"),
    `red_yellow_aqua`  = acthd_cols("bgs red", "bgs yellow", "bgs aqua"),
    `blue_lightgrey`   = acthd_cols("bgs blue", "web lightgrey"),
    `turq_lightgrey`   = acthd_cols("bgs turq", "web lightgrey"),
    `turq_white`       = acthd_cols("bgs turq", "bgs white")
    )
  
  return(palettes)
  
}
