#' Function to store/return ACTHD colors
#'
#' @export
.acthd_colors <- function() {
  
  colors <- c(
    `bgs blue`     = "#002677", 
    `bgs turq`     = "#78D5E1", 
    `bgs yellow`   = "#dfcb21", 
    `bgs orange`   = "#f26c23",
    `bgs red`      = "#ce1e25",
    `bgs green`    = "#9fc13a",
    `bgs aqua`     = "#00828c",
    `bgs purple`   = "#aa4298",
    `bgs navy`     = "#22397e",
    `bgs darkgrey` = "#333740",
    
    `bgs midgrey`  = "#929487",  # taken from last pixel in Brand & Graphics Standards Manual 'grey' gradient
    `bgs white`    = "#FFFFFF",
    
    `web purple`      = "#472d8c",  # sourced from website 05/08/2024
    `web lightpurple` = "#f4f2fa",
    `web lightblue`   = "#d9e5ff",
    `web lightgrey`   = "#e6e5e5",
    `web darkgrey`    = "#4b4b4b"
    )
  
  return(colors)
  
}
