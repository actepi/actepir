#' Color scale constructor for ACT Health Directorate palettes
#' 
#' This function allows the user to apply pre-defined theme to a ggplot object.
#'
#' @param flip Boolean to indicate if the ggplot object has had `coord_flip()` applied to it.
#'
#' @keywords gpplot2
#'
#' @import ggplot2
#' 
#' @export
#' 
#' @examples 
#' # Normal theme application
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'   geom_bar() +
#'   coord_flip() +
#'   acthd_ggplot_theme(flip=TRUE)
#'   
#' # Flipped application
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'   geom_bar() +
#'   coord_flip() +
#'   acthd_ggplot_theme(flip=TRUE)
#'  
acthd_ggplot_theme <- function(flip=FALSE) {
  
  thm = theme(
    
    # Overall settings
    text = element_text(color = "#333740"),
    title = element_text(face = "bold", size = 10),
    line = element_line(color = "#e6e5e5"),
    
    # Title
    plot.title = element_text(
      color = "#333740", hjust = 0, face = "bold", size = 14,
      margin = margin(0,0,0.75,0,"cm")
    ),
    
    # Legend
    legend.position = "right", 
    legend.title=element_blank(),
    
    # Plot and borders
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    
    panel.background = element_rect(fill = "#FFFFFF"),
    #axis.line.x = element_line(linetype = "solid"),
    
    # Gridlines
    panel.grid = element_line(color = "#e6e5e5", linetype = "solid"),
    
    # Axis elements
    axis.title = element_text(face = "bold", size = 9),
    axis.text  = element_text(size = 9),
    
    # Axis ticks
    axis.ticks = element_line(color = "#e6e5e5"),
    #axis.ticks.x = element_blank()
    #axis.ticks.y = element_blank()
    
    # X (vertical grids)
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    axis.ticks.x        = element_blank(),
    
    # Y (horizontal grids)
    panel.grid.major.y  = element_line(),
    panel.grid.minor.y  = element_line(),
    axis.ticks.y        = element_line()
  )
  
  if (flip) {
    
    thm = thm + theme(
      
      # X (vertical grids)
      panel.grid.major.x  = element_line(),
      panel.grid.minor.x  = element_line(),
      axis.ticks.x        = element_line(),
      
      # Y (horizontal grids)
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.y  = element_blank(),
      axis.ticks.y        = element_blank()
    )
    
  }
  
  return(thm)
  
}