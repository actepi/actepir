#' Auto format ggplot object
#' 
#' This function allows the user to apply pre-defined theme to a ggplot object.
#'
#' @param x `gt()` object to have theme applied to it.
#'
#' @keywords gpplot2 acthd
#'
#' @import ggplot2
#' 
#' @export
#' 
#' @examples 
#' # Two-step application
#' output = mpg |> 
#'   ggplot(aes(x = manufacturer, y=hwy)) +
#'   geom_col()
#' 
#' output |> 
#'   acthd_ggplot_autotheme()
#'   
#' # Single-step application
#' # Two-step application
#' { output = mpg |> 
#'     ggplot(aes(x = manufacturer, y=hwy)) +
#'     geom_col()
#' } |> 
#'   acthd_ggplot_autotheme()
#'  
acthd_ggplot_autotheme <- function(x) {
  
  plot = base::sapply(x$layers, function(layer) class(layer$geom)[1]) 
  
  is_flipped = base::inherits(x$coordinates, "CoordFlip")
  
  result = x + theme(
    
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
    legend.position = "bottom", 
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
    
  )

  # gridlines
  if(is_flipped){
    
    result = result + theme(
      # X (vertical grids)
      panel.grid.major.x  = element_line(),
      panel.grid.minor.x  = element_line(),
      axis.ticks.x        = element_line(),
      # Y (horizontal grids)
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.y  = element_blank(),
      axis.ticks.y        = element_blank()
    )
    
  } else {
    
    result = result + theme(
      # X (vertical grids)
      panel.grid.major.x  = element_blank(),
      panel.grid.minor.x  = element_blank(),
      axis.ticks.x        = element_blank(),
      # Y (horizontal grids)
      panel.grid.major.y  = element_line(),
      panel.grid.minor.y  = element_line(),
      axis.ticks.y        = element_line()
    )
    
  }
  
  if (any(plot %in% c("GeomPoint"))) {
    result = result + geom_point(shape=16, size = 2.5)
  }

  if (any(plot %in% c("GeomLine"))) {
    result = result + geom_line(linewidth=0.5)
  }
  
  if (any(plot %in% c("GeomArea"))) {
    result = result + geom_area(alpha = 0.3, linewidth=0.5)
  }

  if (any(plot %in% c("GeomTile"))) {
    
    result = result + theme(
      # remove all grids
      panel.grid.major.x  = element_blank(),
      panel.grid.minor.x  = element_blank(),
      axis.ticks.x        = element_blank(),
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.y  = element_blank(),
      axis.ticks.y        = element_blank()
      ) +
      
      # set standard tile style
      geom_tile(color = "#FFF")
    
  }
  
  # if (any(plot %in% c("GeomBar","GeomCol","GeomHistogram","GeomLine"))) {
  #   
  # }
  
  # if (any(plot %in% c("GeomPoint"))) {
  #   
  #   result = result + theme(
  #     # X (vertical grids)
  #     panel.grid.major.x  = element_line(),
  #     panel.grid.minor.x  = element_line(),
  #     axis.ticks.x        = element_line(),
  #     
  #     # Y (horizontal grids)
  #     panel.grid.major.y  = element_line(),
  #     panel.grid.minor.y  = element_line(),
  #     axis.ticks.y        = element_line()
  #   )
  #   
  # }
  
  return(result)
}

