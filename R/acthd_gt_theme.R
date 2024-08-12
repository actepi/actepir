#' Color theme for gt tables using ACT Health Directorate palettes
#' 
#' This function allows the user to apply pre-defined theme to a `gt` object.  Palettes are intentionally designed from dark to light (meaning the darker colour will fill the table header), with the exception of `spectral`.  This function takes the minimum and maximum colours in a palette and reduces them to two values.  Applying the `reverse` argument may not lead to desirable results as tables can look odd with light headers and dark row lines.  Applying the `single` argument will reduce the palette to a single colour.  The upper end of the palette (generally darker) is used by default.  Applying the `reverse` argument will use the lighter end of the palette. Text is automatically inverted when `reverse` is applied.
#' 
#' @inherit .acthd_palettes details
#' 
#' @param x The `gt` object to be themed
#' @param palette Character name of ACT palette. See 'details' for valid palette names.
#' @param single Boolean indicating whether the palette should be reduced to a single colour
#' @param reverse Boolean indicating whether the palette should be reversed.
#'
#' @keywords gt table acthd palette color
#'
#' @import gt
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr case_match
#' 
#' @export
#' 
#' @examples 
#' # Normal theme application
#' my_gt_table |>
#'   acthd_gt_theme()
#'   
#' # Reversed application
#' my_gt_table |>
#'   acthd_gt_theme(palette = "web_purples", reverse = TRUE)
#'  
acthd_gt_theme <- function(x, palette = "web_blues", single = FALSE, reverse = FALSE) {
  
  # Access palette list
  p = acthd_list_palettes(detail="all",objtype = "df") |>
    dplyr::filter(palette_name == palette)
  
  # take minimum and maximum colours
  c1 = p |> 
    dplyr::filter(
      color_order == dplyr::case_match(reverse, 
                                       FALSE ~ base::min(color_order), 
                                       TRUE ~ base::max(color_order)) 
      ) |> 
    dplyr::select(hex_code) |>
    dplyr::pull()
  
  c2 = p |> 
    dplyr::filter(
      color_order == dplyr::case_match(reverse, 
                                       FALSE ~ base::max(color_order), 
                                       TRUE ~ base::min(color_order)) 
      ) |> 
    dplyr::select(hex_code) |>
    dplyr::pull()
  
  # Apply single colour uniformity, if requested
  if (single) {
    c2 = c1
  }
  
  # Invert header text colour, if requested
  if (reverse) {
    htc = "#333333"
  } else (
    htc = "#FFFFFF"
  )
  
  # create gt theme
  x |>
    
    # Set the header row(s) to c1 value with no borders except the bottom
    tab_style(
      style = list(
        cell_fill(color = c1),
        cell_text(color = htc),
        cell_borders(sides = "bottom", color = "transparent", weight = px(1)),
        cell_borders(sides = "top", color = "transparent", weight = px(1)),
        cell_borders(sides = "left", color = "white", weight = px(0)),
        cell_borders(sides = "right", color = "white", weight = px(0))
      ),
      locations = cells_column_labels(columns=everything())
      ) |>
    
    # Set white vertical lines between each column in the body
    tab_style(
      style = list(
        cell_borders(sides = "left", color = "white", weight = px(1)),
        cell_borders(sides = "right", color = "white", weight = px(1))
      ),
      locations = cells_body(columns = everything())
      ) |>
    
    # Set horizontal lines between rows to c2 value
    tab_style(
      style = list(
        cell_borders(sides = "bottom", color = c2, weight = px(1)),
        cell_borders(sides = "top", color = "transparent", weight = px(0))
      ),
      locations = cells_body(rows = everything())
      ) |>
    
    # Remove external borders for the body
    tab_style(
      style = list(
        cell_borders(sides = c("top", "left", "right"), color = "transparent", weight = px(0))
      ),
      locations = cells_body(columns = everything())
      ) |>
    
    # Align the first column to the left
    cols_align(
      align = "left",
      columns = 1
      ) |>
    
    # Align all other columns to the right
    cols_align(
      align = "auto",
      columns = 2:last_col()
      ) |>
    
    # Set global table definitions: vertical padding to the header and horizontal padding
    tab_options(
      column_labels.padding = px(12),
      data_row.padding = px(8),
      table.font.size = px(12)
    ) |> 
    opt_horizontal_padding(scale = 2.0)
  
}