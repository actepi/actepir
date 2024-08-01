#' Bulk apply labels to data
#' 
#' This function will allow the user to apply value and variable labels in bulk to a dataset.
#' 
#' It assumes that the labelset is formatted according to those stored on EpiServer.  Columns should be:
#' 
#' "VarName" : the variable name
#' "VarType" : indicating whether the row specifies a variable or response option (value) label. an  Valid values in this column are "var" (for variable label) and "opt" (for value label)
#' "DataCode" : containing the underlying code (level)
#' "LabelName" : the variable or value label
#' 
#' All columns should be character.  VarNames must be in the same case as the column names in the data they are to be applied to.
#' 
#' @param data The dataframe of data which the labels will be applied to. Argument should be supplied as an object.
#' @param labels The dataframe containing the labels to be applied. Argument should be supplied as an object.
#' 
#' @keywords labels
#' 
#' @import haven
#' @import tidyr
#' @import dplyr
#' @import purrr
#' @import magrittr
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
#' @examples 
#' # Two-step process: download labels first and save to 
#' # object, then apply labels as last step in downloading data
#' lab = episerver_getlabels("APC")
#' dat = episerver_connect("APC") %>% 
#'   collect() %>% 
#'   applydatalabels(lab)
#' 
applydatalabels <- function(data = parent.frame(), labels = NULL) {
  
  # set names to lower
  names(labels) <- base::tolower(names(labels))
  
  # clean and prepare labels file
  label_info = labels %>% 
    
    # remove duplicate entries
    dplyr::group_by(varname,labeltype,datacode) %>% 
    dplyr::mutate(rank=row_number()) %>% 
    dplyr::filter(rank == 1) %>% 
    dplyr::ungroup() %>% 
    
    # standardise types
    dplyr::mutate(labeltype = tolower(labeltype)) %>% 
    dplyr::filter(labeltype %in% c("opt","var")) %>% 
    
    # roll up into lists
    dplyr::group_by(varname) %>%
    dplyr::summarize(
      variable_label = labelname[labeltype == "var"],
      labels = list(setNames(as.character(labelname[labeltype == "opt"]), as.numeric(datacode[labeltype == "opt"])))
    )
  
  # apply labels to data
  for (i in seq_along(label_info$varname)) {
    var <- label_info$varname[i]
    
    if (!var %in% names(data)) {
      next  # Skip if the variable does not exist in the dataset
    }
    
    var_label <- label_info$variable_label[i]
    lbls <- label_info$labels[[i]]
    
    if (is.numeric(data[[var]])) {
      lbls <- as.numeric(names(lbls))
      names(lbls) <- label_info$labels[[i]]
      data[[var]] <- haven::labelled(data[[var]], lbls)
    } else if (is.character(data[[var]])) {
      data[[var]] <- haven::labelled(data[[var]], setNames(names(lbls), lbls))
    }
    
    attr(data[[var]], "label") <- var_label
  }
  return(data)
  
}