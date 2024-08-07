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
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr reframe
#' @importFrom dplyr row_number
#' @importFrom dplyr if_else
#' @importFrom purrr modify_if
#' @importFrom haven labelled
#' @importFrom magrittr %>%
#' 
#' @export
#' 
#' @examples 
#' # Two-step process: download labels first 
#' # and save to object, then apply labels as 
#' # last step in downloading data
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
    dplyr::mutate(rank=dplyr::row_number()) %>% 
    dplyr::filter(rank == 1) %>% 
    dplyr::ungroup() %>% 
    
    # standardise types
    dplyr::mutate(labeltype = tolower(labeltype)) %>% 
    dplyr::filter(labeltype %in% c("opt","var")) %>% 
    
    # roll up into lists
    dplyr::group_by(varname) %>%
    dplyr::reframe(
      variable_label = labelname[labeltype == "var"],
      labels = list(setNames(as.character(labelname[labeltype == "opt"]), as.numeric(datacode[labeltype == "opt"])))
    )
  
  # nullify zero-length character list (for vars with no value labels)
  label_info = label_info %>%
    mutate(
      labels = purrr::modify_if(labels, ~ length(.) == 0, ~ NA_character_),
      zeroflag = dplyr::if_else(is.na(labels),1,0)
    )
  
  # apply labels to data
  for (i in base::seq_along(label_info$varname)) {
    
    # isolate variable name for iteration
    var <- label_info$varname[i]
    
    # ensure variable name exists in dataset
    if (!var %in% names(data)) {
      next  # Skip variable if not exist in dataset
    }
    
    # grab variable label and value labels
    var_label <- label_info$variable_label[i]
    lbls <- label_info$labels[[i]]
    
    # check value label is not flagged as zero-length
    if (label_info$zeroflag[[i]] != 1) {
      
      # apply value labels, based on column data type
      if (is.numeric(data[[var]])) {
        
        lbls <- as.numeric(names(lbls))
        names(lbls) <- label_info$labels[[i]]
        data[[var]] <- haven::labelled(data[[var]], lbls)
        
      } else if (is.character(data[[var]])) {
        
        data[[var]] <- haven::labelled(data[[var]], setNames(names(lbls), lbls))
        
      }
      
    }
    
    # apply variable label
    base::attr(data[[var]], "label") <- var_label
    
  }
  return(data)
  
}