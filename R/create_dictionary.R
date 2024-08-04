#' Create a data dictionary
#' 
#' This function allow the users to extract variable and value labels into a flattened dataframe structure.
#' 
#' It assumes that the labels conform to the haven_labelled class and uses the `sjlabelled::get_label` and `sjlabelled::get_labels` functions.  This is a convenience wraparound function.
#' 
#' @param x The dataframe of data containing labels. Argument should be supplied as an object.
#' @param vars The column in the dataframe that you wish to limit the extraction to. Argument should be supplied as an object.
#' 
#' @keywords labels
#' 
#' @importFrom sjlabelled get_label
#' @importFrom sjlabelled get_labels
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_split
#' @importFrom magrittr %>%
#' 
#' @export
#' 
#' @examples 
#' # Get flattened dictionary from already-labelled dataset
#' dictionary = create_dictionary(df)
#' 
#' # Limit dictionary to a single variable name
#' dictionary = create_dictionary(df,"gender")
#' 
#' # Limit dictionary to a list of variable names
#' dictionary = create_dictionary(df,c("gender","status"))
#-------------------------------------------------------------------------------
create_dictionary = function(x,vars=NULL){

  #vars = deparse(substitute(vars))
  
  output = x %>% 
    sjlabelled::get_label() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("variable") %>% 
    dplyr::rename(variable_label=".") %>% 
    dplyr::left_join(
      x %>% 
        sjlabelled::get_labels(values = "as.name") %>% 
        base::unlist() %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column("variable") %>% 
        dplyr::rename(value_label=".") %>% 
        dplyr::mutate(
          value = stringr::str_split_i(variable,"\\.",-1),
          variable = sub('\\.[^\\.]*$', '', variable)
        ),
      by="variable"
    )
  
  if (length(vars)>0){
    
    output = output %>% 
      dplyr::filter(variable %in% vars) 
    
  }
  
  return(output)
  
}

