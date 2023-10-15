#' Automatic factor conversion
#' 
#' This function will allow the user to convert a column to a factor and automatically set levels based on a supplied data dictionary. 
#' The dictionary must conform to the DataLabels structure used on EpiServer.
#' @param dfdata DATAFRAME. The dataframe containing the column(s) requiring factor conversion
#' @param dflabels DATAFRAME. The dataframe containing the data dictionary/labels
#' @param vars STRING VECTOR. A vector containing the column names that should be factor converted
#' @param order TRUE/FALSE. Should the resulting factor levels be ordered?
#' @param suffix STRING. Suffix to the resulting factor column name to distinguish it from the original. Not supplying will overwrite the original column.
#' @param nolabel STRING. The name of the level which unmatched levels are assigned to.
#' @keywords autofactor
#' @export
#' @examples 
#' varlist <- c("var1","var2")
#' mydata <- mydata %>% 
#'   autofactor(
#'     dflabels = mylabelsdf,
#'     vars = varlist,
#'     order = TRUE,
#'     suffix = "_X",
#'     nolabel = "UL"
#'     )

autofactor <- function(dfdata = parent.frame(), dflabels, vars, order=NULL, suffix=NULL, nolabel=NULL) {
  
  stopifnot(is.data.frame(dflabels))
  stopifnot(is.character(vars))
  
  if(is.null(order)) order <- FALSE
  stopifnot(is.logical(order))
  
  if(is.null(suffix)) suffix <- ""            # set default suffix if not supplied (will factor original var)
  stopifnot(is.character(suffix))
  
  if(is.null(nolabel)) {                      # set default label for unlabeled if not supplied, else use supplied
    nl.label <- "_unlabeled"
  } else {
    stopifnot(is.character(nolabel) | is.na(nolabel))
    nl.label <- nolabel
  }
  
  names(dflabels) <- tolower(names(dflabels)) # ensure lowercase column names on dflabel
  
  dflabels <- subset(dflabels, subset =       # subset/filter dflabels to only the var(s) supplied in function call 
                       varname %in% vars 
                     & toupper(labeltype) == 'OPT' 
                     & toupper(datatype) == 'INT'
  )
  
  dflabels <- dflabels[with(dflabels, order(
    varname
    ,as.numeric(datacode)
    )), ]                                     # now order dataset (non-INT may need adjusting for)
  
  for (i in 1:length(vars)) {                 # loop through supplied vars vector...
    
    if(vars[i] %in% dflabels$varname) {  
      
      # define the old and new factor column name
      oldcol = paste0(vars[i])
      newcol = paste0(vars[i],suffix)
      
      # subset again to specific var
      tmplabels <- subset(dflabels, subset = varname==vars[i]) 
      
      # get a list of values with no associated label
      nona   <- unique(na.omit(dfdata[[oldcol]]))              # unique list of non-NA values
      nolabs <- unique(nona[!nona %in% tmplabels$datacode])    # list of non-NA values without a label
      
      # if unlabelled data detected...
      if(length(nolabs)>0) {
        
        # ...create new var and collapse unlabelled into Inf
        dfdata[[newcol]] <- ifelse(dfdata[[oldcol]] %in% nolabs, Inf, dfdata[[oldcol]])
        message("The values (",paste(noquote(nolabs),collapse=', '),") in '",oldcol,"' have no associated label and were set to '",nl.label,"' in '",newcol,"'")
        
        # apply factor using labels data for specific var WITH an unlabelled group
        dfdata[[newcol]] <- factor(
          dfdata[[newcol]], 
          ordered = order,
          levels = c(tmplabels$datacode, Inf),
          labels = c(tmplabels$labelname, nl.label)
        )
        
      } else {
        
        # apply factor using labels data for specific var WITHOUT an unlabelled group
        dfdata[[newcol]] <- factor(
          dfdata[[oldcol]], 
          ordered = order,
          levels = tmplabels$datacode,
          labels = tmplabels$labelname
        )  
        
      }
      
      # supply confirmation message
      txt <- paste0("The specified column name '",oldcol,"' was factor converted to '",newcol,"'")
      msg <- paste0("\033[0;", 32, "m",txt,"\033[0m")
      message(msg)
      
    } else {
      
      warn <- paste0("The specified column name '",oldcol,"' could not be found in the supplied dictionary")
      warning(warn, call.=FALSE, immediate.=TRUE)
      
    }
    
  }
  
  return(dfdata)
  
}
