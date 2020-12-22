#' Refactor variables
#'
#' Refactor variables by their common "N" counterparts
#' 
#' @param data A data frames
#' @param varc A character vector of variables names with character values
#' @param varn A character vector of variables names with numeric values
#' 
#' @import dplyr
#' @importFrom data.table := 
#' @importFrom forcats fct_reorder
#' 
#' @family popExp Functions
#' 
refact <- function(data, varc, varn) {
  
  datac <- deparse(substitute(data))
  if (varc %in% colnames(data) && varn %in% colnames(data)) {
    #message(paste("A factor was created for", varc, "based on", varn, "levels"))
    # if not a factor yet, make it a factor, then re-order
    if(!is.factor(data[,(varc)])) {data[, (varc) := as.factor(get(varc))]}
    # If a level was dropped, then don't reorder the factor
     if(length(levels(data[[varc]])) == length(unique(data[[varc]]))){
       data[, (varc) := forcats::fct_reorder(get(varc), get(varn))] 
     }
  } 
}







