#' Refactor common variables by their "N" counterparts
#'
#' Neat trick to first set the selected choice to "0", then to character(0)
#' 
#' @param data A data frames
#' @param varc A character vector of variables names with character values
#' @param varn A character vector of variables names with numeric values
#' 
#' @import dplyr
#' @importFrom data.table := 
#' @importFrom forcats fct_reorder
#' 
refact <- function(data, varc, varn) {
  datac <- deparse(substitute(data))
  if (varc %in% colnames(data) && varn %in% colnames(data)) {
    #message(paste("A factor was created for", varc, "based on", varn, "levels"))
    data[, (varc) := forcats::fct_reorder(get(varc), get(varn))]
  } 
}