#' Check whether a column is found in a data set
#'
#' Checks whether a specified column is found in a specified data set
#'
#' @param columnName The column to look for.
#' @param data the data.frame to search.
#' @return logical scalar. TRUE if the column is found. FALSE otherwise
#'
#' @keywords internal

hasColumn <- function(columnName, data){
  stopifnot(
    typeof(columnName)=="character" || is.null(columnName),
    length(columnName)==1  || is.null(columnName),
    is.data.frame(data)
  )
  
  if(is.null(columnName)){
    return(FALSE)
  } else {
    return(toupper(columnName) %in% toupper(colnames(data)))  
  }
  
}