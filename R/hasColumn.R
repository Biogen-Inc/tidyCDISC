#' Check whether a column is found in a data set
#'
#' Checks whether a specified column is found in a specified data set
#'
#' @param columnName The column to look for.
#' @param data the data.frame to search.
#' @return logical scalar. TRUE if the column is found. FALSE otherwise
#'
#' @examples
#' safetyGraphics:::hasColumn(columnName="PARAM",data=adlbc) #TRUE
#' safetyGraphics:::hasColumn(columnName="Not_a_column",data=adlbc) #FALSE
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