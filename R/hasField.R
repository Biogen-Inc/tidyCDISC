#' Check whether a specified field value is found in a data set
#'
#' This checks whether a specific value is found in a specified column in a specified data set
#'
#' @param fieldValue A value to check for.
#' @param columnName The column to check.
#' @param data the data.frame to search.
#' @return logical scalar. TRUE if field_value is found. FALSE otherwise
#'
#' @examples
#' safetyGraphics:::hasField(fieldValue="Bilirubin (umol/L)",columnName="PARAM",data=adlbc) #TRUE
#' safetyGraphics:::hasField(fieldValue="Not_a_real_value",columnName="",data=adlbc) #FALSE
#'
#' @keywords internal

hasField<- function(fieldValue, columnName, data){
  stopifnot(
    length(fieldValue)==1,
    typeof(columnName)=="character" || is.null(columnName),
    length(columnName)==1  || is.null(columnName), 
    is.data.frame(data)
  )
  
  if(is.null(columnName)){
    return(FALSE)
  } else {
    columnFound <- hasColumn(columnName=columnName, data=data)
    if(columnFound){
      validFields <- unique(data[[columnName]])
    } else{
      validFields <- c()
    }
    
    validFields <- unique(data[[columnName]])
    return(fieldValue %in% validFields)
  }
}