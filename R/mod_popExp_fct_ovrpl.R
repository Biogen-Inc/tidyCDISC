#' Check for overplotting 
#'
#' @param data a data frame
#' @param axis_var x- or y- axis variable
#' @param groupbyvar var to group on
#'
#' @return data updated data frame
#' 
#'   DO NOT REMOVE.
#' @import dplyr
#' @importFrom dplyr %>%
#' @importFrom rlang sym
#' @noRd
#' 
fnoverplt <- function(data,axis_var,groupbyvar) {
  # correction for overplotting, 
  # when you have a BDS with multiple AVISIT records and the user selected only ADSL variables
  # BDS records are usually by USUBJID, AVISIT, and PARAMCD
  # assume we are working with an ADaM BDS data set
  
  colnams <- c("PARAMCD","USUBJID","AVISIT")
  colindf <- sapply(colnams,function(c) c %in% colnames(data))
  if (!all(colindf)) {
    message("fnoverplt: data is not a BDS.  No changes.")
    return(data)
  }
  # seltimevar to include *DY. AVIS*, AVAL*, CHG
  seltime <- select(data, ends_with("DY"), starts_with("AVIS"), starts_with("AVAL"), any_of(c("CHG")) )
  
  if (length(seltime) == 0)  {
    message("fnoverplt: no seltime variables found.  No changes.")
    return(data)
  }
  
  message(paste("fnoverplot nrows before",nrow(data)))
  
  # if not using any of the words in seltime above for the axis or grouping variable
  # then collapse to USUBJID level and set AVISIT to Baseline
  if (!groupbyvar %in% names(seltime) && 
      !axis_var %in% names(seltime) &&
      "AVISIT" %in% names(data) && "USUBJID" %in% names(data)) {
    data <- data %>%
      # set AVISIT to baseline, like it came from ADSL
      dplyr::filter(AVISIT == "Baseline") %>% 
      dplyr::distinct(USUBJID, .keep_all = TRUE)
  }   
  
  message(paste("fnoverplot nrows  after",nrow(data)))
  
  return(data)
}
