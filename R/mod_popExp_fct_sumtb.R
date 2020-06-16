#' Generate summary table 
#'
#' @param data a data frame
#' @param groupbox {T|F} Should groupbyvar be used?
#' @param groupbyvar var to group on
#' @param responsevar y-axis variable
#'
#' @return tb a tibble
#' 
#'   DO NOT REMOVE.
#' @import dplyr
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom rlang sym
#' @noRd
#' 
fnsummtab <- function(data, groupbox, groupbyvar, responsevar) {
  
  x_var <- as.name(groupbyvar)
  y_var <- as.name(responsevar)
  
  seltime <- select(data, ends_with("DY"), starts_with("AVIS")) 
  
  if (groupbox == TRUE){
    req(!is_empty(groupbyvar) && groupbyvar != "")
    tb <- data %>%
      dplyr::group_by(!!x_var)
  } else {
    tb <- data
  }
  
  # NOTE: use type=1 for quantile() definitions, to match SAS output
  tb <- tb %>%
    # using dplyr::summarise not Hmisc::summarize
    dplyr::summarise(
      N = n(),
      Min = min(!!y_var,na.rm = TRUE), 
      Q1 = quantile(!!y_var, 0.25, na.rm = TRUE, type=1), 
      Median = median(!!y_var, na.rm = TRUE, type=1), 
      Mean = round(mean(!!y_var,na.rm = TRUE),2),
      SD   = round(sd(!!y_var,na.rm = TRUE),3),
      Q3 = quantile(!!y_var, 0.75, na.rm = TRUE, type=1), 
      Max = max(!!y_var, na.rm = TRUE) ) 
  
  return(tb)
}
