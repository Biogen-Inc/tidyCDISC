# data table function -- rkrajcik
# parameters:
#    df = data frame, splitby = {T|F}, splitvar = var to group on, respvar = response variable
#    returns: summarized DT::datatable
fnsummtab <- function(data, splitby, splitvar, respvar) {
  
  x_var <- as.name(splitvar)
  y_var <- as.name(respvar)
  
  seltime <- select(data, ends_with("DY"), starts_with("AVIS")) 
  
  if (!splitvar %in% names(seltime) & "AVISIT" %in% names(data) & "USUBJID" %in% names(data)) {
    data.1 <- data %>%
      filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
      distinct(USUBJID, .keep_all = TRUE)
  } else {
    data.1 <- data
  }
  
  if (splitby == TRUE){
    req(splitvar != " ")
    table1 <- data.1 %>%
      dplyr::group_by(!!x_var)
  } else {
    table1 <- data.1
  }

  # NOTE: use type=1 for quantile() definitions, to match SAS output
  table1 <- table1 %>%
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
  
  return(table1)
}
