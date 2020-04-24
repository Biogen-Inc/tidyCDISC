fnoverplt <- function(df,axis_var,groupvar) {
  # correction for overplotting, 
  # when you have a BDS with multiple AVISIT records and the user selected only ADSL variables
  # BDS records are usually by USUBJID, AVISIT, and PARAMCD
  # assume we are working with an ADaM BDS data set

  colnams <- c("PARAMCD","USUBJID","AVISIT")
  colindf <- sapply(colnams,function(c) c %in% colnames(df))
  if (!all(colindf)) {
    message("fnoverplt: data is not a BDS.  No changes.")
    return(df)
  }
  # seltimevar to include *DY. AVIS*, AVAL*, CHG
  seltime <- select(df, ends_with("DY"), starts_with("AVIS"), starts_with("AVAL"), any_of(c("CHG")) )
  
  if (length(seltime) == 0)  {
    message("fnoverplt: no seltime variables found.  No changes.")
    return(df)
  }
  
  message(paste("fnoverplot nrows before",nrow(df)))
  
  # if not using any of the words in seltime above for the axis or grouping variable
  # then collapse to USUBJID level and set AVISIT to Baseline
  if (!groupvar %in% names(seltime) && 
      !axis_var %in% names(seltime) &&
      "AVISIT" %in% names(df) && "USUBJID" %in% names(df)) {
    df <- df %>%
      # set AVISIT to baseline, like it came from ADSL
      dplyr::filter(AVISIT == "Baseline") %>% 
      dplyr::distinct(USUBJID, .keep_all = TRUE)
  }   
  
  message(paste("fnoverplot nrows  after",nrow(df)))
  
  return(df)
}
