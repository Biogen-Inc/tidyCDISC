fnoverplt <- function(df,groupvar) {
  # correction for overplotting
  # BDS records are usually by USUBJID, AVISIT, and PARAMCD
  # assume we are working with an ADaM BDS data set
  colnams <- c("PARAMCD","USUBJID","AVISIT")
  colindf <- sapply(colnams,function(c) c %in% colnames(df))
  if (!all(colindf)) {
    message("fnoverplt: data is not a BDS.  No changes.")
    return(df)
  }
  # restrict seltimevar to AVISIT, AVISITN, VSDY
  seltime <- select(df, ends_with("DY"), starts_with("AVIS"))
  
  if (length(seltime) == 0)  {
    message("fnoverplt: no seltime variables found.  No changes.")
    return(df)
  }
  
  # if not using AVISIT(n) then collapse to USUBJID level and set AVISIT to Baseline
  if (!groupvar %in% names(seltime) && "AVISIT" %in% names(df) && "USUBJID" %in% names(df)) {
    df <- df %>%
      dplyr::filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
      dplyr::distinct(USUBJID, .keep_all = TRUE)
  }     
  return(df)
}
