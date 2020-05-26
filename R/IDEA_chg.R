# perform different mean operations based on class:

IDEA_chg <- function(column, week, method, group, data) {
  UseMethod("IDEA_chg", column)
}

IDEA_chg.default <- function(column, week, method) {
  abort(glue(
    "Can't calculate mean because data is not classified as ADLB, BDS or OCCDS",
  ))
}

# if ADSL supplied look for the column to take mean of
# and look for a grouping variable to group_by
IDEA_chg.ADSL <- function(column, week, group = NULL, data) {
  abort(glue(
    "Can't calculate change from baseline, {column} from ADSL - please choose a variable from BDS"
  ))
}

# if BDS filter by paramcd and week
# We need to calculate the difference in N for this
# and report missing values from the mean if any
IDEA_chg.BDS <- function(column, week, group = NULL, data) {
  
  column <- as.character(column)
  
  if (!column %in% data[["PARAMCD"]]) {
    stop(paste("Can't calculate change from baseline, ", column, " has no CHG values"))
  }
  
  if (!is.null(group)) {
    
    if (week == "NONE") {
      stop("Please select a week from CHG dropdown to calculate the change from baseline for ", column)
    }
    
    group <- sym(group)
    data %>%
      filter(AVISIT == week & PARAMCD == column) %>%
      group_by(!!group) %>%
      mean_summary("CHG") %>%
      transpose_df(1)
  } else {
    data %>%
      filter(AVISIT == week & PARAMCD == column) %>%
      mean_summary("CHG") %>%
      transpose_df(999)
  }
}

IDEA_chg.OCCDS <- function(column, week = NULL) {
  abort(glue(
    "Can't calculate change from baseline of OCCDS"
  ))
}

IDEA_chg.custom <- function(column, week = NULL) {
  abort(glue(
    "Can't calculate change from baseline, data is not classified as ADLB, BDS or OCCDS"
  ))
}
