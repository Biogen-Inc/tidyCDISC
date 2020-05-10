# perform different mean operations based on class:

IDEA_mean <- function(column, week, method, group, data) {
  UseMethod("IDEA_mean", column)
}

IDEA_mean.default <- function(column, week, method) {
  abort(glue(
    "Can't calculate mean because data is not classified as ADLB, BDS or OCCDS",
  ))
}

# if ADSL supplied look for the column to take mean of
# and look for a grouping variable to group_by
IDEA_mean.ADSL <- function(column, week, group = NULL, data) {
  
  column <- sym(as.character(column))
  
  if (!is.numeric(data[[column]])) {
    stop(paste("Can't calculate mean, ", column, " is not numeric"))
  }
  
  if (!is.null(group)) {
    
    group <- sym(group)
    
    data %>%
      group_by(!!group) %>% 
      summarise(
        missing = sum(is.na(!!column)),
        mean = mean(na.omit(!!column)))
    
  } else {
    
    data %>%
      summarise(
        missing = sum(is.na(!!column)),
        mean = mean(na.omit(!!column)))
  }
}

# if BDS filter by paramcd and week
# We need to calculate the difference in N for this
# and report missing values from the mean if any
IDEA_mean.BDS <- function(column, week, group = NULL, data) {
  
  column <- sym(as.character(column))
  
  if (!is.numeric(data[[column]])) {
    stop(paste("Can't calculate mean, ", column, " is not numeric"))
  }
  
  if (!is.null(group)) {
    
    group <- sym(group)
    
    data %>%
      filter(AVISIT == week & PARAMCD == column) %>%
      group_by(!!group) %>% 
      summarise(
        n = n(),
        missing = sum(is.na(AVAL)),
        mean = mean(na.omit(AVAL)))
    
  } else {
    
    data %>%
      filter(AVISIT == week & PARAMCD == column) %>%
      summarise(
        n = n(),
        missing = sum(is.na(AVAL)),
        mean = mean(na.omit(AVAL)))
  }
}

IDEA_mean.OCCDS <- function(column, week = NULL) {
  print("OCCDS")
}

IDEA_mean.custom <- function(column, week = NULL) {
  abort(glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}
