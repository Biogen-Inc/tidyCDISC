# perform different mean operations based on class:

IDEA_freq <- function(column, week, method, group, data) {
  UseMethod("IDEA_freq", column)
}

IDEA_freq.default <- function(column, week, method) {
  abort(glue(
    "Can't calculate mean because data is not classified as ADLB, BDS or OCCDS",
  ))
}

# if ADSL supplied look for the column to take mean of
# and look for a grouping variable to group_by
IDEA_freq.ADSL <- function(column, week, group = NULL, data) {
  column <- sym(as.character(column))
  
  if (is.null(group)) {
    data %>%
      distinct(USUBJID, !!column) %>%
      count(!!column) %>%
      group_by(!!column) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>%
      mutate(prop = n/sum(n)) %>%
      mutate(x = paste0(n, " (", round(prop*100, 2), ")")) %>%
      select(!!column, x)
  } else {
    group <- sym(group)
    data %>%
      distinct(USUBJID, !!column, !!group) %>%
      count(!!column, !!group) %>%
      group_by(!!group) %>%
      mutate(prop = prop.table(n)) %>%
      mutate(v1 = paste0(n, ' (', round(prop*100, 2), ')')) %>%
      select(-n, -prop) %>% 
      spread(!!group, v1)
  }
}

# if BDS filter by paramcd and week
# We need to calculate the difference in N for this
# and report missing values from the mean if any
IDEA_freq.BDS <- function(column, week, group = NULL, data) {
  abort(glue(
    "Can't calculate frequency for BDS - {column} is numeric"
  ))
}

IDEA_freq.OCCDS <- function(column, week = NULL) {
  print("OCCDS")
}

IDEA_freq.custom <- function(column, week = NULL) {
  abort(glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}
