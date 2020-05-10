mean_summary <- function(.data, to_count) {
  
  to_count <- sym(to_count)
  
  .data %>%
    summarise(
      N = n(),
      Missing = sum(is.na(!!to_count)),
      Mean = round(mean(na.omit(!!to_count)), 2),
      SD = round(sd(na.omit(!!to_count)), 2),
      Median = median(na.omit(!!to_count)),
      `Q1 | Q3` = paste(round(quantile(na.omit(!!to_count), 0.25, type = 2),2) , "|", 
                        (round(quantile(na.omit(!!to_count), 0.75, type = 2),2))),
      `Min | Max` = paste0(round(min(na.omit(!!to_count)), 2), " | ", 
                           round(max(na.omit(!!to_count)), 2))
    )
}

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
  
  column <- as.character(column)
  
  if (!is.numeric(data[[column]])) {
    stop(paste("Can't calculate mean, ", column, " is not numeric"))
  }
  
  if (!is.null(group)) {
    group <- sym(group)
    data %>%
      group_by(!!group) %>% 
      mean_summary(column)
  } else {
    data %>%
      mean_summary(column)
  }
  
}

# if BDS filter by paramcd and week
# We need to calculate the difference in N for this
# and report missing values from the mean if any
IDEA_mean.BDS <- function(column, week, group = NULL, data) {
  
  column <- as.character(column)
  
  if (!column %in% data[["PARAMCD"]]) {
    stop(paste("Can't calculate mean, ", column, " has no AVAL"))
  }
  
  if (!is.null(group)) {
    group <- sym(group)
    data %>%
      filter(AVISIT == week & PARAMCD == column) %>%
      group_by(!!group) %>%
      mean_summary("AVAL")
  } else {
    data %>%
      filter(AVISIT == week & PARAMCD == column) %>%
      mean_summary("AVAL")
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
