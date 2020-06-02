# perform different mean operations based on class:

IDEA_anova <- function(column, week, group, data) {
  UseMethod("IDEA_anova", column)
}

IDEA_anova.default <- function(column, week, group, data) {
  abort(glue(
    "Can't calculate mean because data is not classified as ADLB, BDS or OCCDS",
  ))
}

# if ADSL supplied look for the column to take mean of
# and look for a grouping variable to group_by
IDEA_anova.ADSL <- function(column, week, group = NULL, data) {
  
  column <- as.character(column)
  
  if (!is.numeric(data[[column]])) {
    stop(paste("Can't calculate ANOVA, ", column, " is not numeric"))
  }
  
  if (is.null(group)) {
    
    stop(paste("Can't calculate ANOVA without grouping data"))
    
  } else {
    group <- sym(group)
    column <- sym(column)
    all_dat <- data %>% distinct(!!column, !!group, USUBJID)
    broom::tidy(aov(all_dat[[paste(column)]] ~ all_dat[[paste(group)]], data=all_dat))
  }
  
}

# if BDS filter by paramcd and week
# We need to calculate the difference in N for this
# and report missing values from the mean if any
IDEA_anova.BDS <- function(column, week, group = NULL, data) {
  
  column <- as.character(column)
  
  if (!column %in% data[["PARAMCD"]]) {
    stop(paste("Can't calculate ANOVA, ", column, " has no AVAL"))
  }
  
  if (week == "NONE") {
    stop(paste("Select week from dropdown to calculate ANOVA"))
  }
  
  
  if (is.null(group)) {
    stop(paste("Can't calculate ANOVA without grouping data"))
  } else {
    group <- sym(group)
    
    all_dat <- data %>%  filter(PARAMCD == column & AVISIT == week)
    
    if (length(unique(all_dat[[paste(group)]])) == 1) stop(glue::glue("Only one {group} in data selected, choose another week for ANOVA"))
    
    ttest <- broom::tidy(aov(all_dat$AVAL ~ all_dat[[paste(group)]], data=all_dat))
    
    group_n <- length(unique(all_dat[[paste(group)]])) + 1
    
    anova_df <- data.frame(matrix(NA, ncol=group_n, nrow=4))
    anova_df[1,1] <- "p-value"
    anova_df[2,1] <- "Test Statistic"
    anova_df[3,1] <- "Mean Sum of Squares"
    anova_df[4,1] <- "Sum of Squares"
    anova_df[1, group_n] <- round(ttest$p.value[1], 3)
    anova_df[2, group_n] <- round(ttest$statistic[1], 2)
    anova_df[3, group_n] <- round(ttest$meansq[1], 2)
    anova_df[4, group_n] <- round(ttest$sumsq[1], 2)
    
    anova_df <- mutate_all(anova_df, as.character) %>%
      mutate_all(coalesce, "")
  }
  
}

IDEA_anova.OCCDS <- function(column, week = NULL) {
  print("OCCDS")
}

IDEA_anova.custom <- function(column, week = NULL) {
  abort(glue(
    "Can't calculate ANOVA, data is not classified as ADLB, BDS or OCCDS"
  ))
}