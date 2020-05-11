# perform different mean operations based on class:

IDEA_anova <- function(column, week, method, group, data) {
  UseMethod("IDEA_mean", column)
}

IDEA_anova.default <- function(column, week, method) {
  abort(glue(
    "Can't calculate mean because data is not classified as ADLB, BDS or OCCDS",
  ))
}

# if ADSL supplied look for the column to take mean of
# and look for a grouping variable to group_by
IDEA_anova.ADSL <- function(column, week, group = NULL, data) {
  
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
IDEA_anova.BDS <- function(column, week, group = NULL, data) {
  
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

IDEA_anova.OCCDS <- function(column, week = NULL) {
  print("OCCDS")
}

IDEA_anova.custom <- function(column, week = NULL) {
  abort(glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}



  #     # withot a column block the t-test block returns NA
  #     # because you can't write a t-test without a comparison!
  #     if (COLUMN == "") {
  #       
  #       d <- data.frame(TTEST = NA)
  #       colnames(d) <- paste0("Total (N  = ", total(), ")")
  #       row.names(d) <- "T-Test"
  #       
  #     } else {
  #       # get the total Ns for each column group
  #       # to use within the column names below
  #       
  #       header_df <- 
  #         all_data() %>%
  #         distinct(USUBJID, !!COLUMN) %>%
  #         group_by(!!COLUMN) %>%
  #         summarise(count = n())
  #       
  #       # take the filtered data, then filter even further
  #       # by removing NAs and using only the week selected from dropdown
  #       if (as.character(ROW) %in% PARAMCD_names()) {
  #         all_dat <- all_data() %>%  filter(PARAMCD == ROW & AVISIT == WEEK)
  #         ttest <- broom::tidy(aov(all_dat$AVAL ~ all_dat[[paste(COLUMN)]], data=all_dat))
  #       } else {
  #         all_dat <- all_data() %>% distinct(!!ROW, !!COLUMN, USUBJID)
  #         ttest <- broom::tidy(aov(all_dat[[paste(ROW)]] ~ all_dat[[paste(COLUMN)]], data=all_dat))
  #       }
  #       
  #       # use broom for an ANOVA
  #       
  #       # need to make a dataframe with length of columns 
  #       # and the values are NA except for the last column which is the p.value
  #       
  #       # Group 1 | Group 2 | Group 3
  #       #   NA        NA      p-value
  #       
  #       len = length(unique(all_data()[[COLUMN]])) - 1
  #       d <- data.frame(t(data.frame("X" = c(rep(" ", len), round(ttest$p.value[1], 3)),
  #                                    "Y" = c(rep(" ", len), round(ttest$meansq[1], 3)))))
  #       row.names(d) <- c("P-Value", "Mean Squared")
  #       colnames(d) <- lapply(paste0(unlist(header_df[,1]), " (N = ", unlist(header_df[,2]), ")"), CapStr) 
  #       
  #     }
  #     # set datalist for ttest to either 
  #     # if there was a column selected or not
  #     # add an empty row into the dataframe
  #     # with the row block name as its value
  #     # append the rowname dataframe to actual data above
  #     insert <- data.frame(t(data.frame("X" = c(rep(" ", length(d))))))
  #     
  #     if (as.character(ROW) %in% PARAMCD_names()) {
  #       row.names(insert) <- paste0(CapStr(as.character(ROW)), ": ", WEEK)
  #     } else {
  #       row.names(insert) <- paste0(CapStr(as.character(ROW)))
  #     }
  #     
  #     colnames(insert) <- colnames(d)
  #     data <- rbind(insert, d)
  #     colnames(data) <- lapply(colnames(data), CapStr)
  #     datalist[[i]] <- rownames_to_column(data, var = "row_name")
  #     