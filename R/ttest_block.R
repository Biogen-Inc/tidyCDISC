
# take the filtered data, then filter even further
# by removing NAs and using only the week selected from dropdown
if (as.character(ROW) %in% PARAMCD_names()) {
  all_dat <- all_data() %>%  filter(PARAMCD == ROW & AVISIT == WEEK)
  ttest <- broom::tidy(aov(all_dat$AVAL ~ all_dat[[paste(COLUMN)]], data=all_dat))
} else {
  all_dat <- all_data() %>% filter(AVISIT == WEEK)
  ttest <- broom::tidy(aov(all_dat[[paste(ROW)]] ~ all_dat[[paste(COLUMN)]], data=all_dat))
}
