test_data <- readsRDS("test_data.RDS")
test_data_filtered <- readRDS("test_data_filtered.RDS")

##################################################
# Mean - I think the filter step is wrong?
##################################################

ROW <- sym("DIABP")
WEEK <- sym("Week 12")
COLUMN <- sym("TRT01P")

# 6
data_mean <- test_data %>% filter(PARAMCD == ROW & AVISIT == WEEK)

# 10

##################################################
# Mean - I think the filter step is wrong?
##################################################

ROW <- sym("SEX")
COLUMN <- sym("TRT01P")


# 14
data_freq_14 <- test_data %>% distinct(USUBJID, !!ROW)
  
# 15: Frequency of row filtered, same issue?
data_freq_14

# 16: Frequency of row, grouped
data_freq_16 <- test_data %>% distinct(USUBJID, !!ROW, !!COLUMN)

# 17: Frequency of row, grouped, and filtered - same issue?
data_freq_16


##################################################
# T-Test - I wonder if this is due to aov()
##################################################

# 18

# 19

# 20

# 21

##################################################
# CHG from Baseline - same issue as Mean and Freq?
##################################################

# 22 - same as data_mean!

# 25 - same as data_mean!


write.csv(data_mean, "data_mean.csv")
write.csv(data_freq_14, "data_freq_14.csv")
write.csv(data_freq_16, "data_freq_16.csv")