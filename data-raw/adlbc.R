## code to prepare `adlbc` dataset goes here
adlbc <- haven::read_xpt("data-raw/adlbc.xpt")

# The analysis Date Variable is way off in the future, it's not even remotely
# close to occuring wtihin the confines of the study, so we're adjusting the
# ADT variable to be withing the treatment exposure start and end

# lab timeline
min_lb <- min(adlbc$ADT)
max_lb <- max(adlbc$ADT)
lb_dur <- max_lb - min_lb # duartion

# Treatment timeline
min_trt <- min(adsl$TRTSDT)
max_trt <- max(adsl$TRTEDT)
trt_dur <- max_trt - min_trt # duration

# creating a move variable, which tracks how many days have elapsed between the
# start of the trt and start of lab's drawn, plus the difference in duration
# days
move = min_lb - min_trt + (lb_dur - trt_dur)

# Subtract the 'move' var's days, overwriting ADT
adlbc <- adlbc %>%
  mutate(ADT = ADT - move )

usethis::use_data(adlbc, overwrite = TRUE)
