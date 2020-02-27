intermediate <- all_data() %>%
  filter(PARAMCD == ROW & AVISIT == WEEK) %>%
  summarise(mean = round(mean(CHG), 3))

# as above but now we group by the column block
intermediate <- all_data() %>%
  group_by(!!COLUMN) %>% 
  filter(AVISIT == WEEK & PARAMCD == ROW) %>%
  summarise(mean = round(mean(CHG), 3))