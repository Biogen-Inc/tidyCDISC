df <- all_data() %>%
  filter(PARAMCD == ROW & AVISIT == WEEK) %>%
  summarise(N = n(),
            `Mean (SD)` = paste0(round(mean(AVAL), 2), " (", round(sd(AVAL), 2), ")"),
            Median = median(AVAL),
            `Q1 | Q3` = paste(round(quantile(AVAL, 0.25, type = 2), 2) , "|", round(quantile(AVAL, 0.75, type = 2), 2)),
            `Min | Max` = paste(round(min(AVAL), 2), " | ", round(max(AVAL), 2)))

df <- all_data() %>%
  filter(PARAMCD == ROW & AVISIT == WEEK) %>%
  group_by(!!COLUMN) %>%
  summarise(N = n(),
            `Mean (SD)` = paste0(round(mean(AVAL), 2), " (", round(sd(AVAL), 2), ")"),
            Median = median(AVAL),
            `Q1 | Q3` = paste(round(quantile(AVAL, 0.25, type = 2), 2) , "|", round(quantile(AVAL, 0.75, type = 2), 2)),
            `Min | Max` = paste(round(min(AVAL), 2), " | ", round(max(AVAL), 2))
            
df <- all_data() %>%
              # if it's not a PARAMCD, it's from ADSL
              # which doesn't have an AVISIT column
              # filter(AVISIT == WEEK) %>%
              distinct(USUBJID, !!ROW, !!COLUMN) %>%
              filter(!is.na(!!ROW)) %>%
              group_by(!!COLUMN) %>%
              summarise(N = n(),
                        `Mean (SD)` = paste0(round(mean(!!ROW), 2), " (", round(sd(!!ROW), 2), ")"),
                        Median = median(!!ROW),
                        `Q1 | Q3` = paste(round(quantile(!!ROW, 0.25, type = 2),2) , "|", (round(quantile(!!ROW, 0.75, type = 2),2))),
                        `Min | Max` = paste0(round(min(!!ROW), 2), " | ", round(max(!!ROW), 2)))