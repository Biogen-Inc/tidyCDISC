df <-
  all_data() %>%
  distinct(USUBJID, !!ROW) %>%
  count(!!ROW) %>%
  group_by(!!ROW) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)) %>%
  mutate(x = paste0(n, " (", round(prop, 2), ")")) %>%
  select(!!ROW, x)

df <-
  all_data() %>%
  distinct(USUBJID, !!ROW, !!COLUMN) %>%
  count(!!ROW, !!COLUMN) %>%
  group_by(!!ROW) %>%
  mutate(prop = prop.table(n)) %>%
  mutate(v1 = paste0(n, ' (', round(prop, 2), ')')) %>%
  select(-n, -prop) %>% 
  spread(!!COLUMN, v1)