# perform different operations based on class:

IDEA_freq <- function(column, week, group = NULL, data) {
 
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
