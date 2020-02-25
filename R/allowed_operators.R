allowed_operators <- c(">", ">=", "==", "<=", "<", "!=") %>% 
  set_names() %>% 
  map(match.fun)