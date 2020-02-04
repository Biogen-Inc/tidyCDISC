pivot_paramcd <- function(df_to_pivot) {
  # First are we pivoting AVAL or both AVAL and CHG
  ifelse(
    "CHG" %in% colnames(df_to_pivot),
    values <<- c("CHG", "AVAL"),
    values <<- "AVAL"
  )
  
  # Use the values to pivot by AVAL or AVAL and CHG
  df_to_pivot %>% 
    select(PARAMCD, USUBJID, !!values) %>%
    group_by(PARAMCD) %>%
    mutate(rn = row_number()) %>%
    ungroup %>%
    pivot_wider(names_from = PARAMCD, values_from = !!values) %>%
    select(-rn)
}