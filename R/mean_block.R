mean_block <- function(data, row, week, column, mean_by) {
  df <- data %>%
    filter(PARAMCD == row & AVISIT == week) %>%
    group_by(!!column) %>%
    summarise(N = n(),
              `Mean (SD)` = paste0(round(mean(!!mean_by), 2), " (", round(sd(!!mean_by), 2), ")"),
              Median = median(!!mean_by),
              `Q1 | Q3` = paste(round(quantile(!!mean_by, 0.25), 2) , "|", round(quantile(!!mean_by, 0.75), 2)),
              `Min | Max` = paste(round(min(!!mean_by), 2), " | ", round(max(!!mean_by), 2)))
  return(df)
}