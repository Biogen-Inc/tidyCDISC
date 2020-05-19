mean_summary <- function(.data, to_count) {
  
  to_count <- sym(to_count)
  
  .data %>%
    summarise(
      Missing = sum(is.na(!!to_count)),
      Mean = round(mean(na.omit(!!to_count)), 2),
      SD = round(sd(na.omit(!!to_count)), 2),
      Median = median(na.omit(!!to_count)),
      `Q1 | Q3` = paste(round(quantile(na.omit(!!to_count), 0.25, type = 2),2) , "|", 
                        (round(quantile(na.omit(!!to_count), 0.75, type = 2),2))),
      `Min | Max` = paste0(round(min(na.omit(!!to_count)), 2), " | ", 
                           round(max(na.omit(!!to_count)), 2))
    )
}