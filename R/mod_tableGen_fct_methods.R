IDEA_methods <- function(agg, column, week, group, data) {
  if (agg == "MEAN") {
    IDEA_mean(column, week, group, data)
  } else if (agg == "FREQ") {
    IDEA_freq(column, week, group, data)
  } else if (agg == "ANOVA") {
    IDEA_anova(column, week, group, data)
  } else {
    IDEA_chg(column, week, group, data)
  }
}