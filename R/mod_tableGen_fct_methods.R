#' Find the proper function to apply to 
#' each statistical and column block pairing
#' and use the metadata associated with 
#' each column block for the function's arguments
#' @param agg the statistic to apply given the block name
#' @param column the column to apply that statistic too,
#' and class of the column dictated by the data frame it came from
#' @param week the week if needed for calculation
#' @param group whether to perform a group_by and if so by which column
#' @param data the dataset to perform all functions on
#' 
#' @return the table corresponding to the proper function
#' to perform given the supplied column.
#' This is used within a map to apply to all blocks
#' inside the table generator module.

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