#' Refactor common variables by their "N" counterparts
#'
#' Neat trick to first set the selected choice to "0", then to character(0)
#' 
#' @param data A data frames
#' @param varc A character vector of variables names with character values
#' @param varn A character vector of variables names with numeric values
#' 
#' @import dplyr
#' @importFrom data.table := 
#' @importFrom forcats fct_reorder
#' 
refact <- function(data, varc, varn) {
  datac <- deparse(substitute(data))
  if (varc %in% colnames(data) && varn %in% colnames(data)) {
    message(paste("A factor was created for", varc, "based on", varn, "levels"))
    data[, (varc) := forcats::fct_reorder(get(varc), get(varn))]
  } 
}

#' IDEA boxplot
#' 
#' Create boxplot using either the selected response variable
#' or if a PARAMCD is selected, then plot the corresponding value
#' and filter the data by week
#' 
#' @param data Merged data to be used in plot
#' @param yvar Selected y-axis 
#' @param group Selected x-axis 
#' @param value If yvar is a PARAMCD then the user must select 
#' AVAL, CHG, or BASE to be plotted on the y-axis
#' @param points \code{logical} whether to add a jitter to the plot
#' 
#' @family popExp Functions

IDEA_boxplot <- function(data, yvar, group, value, points = FALSE) {
  if (yvar %in% colnames(data)) {
    p <- ggplot2::ggplot(data) + 
      ggplot2::aes_string(x = group, y = yvar)
  } else {
    p <- data %>% 
      dplyr::filter(PARAMCD == yvar) %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = group, y = value)
  }
  
  p <- p + 
    ggplot2::geom_boxplot() +
    ggplot2::xlab("") +
    ggplot2::theme(text = element_text(size = 20),
                   axis.text.x = element_text(size = 20),
                   axis.text.y = element_text(size = 20)) +
    ggplot2::theme_bw()
  
  if (points) { p <- p + ggplot2::geom_jitter() }
  return(p)
}

#' IDEA scatterplot
#' 
#' Create scatter plot where if the variables are numeric then they
#' are plotted, and if they are PARAMCD's then a week and value 
#' must be selected for plotting.
#' 
#' @param data Merged data to be used in plot
#' @param xvar Selected x-axis 
#' @param week_x Selected x axis week if \code{xvar} is a PARAMCD
#' @param value_x Selected x-axis value if \code{xvar} is a PARAMCD: 
#' either AVAL, CHG, or BASE
#' @param yvar Selected xy-axis 
#' @param week_y Selected y-axis week if \code{yvar} is a PARAMCD
#' @param value_y Selected y-axis value if \code{yvar} is a PARAMCD: 
#' either AVAL, CHG, or BASE
#' @param separate whether to facet plots by categorical or factor
#' @param color whether to color plots by categorical or factor
#' 
#' @family popExp functions

IDEA_scatterplot <- function(data, yvar, xvar, week_x, value_x, week_y, value_y, separate = "NONE", color = "NONE") {
  # x and y are numeric columns
  if (yvar %in% colnames(data) & xvar %in% colnames(data)) {
    p <- ggplot2::ggplot(data) + 
      ggplot2::aes_string(x = xvar, y = yvar) +
      ggplot2::geom_point()
    # y numeric, x is paramcd 
  } else if (yvar %in% colnames(data) & !xvar %in% colnames(data)) {
    p <- data %>% 
      dplyr::filter(PARAMCD == xvar) %>%
      dplyr::filter(AVISIT == week_x) %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = value_x, y = yvar) +
      ggplot2::geom_point()
    # x numeric, y paramcd
  } else if (!yvar %in% colnames(data) & xvar %in% colnames(data)) {
    p <- data %>% 
      dplyr::filter(PARAMCD == yvar) %>%
      dplyr::filter(AVISIT == week_y) %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = xvar, y = value_y) +
      ggplot2::geom_point()
    # both paramcds
  } else {
    y_dat <- data %>%
      dplyr::filter(PARAMCD == yvar & AVISIT == week_y) %>%
      dplyr::select(USUBJID, PARAMCD, value_y, one_of(color, separate)) %>%
      tidyr::pivot_wider(names_from = PARAMCD, values_from = value_y) %>%
      tidyr::unnest(yvar)
    y_dat
    
    x_dat <- data %>%
      dplyr::filter(PARAMCD == xvar & AVISIT == week_x) %>%
      dplyr::select(USUBJID, PARAMCD, value_x, one_of(color, separate)) %>%
      tidyr::pivot_wider(names_from = PARAMCD, values_from = value_x) %>%
      tidyr::unnest(xvar)
    x_dat
    
    p <-
      y_dat %>%
      inner_join(x_dat) %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = yvar, y = xvar) +
      ggplot2::geom_point()
    
  }
  print(p)
  p <- p + 
    ggplot2::theme(text = element_text(size = 20),
                   axis.text = element_text(size = 20)) +
    ggplot2::theme_bw()
  
  if (separate != "NONE") { p <- p + ggplot2::facet_wrap(as.formula(paste(".~", separate))) }
  if (color != "NONE") { p <- p + ggplot2::aes_string(color = color)}
  return(p)
}

#' IDEA spaghetti plot
#' 
#' Create a spaghetti plot with a time dependent variable as the x-axis
#' and using either the selected response variable
#' or if a PARAMCD is selected, then plot the corresponding value. 
#' Lines are plotted by patient
#' 
#' @param data Merged data to be used in plot
#' @param yvar Selected y-axis 
#' @param time Selected x-axis constained to time dependent columns
#' @param value If yvar is a PARAMCD then the user must select 
#' AVAL, CHG, or BASE to be plotted on the y-axis
#' 
#' @family popExp Functions
IDEA_spaghettiplot <- function(data, yvar, time, value) {
  if (yvar %in% colnames(data)) {
    p <- ggplot2::ggplot(data) + 
      ggplot2::aes_string(x = time, y = yvar, group = "USUBJID")
  } else {
    p <- data %>% 
      dplyr::filter(PARAMCD == yvar) %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = time, y = value, group = "USUBJID") 
  }
  
  p <- p + 
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme(text = element_text(size = 20),
                   axis.text = element_text(size = 20)) +
    ggplot2::theme_bw()
  
  return(p)
}