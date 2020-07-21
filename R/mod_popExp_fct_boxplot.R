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

IDEA_boxplot <- function(data, yvar, group, value = NULL, points = FALSE) {
  
  if (yvar %in% colnames(data)) {
    p <- ggplot2::ggplot(data) + 
      ggplot2::aes_string(x = group, y = yvar) +
      ggplot2::ylab(attr(data[[yvar]], "label"))
    
    var_title <- paste(attr(data[[yvar]], 'label'), "by", attr(data[[group]], "label"))
    
  } else {
    d <- data %>% dplyr::filter(PARAMCD == yvar)
    
    var_label <- paste(unique(d$PARAM))
    var_title <- paste(var_label, "by", attr(data[[group]], "label"))
    
    p <- d %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = group, y = value) +
      ggplot2::ylab(glue::glue("{var_label} ({attr(data[[value]], 'label')})"))
  }
  
  p <- p + 
    ggplot2::geom_boxplot() +
    ggplot2::xlab("") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.title = element_text(size = 16)) +
    ggplot2::ggtitle(var_title)
  
  
  if (points) { p <- p + ggplot2::geom_jitter() }
  return(p)
}
