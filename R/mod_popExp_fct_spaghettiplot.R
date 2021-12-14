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
#' @export
#' @keywords popEx
#' 
app_spaghettiplot <- function(data, yvar, time, value = NULL) {
  if (yvar %in% colnames(data)) {
    
    # initialize plot
    p <- ggplot2::ggplot(data) + 
      ggplot2::aes_string(x = time, y = yvar, group = "USUBJID") +
      ggplot2::ylab(attr(data[[yvar]], "label")) +
      ggplot2::xlab(attr(data[[time]], "label"))
    
    # initialize title with variables plotted
    var_title <- paste(attr(data[[yvar]], 'label'), "by", attr(data[[time]], "label"))
    
  } else {
    
    # Filter data based on param var selected
    d <- data %>% dplyr::filter(PARAMCD == yvar) 
    
    # initialize title with variables plotted
    var_label <- paste(unique(d$PARAM))
    var_title <- paste(var_label, "by", attr(data[[time]], "label"))
    
    # initialize plot
    p <- d %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = time, y = value, group = "USUBJID")  +
      ggplot2::ylab(
        glue::glue("{var_label} ({attr(d[[value]], 'label')})")
      ) +
      ggplot2::xlab(attr(data[[time]], "label"))
  }
  
  # Add common layers to plot
  p <- p + 
    ggplot2::geom_line() +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.title = element_text(size = 16)) +
    ggplot2::ggtitle(var_title)
  
  return(p)
}
