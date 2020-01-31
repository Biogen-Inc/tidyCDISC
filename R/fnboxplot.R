# boxplot function -- rkrajcik 
# parameters:
#    data = data frame, splitby = {T|F}, splitvar = var to group on, respvar = y-axis variable
#    returns: p for ggplotly
fnboxplot <- function(data, splitby, splitvar, respvar) {
  
  x_var <- as.name(splitvar)
  y_var <- as.name(respvar)
  
  if(splitby == TRUE) {
    req(splitvar != " ")
    
    ggtitle <- paste("Distribution of",respvar,"Grouped by",splitvar)
    p <- ggplot(data,
                aes(x = !!x_var, y = !!y_var, fill = !!x_var)) +
      labs(x = splitvar, y = respvar)
    
  } else {
    ggtitle <- paste("Distribution of",respvar)
    p <- ggplot(data,
                aes(x = " ", y = !!y_var )) +
      labs(x = "All", y = respvar)
  }
  
  p <- p +
    geom_boxplot(outlier.shape = NA, na.rm = TRUE, alpha = 0.1) +
    ggtitle(ggtitle) +
    scale_fill_discrete() +
    theme_classic()
  
  return(p)
}
