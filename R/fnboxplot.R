# boxplot function -- rkrajcik 
# parameters:
#    data = data frame, splitby = {T|F}, splitvar = var to group on, respvar = y-axis variable
#    returns: p for ggplotly
fnboxplot <- function(data, splitby, splitvar, respvar) {
  
  y_var <- as.name(respvar)
  
  if(splitby == TRUE) {
    req(splitvar != " ")
    
    x_var <- as.name(splitvar)
    
    # correction for overplotting
    data <- fnoverplt(data,splitvar)
    
    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!x_var, y = !!y_var, fill = !!x_var)) +
      labs(x = splitvar, y = respvar)
    
  } else {
    p <- ggplot(data,na.rm=TRUE,
                aes(x = " ", y = !!y_var )) +
      labs(x = "All", y = respvar)
  }
  
  p <- p +
    geom_boxplot(outlier.shape = NA, na.rm = TRUE, alpha = 0.1) +
    scale_fill_discrete() +
    theme_classic()
  
  return(p)
}
