# boxplot function -- rkrajcik 
# parameters:
#    data = data frame, groupbox = {T|F}, groupbyvar = var to group on, responsevar = y-axis variable
#    returns: p for ggplotly
fnboxplot <- function(data, groupbox, groupbyvar, responsevar) {
  
  if(groupbox == TRUE) {
    req(!is_empty(groupbyvar) && groupbyvar != "")

    # correction for overplotting
    data <- fnoverplt(data,responsevar,groupbyvar)

    print(paste("N of rows",nrow(data)))
    # remove missing groups from plot
    data <- filter(data, !is.na(!!sym(groupbyvar))) 
    print(paste("N of rows",nrow(data)))
    
    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!sym(groupbyvar), y = !!sym(responsevar), fill = !!sym(groupbyvar))) +
      labs(x = groupbyvar, y = responsevar)
    
  } else {
    p <- ggplot(data,na.rm=TRUE,
                aes(x = " ", y = !!sym(responsevar) )) +
      labs(x = "All", y = responsevar)
  }
  
  p <- p +
    geom_boxplot(outlier.shape = NA, na.rm = TRUE, alpha = 0.2, notch = TRUE) 
  
  return(p)
}
