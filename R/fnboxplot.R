# boxplot function -- rkrajcik 
# parameters:
#    data = data frame, groupbox = {T|F}, groupvar = var to group on, respvar = y-axis variable
#    returns: p for ggplotly
fnboxplot <- function(data, groupbox, groupvar, respvar) {
  
  if(groupbox == TRUE) {
    req(groupvar != " ")
    
    # print("in fnboxplot")
    # print(paste("N of rows",nrow(data)))
    # remove missing groups from plot
    data <- filter(data, !is.na(!!sym(groupvar))) 
    # print(paste("N of rows",nrow(data)))

    # correction for overplotting
    # data <- fnoverplt(data,groupvar)
    
    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!sym(groupvar), y = !!sym(respvar), fill = !!sym(groupvar))) +
      labs(x = groupvar, y = respvar)
    
  } else {
    p <- ggplot(data,na.rm=TRUE,
                aes(x = " ", y = !!sym(respvar) )) +
      labs(x = "All", y = respvar)
  }
  
  p <- p +
    geom_boxplot(outlier.shape = NA, na.rm = TRUE, alpha = 0.2, notch = TRUE) 
  
  return(p)
}
