# boxplot function -- rkrajcik 
# parameters:
#    data = data frame, groupbox = {T|F}, groupbyvar = var to group on, responsevar = y-axis variable
#    returns: p for ggplotly
fnboxplot <- function(data, groupbox, groupbyvar, responsevar, addpoints) {
  
  if(groupbox == TRUE) {
    req(!is_empty(groupbyvar) && groupbyvar != "")

    # correction for overplotting
    data <- fnoverplt(data,responsevar,groupbyvar)

    # remove missing groups from plot
    data <- filter(data, !is.na(!!sym(groupbyvar))) 

    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!sym(groupbyvar), y = !!sym(responsevar), fill = !!sym(groupbyvar)))
    
    if (addpoints == TRUE){
      p <- p +
        # Remove outliers when overlaying boxplot with original data points
        geom_boxplot(outlier.shape = NA, na.rm = TRUE) + 
        suppressWarnings(geom_point(position = 'jitter', alpha = 0.2,  na.rm = TRUE,
                                    aes(text = 
                                          paste0(USUBJID,
                                                 "<br>",groupbyvar, ": ",get(groupbyvar),
                                                 "<br>",responsevar,": ",get(responsevar)
                                          )
                                    ))) # aes, geom_point, suppressWarnings 
    } else {
      p <- p +
        geom_boxplot(outlier.colour = "darkblue", outlier.shape = 0, na.rm = TRUE, alpha = 0.2) 
    }
    
  } else {
    p <- ggplot(data,na.rm=TRUE,
                aes(x = " ", y = !!sym(responsevar) ))
    
    if (addpoints == TRUE) {
      p <- p +
        # Remove outliers when overlaying boxplot with original data points
        geom_boxplot(outlier.shape = NA, na.rm = TRUE) + 
        suppressWarnings(geom_point(position = 'jitter', alpha = 0.2,  na.rm = TRUE,
                                    aes(text = 
                                          paste0(USUBJID,
                                                 "<br>",groupbyvar, ": ",get(groupbyvar),
                                                 "<br>",responsevar,": ",get(responsevar)
                                          )
                                    ))) # aes, geom_point, suppressWarnings 
    } else {
      p <- p +
        geom_boxplot(outlier.colour = "darkblue", outlier.shape = 0, na.rm = TRUE, alpha = 0.2) 
    }
  }
  
  # Add light theme
  p <- p + theme_light()
  
  # remove the legend
  p <-  p + theme(legend.position = "none")  
  
  return(p)
}
