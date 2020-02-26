# scatterplot function -- rkrajcik
# parameters:
#    data = data frame, splitby = {T|F}, splitvar = var to group on, selxvar = x-axis, selyvar = y-axis
#    returns: p for ggplotly
fnscatter <- function(data, splitby, splitvar, selxvar, selyvar) {
  
  x_var <- as.name(selxvar)
  y_var <- as.name(selyvar)
  
  if(splitby == TRUE) {
    req(splitvar != " ")
    
    z_var <- as.name(splitvar)
    
    ggtitle <- paste("Plot of",selyvar,"by",selxvar,"Grouped by",splitvar)
    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!x_var, y = !!y_var, fill = !!z_var, shape = !!z_var, color = !!z_var)) +
      labs(x = selxvar, y = selyvar, title = ggtitle)
    
    if (substr(splitvar,1,3) %in% c("ARM","TRT")) {
      # add shapes for ARM/TRT group
      p <- p + scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9, 10, 11, 12)) 
    }
    
    if ("USUBJID" %in% colnames(data)) {
    p <- p + 
        suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
        aes(text = paste0(USUBJID,
                  "<br>",splitvar,": ",get(splitvar),
                  "<br>",selxvar,": ",get(selxvar),
                  "<br>",selyvar,": ",get(selyvar)))))
    } else {
      p <- p + 
         suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
         aes(text = paste0(splitvar, ": ",get(splitvar),
                   "<br>",selxvar,": ",get(selxvar),
                   "<br>",selyvar,": ",get(selyvar)))))
    }
  
    
  } else {
    ggtitle <- paste("Plot of",selyvar,"by",selxvar)
    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!x_var, y = !!y_var )) +
      labs(x = selxvar, y = selyvar, title = ggtitle)
    
    if ("USUBJID" %in% colnames(data)) {
      p <- p +
        suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
        aes(text =  paste0(USUBJID,
                   "<br>",selxvar,": ",get(selxvar),
                   "<br>",selyvar,": ",get(selyvar)))))
    } else {
      p <- p +
        suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
        aes(text = paste0(selxvar,": ",get(selxvar),
                   "<br>",selyvar,": ",get(selyvar)))))
    }

  }
  
  # p <- p +
  #   scale_fill_discrete() +
  #   theme_classic()
  
  return(p)
  
}
