# scatterplot function -- rkrajcik
# parameters:
#    data = data frame, splitby = {T|F}, splitbyvar = var to group on, selxvar = x-axis, selyvar = y-axis
#    returns: p for ggplotly
fnscatter <- function(data, splitby, splitbyvar, selxvar, selyvar) {
  
  x_var <- as.name(selxvar)
  y_var <- as.name(selyvar)
  
  # correction for overplotting
  data <- fnoverplt(data,selxvar)
  
  if(splitby == TRUE) {
    req(splitbyvar != " ")
    
    
    z_var <- as.name(splitbyvar)
    
    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!x_var, y = !!y_var, fill = !!z_var, shape = !!z_var, color = !!z_var)) +
      labs(x = selxvar, y = selyvar)
    
    if (substr(splitbyvar,1,3) %in% c("ARM","TRT")) {
      # add shapes for ARM/TRT group
      p <- p + scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9, 10, 11, 12)) 
    }
    
    if ("USUBJID" %in% colnames(data)) {
    p <- p + 
        suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
        aes(text = paste0(USUBJID,
                  "<br>",splitbyvar,": ",get(splitbyvar),
                  "<br>",selxvar,": ",get(selxvar),
                  "<br>",selyvar,": ",get(selyvar)))))
    } else {
      p <- p + 
         suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
         aes(text = paste0(splitbyvar, ": ",get(splitbyvar),
                   "<br>",selxvar,": ",get(selxvar),
                   "<br>",selyvar,": ",get(selyvar)))))
    }
  
    
  } else {
    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!x_var, y = !!y_var )) +
      labs(x = selxvar, y = selyvar)
    
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
