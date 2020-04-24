# scatterplot function -- rkrajcik
# parameters:
#    data = data frame, groupbox = {T|F}, groupbyvar = var to group on, selxvar = x-axis, selyvar = y-axis
#    returns: p for ggplotly
fnscatter <- function(data, groupbox, groupbyvar, selxvar, selyvar) {
  
  # correction for overplotting
  data <- fnoverplt(data,selxvar,groupbyvar)

  if(groupbox == TRUE) {
    req(!is_empty(groupbyvar) && groupbyvar != "")
    
    # remove missing groups from plot
    data <- filter(data, !is.na(!!sym(groupbyvar))) 
    # print(paste("N of rows",nrow(data)))

    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!sym(selxvar), y = !!sym(selyvar), color = !!sym(groupbyvar))) 
    
    if ("USUBJID" %in% colnames(data)) {
    p <- p + 
        suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
        aes(text = paste0(USUBJID,
                  "<br>",groupbyvar,": ",get(groupbyvar),
                  "<br>",selxvar,": ",get(selxvar),
                  "<br>",selyvar,": ",get(selyvar)))))
    } else {
      p <- p + 
         suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
         aes(text = paste0(groupbyvar, ": ",get(groupbyvar),
                   "<br>",selxvar,": ",get(selxvar),
                   "<br>",selyvar,": ",get(selyvar)))))
    }
  
    
  } else {
    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!sym(selxvar), y = !!sym(selyvar) )) 
    
    if ("USUBJID" %in% colnames(data)) {
      p <- p +
        suppressWarnings(geom_point(position = 'identity', alpha = 0.2, na.rm = TRUE,
        aes(text =  paste0(USUBJID,
                   "<br>",selxvar,": ",get(selxvar),
                   "<br>",selyvar,": ",get(selyvar)))))
    } else {
      p <- p +
        suppressWarnings(geom_point(position = 'identity', alpha = 0.2, na.rm = TRUE,
        aes(text = paste0(selxvar,": ",get(selxvar),
                   "<br>",selyvar,": ",get(selyvar)))))
    }

  }
  
  return(p)
  
}
