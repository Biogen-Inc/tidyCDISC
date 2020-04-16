# scatterplot function -- rkrajcik
# parameters:
#    data = data frame, groupbox = {T|F}, groupvar = var to group on, selxvar = x-axis, selyvar = y-axis
#    returns: p for ggplotly
fnscatter <- function(data, groupbox, groupvar, selxvar, selyvar) {
  
  # correction for overplotting
  # message(paste("fnoverplot nrows before",nrow(data)))
  # data <- fnoverplt(data,groupvar)
  # message(paste("fnoverplot nrows after ",nrow(data)))

  if(groupbox == TRUE) {
    req(groupvar != " ")
    
    # remove missing groups from plot
    # print("in scatterplot")
    # print(paste("N of rows",nrow(data)))
    # remove missing groups from plot
    data <- filter(data, !is.na(!!sym(groupvar))) 
    # print(paste("N of rows",nrow(data)))

    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!sym(selxvar), y = !!sym(selyvar), color = !!sym(groupvar))) 
    
    if ("USUBJID" %in% colnames(data)) {
    p <- p + 
        suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
        aes(text = paste0(USUBJID,
                  "<br>",groupvar,": ",get(groupvar),
                  "<br>",selxvar,": ",get(selxvar),
                  "<br>",selyvar,": ",get(selyvar)))))
    } else {
      p <- p + 
         suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
         aes(text = paste0(groupvar, ": ",get(groupvar),
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
