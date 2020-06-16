#' Generate basic scatterplot 
#'
#' @param data a data frame
#' @param groupbox {T|F} Should groupbyvar be used?
#' @param groupbyvar var to group on
#' @param selxvar x-axis variable
#' @param selyvar y-axis variable
#'
#' @return p ggplot2 object
#' 
#'   DO NOT REMOVE.
#' @import dplyr
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom rlang sym
#' @importFrom sjlabelled get_label
#' @noRd
#' 
fnscatter <- function(data, groupbox, groupbyvar, selxvar, selyvar) {
  
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
  
  # light theme
  p <- p + theme_light()
  
  # https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
  nlevs <- nlevels(factor(data[[groupbyvar]]))
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nlevs)
  p <- p + scale_fill_manual(values = mycolors) 
  
  # add x- and y- axis labels
  # set def.value to use name if the variable has no label attribute
  labx <- sjlabelled::get_label(data[[selxvar]], def.value = unique(selxvar))
  laby <- sjlabelled::get_label(data[[selyvar]], def.value = unique(selyvar))
  
  p <- p + labs(x = labx, y = laby)
  
  return(p)
  
}
