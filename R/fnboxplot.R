# boxplot function -- rkrajcik 
# parameters:
#    data = data frame, splitby = {T|F}, splitvar = var to group on, respvar = y-axis variable
#    returns: p for ggplotly
fnboxplot <- function(data, splitby, splitvar, respvar) {
  
  y_var <- as.name(respvar)
  
  if(splitby == TRUE) {
    req(splitvar != " ")
    
    x_var <- as.name(splitvar)
    
    # restrict seltimevar to AVISIT, AVISITN, VSDY
    seltime <- select(data, ends_with("DY"), starts_with("AVIS")) 
    
    # if not using AVISIT(n) then collapse to USUBJID level and set AVISIT to Baseline
    print(paste("befor",nrow(data)))
    if (!splitvar %in% names(seltime) & "AVISIT" %in% names(data) & "USUBJID" %in% names(data)) {
      data.1 <- data %>%
        filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
        distinct(USUBJID, .keep_all = TRUE)
    } else {
      data.1 <- data
    }
    print(paste("after",nrow(data.1)))
 
    ggtitle <- paste("Distribution of",respvar,"Grouped by",splitvar)
    p <- ggplot(data,na.rm = TRUE,
                aes(x = !!x_var, y = !!y_var, fill = !!x_var)) +
      labs(x = splitvar, y = respvar)
    
  } else {
    ggtitle <- paste("Distribution of",respvar)
    p <- ggplot(data.1,na.rm=TRUE,
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
