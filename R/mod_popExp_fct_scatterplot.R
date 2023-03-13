#' Scatterplot
#' 
#' Create scatter plot where if the variables are numeric then they
#' are plotted, and if they are PARAMCD's then a week and value 
#' must be selected for plotting.
#' 
#' @param data Merged data to be used in plot
#' @param xvar Selected x-axis 
#' @param week_x Selected x axis week if \code{xvar} is a PARAMCD
#' @param value_x Selected x-axis value if \code{xvar} is a PARAMCD: 
#' either AVAL, CHG, or BASE
#' @param yvar Selected xy-axis 
#' @param week_y Selected y-axis week if \code{yvar} is a PARAMCD
#' @param value_y Selected y-axis value if \code{yvar} is a PARAMCD: 
#' either AVAL, CHG, or BASE
#' @param separate character, categorical or factor variable to facet plots by. Default is 'NONE'.
#' @param color character, categorical or factor variable to COLOR points by. Default is 'NONE'.
#' 
#' @importFrom stats as.formula
#' 
#' @family popExp functions
#' @keywords popEx
#' 
#' @return A ggplot object containing the scatterplot
#' 
#' @noRd
app_scatterplot <- function(data, yvar, xvar, week_x, value_x, week_y, value_y, separate = "NONE", color = "NONE") {
  
  # data = all_data
  # yvar = "DIABP"
  # xvar = "DIABP"
  # week_x = "SCREENING"
  # value_x = "AVAL"
  # week_y = "SCREENING"
  # value_y = "AVAL"
  # separate = "ACTARM"
  # color = "ACTARM"
  
  # separate = "NONE"
  # color = "ACTARM"
  
  # separate = "ACTARM"
  # color = "NONE"
  
  # separate = "NONE"
  # color = "NONE"
  
  # ---------------------------
  # x and y are numeric columns
  if (yvar %in% colnames(data) & xvar %in% colnames(data)) {
    
    # If not displaying param var, but a BDS data set is loaded, filter to the first visit
    suppressWarnings(
      d <- data %>%
        {if("AVISITN" %in% colnames(data)) filter(., AVISITN == min(AVISITN, na.rm = TRUE)) else .} %>%
        select(USUBJID, xvar, yvar, one_of(color, separate)) %>%
        distinct()%>%
        dplyr::mutate(across(where(function(x) all(is.na(x))), ~ "NA" )) # Convert NA cols to "NA"
    )
    
    # Initialize plot x & y vars
    x.var <- xvar
    x.lab <- attr(data[[xvar]], 'label')
    y.var <- yvar
    y.lab <- attr(data[[yvar]], 'label')
    
    # Initialize title of variables plotted
    var_title <- paste(y.lab, "versus", x.lab)
    
    
    
  # --------------------------- 
  # y numeric, x is paramcd 
  } else if (yvar %in% colnames(data) & !xvar %in% colnames(data)) {
    
    # Filter data by param selected
    suppressWarnings(
      d <- data %>% dplyr::filter(PARAMCD == xvar) %>%
        {if(xvar != "HEIGHT") filter(., AVISIT == week_x) else .} %>% # If yvar is HEIGHT, then don't filter by AVISIT
        # select the variables that matter, and get distinct rows to duplicate points aren't plotted
        dplyr::select(USUBJID, PARAM, PARAMCD, AVISIT, value_x, yvar, one_of(color, separate)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(across(where(function(x) all(is.na(x))), ~ "NA" )) # Convert NA cols to "NA"
    )
    
    # initialize plot x & y vars
    x.var <- value_x
    x.lab <- glue::glue("{unique(d$PARAM)}: {week_x} ({attr(data[[value_x]], 'label')})")
    y.var <- yvar
    y.lab <- attr(data[[yvar]], 'label')
    
    # Initialize title of variables plotted
    var_title <- paste(y.lab, "versus", unique(d$PARAM), "at", week_x)
    

    
  # --------------------------- 
  # x numeric, y paramcd
  } else if (!yvar %in% colnames(data) & xvar %in% colnames(data)) {
    
    # Filter data by param selected
    suppressWarnings(
    d <- data %>% dplyr::filter(PARAMCD == yvar) %>%
      {if(yvar != "HEIGHT") filter(., AVISIT == week_y) else .} %>% # If yvar is HEIGHT, then don't filter by AVISIT
      # select the variables that matter, and get distinct rows to duplicate points aren't plotted
      dplyr::select(USUBJID, PARAM, PARAMCD, AVISIT, value_y, xvar, one_of(color, separate)) %>%
      dplyr::distinct() %>%
      dplyr::mutate(across(where(function(x) all(is.na(x))), ~ "NA" )) # Convert NA cols to "NA"
    )
    
    # initialize plot x & y vars
    x.var <- xvar
    x.lab <- attr(data[[xvar]], 'label')
    y.var <- value_y
    y.lab <- glue::glue("{unique(d$PARAM)}: {week_y} ({attr(data[[value_y]], 'label')})")
    
    # Initialize title of variables plotted
    var_title <- paste(unique(d$PARAM), "at", week_y, "versus", x.lab)
    

    
  # ---------------------------
  # both x & y are paramcds
  } else {
    
    # Build plot data for y variable
    y_data <- data %>% dplyr::filter(PARAMCD == yvar)
      
    suppressWarnings(  
      y_dat <- y_data %>%
        {if(yvar != "HEIGHT") filter(., AVISIT == week_y) else .} %>% # If yvar is HEIGHT, then don't filter by AVISIT
        # Select the variables that matter and pivot aval into new column
        dplyr::select(USUBJID, AVISIT, PARAMCD, value_y, one_of(color, separate)) %>%
        tidyr::pivot_wider(names_from = PARAMCD, values_from = value_y) %>%
        tidyr::unnest(yvar) #%>% # if their are more than 1 AVAL per Patient, per Visit
        # dplyr::mutate(across(where(function(x) all(is.na(x))), ~ "NA" )) # Convert NA cols to "NA"
    )
    
    # Build plot data for x variable
    x_data <-  data %>% dplyr::filter(PARAMCD == xvar)
    
    suppressWarnings(
      x_dat <- x_data %>%
        {if(xvar != "HEIGHT") filter(., AVISIT == week_x) else . } %>%# If yvar is HEIGHT, then don't filter by AVISIT
        # Select the variables that matter and pivot aval into new column
        dplyr::select(USUBJID, AVISIT, PARAMCD, value_x, one_of(color, separate)) %>%
        tidyr::pivot_wider(names_from = PARAMCD, values_from = value_x) %>%
        tidyr::unnest(xvar) #%>% # if their are more than 1 AVAL per Patient, per Visit
        # dplyr::mutate(across(where(function(x) all(is.na(x))), ~ "NA" )) # Convert NA cols to "NA"
    )
    
    
    # create plot data
    suppressWarnings(
      by_u <- y_dat %>% rowwise() %>% #select(-AVISIT) %>%
        mutate(across(tidyr::one_of(color, separate), function(x) if(is.na(x)) "NA" else x)) %>%
        full_join(x_dat %>% rowwise() %>% #select(-AVISIT)
                    mutate(across(tidyr::one_of(color, separate), function(x) if(is.na(x)) "NA" else x))
                  , by = c("USUBJID") ) %>% 
        #, suffix = c(paste0(": ", unique(y_dat$AVISIT)), paste0(": ", unique(x_dat$AVISIT)))) %>% #
        arrange(USUBJID)  
    )
    suppressMessages(
      by_all <- y_dat %>% select(-AVISIT) %>%
        full_join(x_dat %>% select(-AVISIT)) %>% #
        arrange(USUBJID) 
    )
    suppressMessages(
      d <- {if(nrow(by_u) == nrow(by_all) ) by_all else {
        
        # needed for option 1 or 2
        suff <- function(x, suf) sym(paste0(x, ".", suf))
        if(paste(suff(xvar,"x")) %in% names(by_u)) xvar <- paste(suff(xvar,"x"))
        if(paste(suff(yvar,"y")) %in% names(by_u)) yvar <- paste(suff(yvar,"y"))
        
        # # option 1
        # suppressWarnings(
        #   y_dat %>%
        #     mutate(across(tidyr::one_of(color, separate), function(x) paste0(AVISIT, ": ", ifelse(is.na(x), "NA", x)))) %>%
        #     select(-AVISIT) %>%
        #     full_join( x_dat %>%
        #       mutate(across(tidyr::one_of(color, separate), function(x) paste0(AVISIT, ": ", ifelse(is.na(x), "NA", x)))) %>%
        #       select(-AVISIT)
        #     , by = c("USUBJID")
        #     )  %>%
        #     tidyr::drop_na() %>%
        #     {if(color %in% names(by_all)) tidyr::unite(., !!sym(color), c(suff(color, "x"), suff(color, "y")), sep = " & ") else .} %>%
        #     {if(separate %in% names(by_all)) tidyr::unite(., !!sym(separate), c(suff(separate, "x"), suff(separate, "y")), sep = " & ") else .}
        # )
        
        # option 2
        mk_str <- function(var.x, var.y, visit_var.x, visit_var.y) {
          if(var.x == var.y) {
            if(is.na(var.x)) "NA" else var.x
        } else {
            paste0(visit_var.x, ": ", if(is.na(var.x)) "NA" else var.x, " & ",
                   visit_var.y, ": ", if(is.na(var.y)) "NA" else var.y)
        }}
        suppressWarnings(
          by_u %>%
            tidyr::drop_na() %>%
            rowwise() %>% # new
            {if(color %in% names(by_all)) mutate(., !!sym(color) := mk_str(!!suff(color, "x"), !!suff(color, "y"), !!suff("AVISIT", "x"), !!suff("AVISIT", "y"))) else .} %>%
            {if(separate %in% names(by_all)) mutate(., !!sym(separate) := mk_str(!!suff(separate, "x"), !!suff(separate, "y"), !!suff("AVISIT", "x"), !!suff("AVISIT", "y"))) else .} %>%
            select(USUBJID, tidyr::one_of(color, separate), xvar, yvar)
        )
        
      }} %>%
        dplyr::mutate(across(where(function(x) all(is.na(x))), ~ "NA" ))
    )
    
    # Initialize plot x & y vars
    x.var <- xvar
    x.lab <- glue::glue("{unique(x_data$PARAM)}: {week_x} ({attr(data[[value_x]], 'label')})")
    y.var <- yvar
    y.lab <- glue::glue("{unique(y_data$PARAM)}: {week_y} ({attr(data[[value_y]], 'label')})")
    
    # Initialize title of variables plotted
    var_title <- paste(unique(y_data$PARAM),"versus", unique(x_data$PARAM))
  }
  
  

  # --------------
  # Plot time
  # --------------

  # if separate or color used, include those "by" variables in title
  by_title <- case_when(
    separate == color & color != "NONE" ~  paste("\nby", attr(data[[color]], "label")),
    separate != "NONE" & color != "NONE" ~ paste("\nby", attr(data[[color]], "label"), "and", attr(data[[separate]], "label")),
    separate != "NONE" ~ paste("\nby", attr(data[[separate]], "label")),
    color != "NONE" ~ paste("\nby", attr(data[[color]], "label")), 
    TRUE ~ ""
  )

  # Add plot layers
  p <- d %>%
    # wrap text on color variable. Changing the name of color var in the process
    {if(color != "NONE") mutate(., !!sym(paste0("By ", color)) := factor(stringr::str_wrap(!!sym(color), 30),
               levels = stringr::str_wrap(get_levels(pull(d,color)), 30))) else .} %>%
    ggplot2::ggplot() +
    ggplot2::aes_string(x = x.var, y = y.var) + # here
    ggplot2::xlab(x.lab) + 
    ggplot2::ylab(y.lab) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 16)
    ) +
    ggplot2::ggtitle(paste(var_title, by_title)
                     # ,subtitle = paste(by_title) # plotly won't automatically accept this
    )
  # plotly::ggplotly(p) %>% plotly::layout(title = list(yref = "container", y = .95, yanchor = "bottom"))
  
  if (color != "NONE") { p <- p + ggplot2::aes_string(colour = paste0("`By ", color, "`")) + 
        ggplot2::labs(colour = paste0("By ", color)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, vjust = 4)
                       ,plot.margin = ggplot2::margin(t = .35, unit = "cm")
        )
  # p
  # plotly::ggplotly(p) %>% plotly::layout(title =
  #      list(yref = "container", y = .95, yanchor = "bottom")) #pad = list(b = 200)
  }
  
  if (separate != "NONE") {
    lbl <- paste0(separate, ": ", get_levels(pull(d, separate)) ) %>% stringr::str_wrap(50)
    max_lines <- max(stringr::str_count(lbl, "\n")) + 1
    p <- p +
      ggplot2::facet_wrap(stats::as.formula(paste0(".~ ", separate)), 
        labeller = ggplot2::as_labeller(setNames(lbl , get_levels(pull(d, separate))))
      ) + # strip height is not adjusting automatically with text wrap in the app (though it does locally)
      ggplot2::theme(
        strip.text = ggplot2::element_text(
          margin = ggplot2::margin(t = (5 * max_lines), b = (6 * max_lines))),
        plot.title = ggplot2::element_text(size = 16, vjust = 10)
        ,plot.margin = ggplot2::margin(t = .6, unit = "cm")
      ) 
    if(max_lines > 1) p <- p + ggplot2::theme(panel.spacing.y = 
                   ggplot2::unit((.25 * max_lines),"lines"))
    # p
    # plotly::ggplotly(p) %>% plotly::layout(title =
    #               list(yref = "container", y = .95, yanchor = "bottom")) #pad = list(b = 200)
  }
  return(p)
}
