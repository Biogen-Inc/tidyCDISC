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
  # yvar = "ALB"
  # xvar = "ALP"
  # week_x = "Baseline"
  # value_x = "AVAL"
  # week_y = "Week 2"
  # value_y = "AVAL"
  # separate = "AGEGR1"
  # color = "AENTMTFL"
  
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
    
    # Initialize plot
    p <- d %>%
      ggplot2::ggplot() + 
      ggplot2::aes_string(x = xvar, y = yvar) +
      ggplot2::xlab(attr(data[[xvar]], 'label')) + 
      ggplot2::ylab(attr(data[[yvar]], 'label'))
    
    # Initialize title of variables plotted
    var_title <- paste(attr(data[[yvar]], 'label'), "versus", attr(data[[xvar]], 'label'))
    
    
    
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
    
    # Initialize title of variables plotted
    var_title <- paste(attr(data[[yvar]], 'label'), "versus", unique(d$PARAM), "at", week_x)
    
    # initialize plot
    p <- d %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = value_x, y = yvar) +
      ggplot2::xlab(
        glue::glue("{unique(d$PARAM)}: {week_x} ({attr(data[[value_x]], 'label')})")
      ) +
      ggplot2::ylab(attr(data[[yvar]], 'label'))
    
    
    
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
    
    # Initialize title of variables plotted
    var_title <- paste(unique(d$PARAM), "at", week_y, "versus", attr(data[[xvar]], 'label'))
    
    # initialize plot
    p <- d %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = xvar, y = value_y) +
      ggplot2::xlab(attr(data[[xvar]], 'label')) + 
      ggplot2::ylab(
        glue::glue("{unique(d$PARAM)}: {week_y} ({attr(data[[value_y]], 'label')})")
      )
    
    
    
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
    
    # Initialize title of variables plotted
    var_title <- paste(unique(y_data$PARAM),"versus", unique(x_data$PARAM))
    
    # initialize plot
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
      p <- {if(nrow(by_u) == nrow(by_all) ) by_all else {
        
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
        dplyr::mutate(across(where(function(x) all(is.na(x))), ~ "NA" )) %>%
        ggplot2::ggplot() +
        ggplot2::aes_string(x = xvar, y = yvar) +
        ggplot2::xlab(
          glue::glue("{unique(x_data$PARAM)}: {week_x} ({attr(data[[value_x]], 'label')})")
        ) + 
        ggplot2::ylab(
          glue::glue("{unique(y_data$PARAM)}: {week_y} ({attr(data[[value_y]], 'label')})")
        ) 
    )
  }
  
  
  # if separate or color used, include those "by" variables in title
  by_title <- case_when(
    separate != "NONE" & color != "NONE" ~ paste("\nby", attr(data[[color]], "label"), "and", attr(data[[separate]], "label")),
    separate != "NONE" ~ paste("\nby", attr(data[[separate]], "label")),
    color != "NONE" ~ paste("\nby", attr(data[[color]], "label")), 
    TRUE ~ ""
  )
  
  # Add plot layers common to all graphs
  p <- p + 
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
  
  # Add in plot layers conditional upon user selection
  if (separate != "NONE") { 
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste(".~", separate)), labeller = ggplot2::label_both)}
  if (color != "NONE") { p <- p + ggplot2::aes_string(color = color)}
  if (by_title != "") {p <- p + ggplot2::theme(plot.margin = ggplot2::margin(t = 1.15, unit = "cm"))}
  
  return(p)
}
