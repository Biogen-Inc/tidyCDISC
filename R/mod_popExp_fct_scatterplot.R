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
  # ---------------------------
  # x and y are numeric columns
  if (yvar %in% colnames(data) & xvar %in% colnames(data)) {
    
    # If not displaying param var, but a BDS data set is loaded, filter to the first visit
    if("AVISITN" %in% colnames(data)){
      suppressWarnings(
        d <- data %>%
          filter(AVISITN == min(AVISITN, na.rm = TRUE)) %>%
          select(USUBJID, xvar, yvar, one_of(color, separate)) %>%
          distinct()
      )
    } else { # otherwise, no need to filter
      suppressWarnings(
        d <- data %>%
          select(USUBJID, xvar, yvar, one_of(color, separate)) %>%
          distinct()
      )
    }
    
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
    d <- data %>% dplyr::filter(PARAMCD == xvar) 
    
    # If yvar is HEIGHT, then don't filter by AVISIT
    if(xvar != "HEIGHT"){ d <- d %>% filter(AVISIT == week_x) }
    
    # select the variables that matter, and get distinct rows to duplicate points aren't plotted
    suppressWarnings(
      d <- d %>%
        dplyr::select(USUBJID, PARAM, PARAMCD, AVISIT, value_x, yvar, one_of(color, separate)) %>%
        dplyr::distinct()
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
    d <- data %>% dplyr::filter(PARAMCD == yvar)
    
    # If yvar is HEIGHT, then don't filter by AVISIT
    if(yvar != "HEIGHT"){ d <- d %>% filter(AVISIT == week_y) }
    
    # select the variables that matter, and get distinct rows to duplicate points aren't plotted
    suppressWarnings(
      d <- d %>%
        dplyr::select(USUBJID, PARAM, PARAMCD, AVISIT, value_y, xvar, one_of(color, separate)) %>%
        dplyr::distinct()
    )
    
    # initialize plot x & y vars
    x.var <- xvar
    x.lab <- attr(data[[xvar]], 'label')
    y.var <- value_y
    y.lab <- glue::glue("{unique(d$PARAM)}: {week_y} ({attr(data[[value_y]], 'label')})")
    
    # Initialize title of variables plotted
    var_title <- paste(unique(d$PARAM), "at", week_y, "versus", x.lab)
    
  
    
  # ---------------------------
  # both paramcds
  } else {
    
    # Build plot data for y variable
    y_data <- data %>% dplyr::filter(PARAMCD == yvar)
    
    # If yvar is HEIGHT, then don't filter by AVISIT
    if(yvar != "HEIGHT"){ y_data <- y_data %>% filter(AVISIT == week_y) }
    
    # Select the variables that matter and pivot aval into new column
    suppressWarnings(  
      y_dat <- y_data %>%
        dplyr::select(USUBJID, PARAMCD, value_y, one_of(color, separate)) %>%
        tidyr::pivot_wider(names_from = PARAMCD, values_from = value_y) %>%
        tidyr::unnest(yvar) %>% # if their are more than 1 AVAL per Patient, per Visit
        select_if(~!all(is.na(.))) # remove NA cols
    )
    
    # Build plot data for x variable
    x_data <-  data %>% dplyr::filter(PARAMCD == xvar)
    
    # If yvar is HEIGHT, then don't filter by AVISIT
    if(xvar != "HEIGHT"){ x_data <- x_data %>% filter(AVISIT == week_x) }
    
    # Select the variables that matter and pivot aval into new column
    suppressWarnings(
      x_dat <- x_data %>%
        dplyr::select(USUBJID, PARAMCD, value_x, one_of(color, separate)) %>%
        tidyr::pivot_wider(names_from = PARAMCD, values_from = value_x) %>%
        tidyr::unnest(xvar) %>% # if their are more than 1 AVAL per Patient, per Visit
        select_if(~!all(is.na(.))) # remove NA cols
    )
    
    # Initialize plot x & y vars
    x.var <- xvar
    x.lab <- glue::glue("{unique(x_data$PARAM)}: {week_x} ({attr(data[[value_x]], 'label')})")
    y.var <- yvar
    y.lab <- glue::glue("{unique(y_data$PARAM)}: {week_y} ({attr(data[[value_y]], 'label')})")
    
    # Initialize title of variables plotted
    var_title <- paste(unique(y_data$PARAM),"versus", unique(x_data$PARAM))
    
    suppressMessages(
      d <- y_dat %>%
        inner_join(x_dat)
    )
    # test <- d %>%
    #   {if(color != "NONE") mutate(., !!sym(color) := factor(stringr::str_wrap(!!sym(color), 30),
    #                             levels = stringr::str_wrap(get_levels(pull(d,color)), 30))) else .}
    # levels(test$AGEGR1)
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
    # wrap text on color or separate variables as needed. Don't change the name
    # of color var, but we do for sep just in case xvar = yvar.
    
    # {if(separate != "NONE") mutate(., !!sym(paste0(separate,"_sep")) :=
    #     factor(stringr::str_wrap(!!sym(separate), 50),
    #            levels = stringr::str_wrap(get_levels(pull(d,separate)), 50))) else .} %>%
    
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
  
  # Add in plot layers conditional upon user selection
  if (color != "NONE") { p <- p + ggplot2::aes_string(colour = paste0("`By ", color, "`")) + 
        ggplot2::labs(colour = paste0("By ", color)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, vjust = 4))
  }
  if (separate != "NONE") {
    lbl <- paste0(separate, ": ", get_levels(pull(d, separate)) ) %>% stringr::str_wrap(50)
    max_lines <- max(stringr::str_count(lbl, "\n")) + 1
    p <- p +
      ggplot2::facet_wrap(stats::as.formula(paste0(".~ ", separate)), 
        labeller = ggplot2::as_labeller(setNames(lbl , get_levels(pull(d, separate))))
      ) + # strip height is not adjusting automatically with text wrap in the app (though it does locally)
      ggplot2::theme(strip.text = ggplot2::element_text(
        margin = ggplot2::margin(t = (5 * max_lines), b = (6 * max_lines))),
        plot.title = ggplot2::element_text(size = 16, vjust = 10)
      ) 
  }
  if (by_title != "") {p <- p + ggplot2::theme(plot.margin = ggplot2::margin(t = 1, unit = "cm"))}
  
  return(p)
}

# cases <- c("case1 has long name", "case2 long too", "case3 long as well", "case4 also long", "case5 long")
# var1 <- cases[round(runif(100,1,3))]
# var2 <- cases[round(runif(100,1,5))]
# # var1 <- "the first variable"
# # var2 <- "variable number two"
# facetFormula <- as.formula("var1 ~ var2")
# myX <- runif(100,0,10)
# myY <- runif(100,-5,5)
# myData <- data.frame(myX, myY, var1, var2)
# ggplot(myData, aes(x = myX, y = myY)) +
#   geom_point(alpha = .5) +
#   facet_grid(facetFormula,
#              labeller = label_both)
# x <- distinct(myData[3:4])
# names(x)[[1]]
# my_label <- function(x) {
#   x[1] <- stringr::str_wrap(gsub("_", " ", x[1]), 50)
#   label_both(x, sep = ": ")
# }
# 
# ggplot(myData, aes(x = myX, y = myY)) +
#   geom_point(alpha = .5) +
#   facet_grid(facetFormula, labeller = my_label)
