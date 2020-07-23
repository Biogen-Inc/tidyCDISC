#' IDEA scatterplot
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
#' @param separate whether to facet plots by categorical or factor
#' @param color whether to color plots by categorical or factor
#' 
#' @importFrom stats as.formula
#' 
#' @family popExp functions
#' 
IDEA_scatterplot <- function(data, yvar, xvar, week_x, value_x, week_y, value_y, separate = "NONE", color = "NONE") {
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
    
    # Initialize plot
    p <- d %>%
      ggplot2::ggplot() + 
      ggplot2::aes_string(x = xvar, y = yvar) +
      ggplot2::xlab(attr(data[[xvar]], 'label')) + 
      ggplot2::ylab(attr(data[[yvar]], 'label')) +
      ggplot2::geom_point(na.rm = TRUE)
    
    # Initialize title of variables plotted
    var_title <- paste(attr(data[[yvar]], 'label'), "versus", attr(data[[xvar]], 'label'))
    
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
    
    # Initialize title of variables plotted
    var_title <- paste(attr(data[[yvar]], 'label'), "versus", unique(d$PARAM), "at", week_x)
    
    # initialize plot
    p <- d %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = value_x, y = yvar) +
      ggplot2::xlab(
        glue::glue("{unique(d$PARAM)}: {week_x} ({attr(data[[value_x]], 'label')})")
      ) +
      ggplot2::ylab(attr(data[[yvar]], 'label')) +
      ggplot2::geom_point(na.rm = TRUE)
    
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
    
    # Initialize title of variables plotted
    var_title <- paste(unique(d$PARAM), "at", week_y, "versus", attr(data[[xvar]], 'label'))
    
    # initialize plot
    p <- d %>%
      ggplot2::ggplot() +
      ggplot2::aes_string(x = xvar, y = value_y) +
      ggplot2::xlab(attr(data[[xvar]], 'label')) + 
      ggplot2::ylab(
        glue::glue("{unique(d$PARAM)}: {week_y} ({attr(data[[value_y]], 'label')})")
      ) +
      ggplot2::geom_point(na.rm = TRUE)
    
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
    
    # Initialize title of variables plotted
    var_title <- paste(unique(y_data$PARAM),"versus", unique(x_data$PARAM))
    
    # initialize plot
    suppressMessages(
      p <-
        y_dat %>%
        inner_join(x_dat) %>%
        ggplot2::ggplot() +
        ggplot2::aes_string(x = xvar, y = yvar) +
        ggplot2::xlab(
          glue::glue("{unique(x_data$PARAM)}: {week_x} ({attr(data[[value_x]], 'label')})")
        ) + 
        ggplot2::ylab(
          glue::glue("{unique(y_data$PARAM)}: {week_y} ({attr(data[[value_y]], 'label')})")
        ) +
        ggplot2::geom_point(na.rm = TRUE)
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
    ggplot2::theme_bw() +
    theme(
      text = element_text(size = 12),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16)
    ) +
    ggplot2::ggtitle(paste(var_title, by_title)
                     # ,subtitle = paste(by_title) # plotly won't automatically accept this
    )
  
  # Add in plot layers conditional upon user selection
  if (separate != "NONE") { p <- p + ggplot2::facet_wrap(stats::as.formula(paste(".~", separate))) }
  if (color != "NONE") { p <- p + ggplot2::aes_string(color = color)}
  if (by_title != "") {p <- p + theme(plot.margin = margin(t = 1.2, unit = "cm"))}
  
  return(p)
}