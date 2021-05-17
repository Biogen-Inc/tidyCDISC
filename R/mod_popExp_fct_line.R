#' IDEA line plot
#' 
#' Create a line plot with a time variable as the x-axis
#' and using either the selected response variable
#' or if a PARAMCD is selected, then plot the corresponding value
#' to calculate the means. Lines are plotted by patient
#' 
#' @param data Merged data to be used in plot
#' @param yvar Selected y-axis 
#' @param time Selected x-axis constained to time dependent columns
#' @param value If yvar is a PARAMCD then the user must select 
#' AVAL, CHG, or BASE to be plotted on the y-axis
#' 
#' @family popExp Functions
IDEA_lineplot <- function(data, yvar, time, value = NULL, separate = "NONE", color = "NONE",
   err_bars = FALSE, label_points = FALSE, gtxt_x_pos = "middle", gtxt_y_pos = "top") {
  
  # library(dplyr)
  data0 <- data #%>% varN_fctr_reorder()
    
  # print(unique(data0[,c("AVISIT", "AVISITN")]))
  # print(".")
  # print(levels(data0$AVISIT))
  # print(".")
  # print(unique(data0$AVISIT))
  # print(".")
  # subset data based on yvar being paramcd or not
  if (yvar %in% colnames(data)) {
    suppressWarnings(
      d0 <- data0 %>% select(USUBJID, time, val = yvar, one_of(color, separate))
    )
    yvar_label <- yl <- ifelse(rlang::is_empty(attr(data[[yvar]], "label")), yvar, attr(data[[yvar]], "label"))
  } else {
    d0 <- data0 %>%
      dplyr::filter(PARAMCD == yvar) %>%
      select(USUBJID, time, PARAM, PARAMCD, val = value, one_of(color, separate))
    yvar_label <- ifelse(rlang::is_empty(paste(unique(d0$PARAM))), yvar, paste(unique(d0$PARAM)))
    yl <- glue::glue("{yvar_label} ({attr(data[[value]], 'label')})")
  }
  xl <- ifelse(rlang::is_empty(attr(d0[[time]], "label")), time, attr(d0[[time]], "label"))
  # print("yvar_label:")
  # print(yvar_label)
  # print("yl:")
  # print(yl)
  
  # mtcars %>% select("mpg" = "cyl")
    
  # by <- sym("cyl")
  # mtcars %>%
  #   # group_by(!!by, gear) %>%
  #   group_by_at(vars(one_of("none"), gear)) %>%
  #   summarize(mean_mpg = mean(mpg))
  
  val_sym <- rlang::sym("val")
  
  # Group data as needed to calc means
  d <-
    d0 %>%
    group_by_at(vars(time, one_of(color, separate))) %>%
    summarize(MEAN = round(mean(!!val_sym, na.rm = T), 2),
              # SEM = round(std_err(!!val_sym, na.rm = T),2), # NOT accurate?
              N = n_distinct(USUBJID, na.rm = T),
              n = n(),
              STD = round(sd(!!val_sym, na.rm = T), 2),
              SEM = round(STD/ sqrt(n), 2),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(Lower = MEAN - SEM, Upper = MEAN + SEM) %>%
    select( -STD , -n)
  # print(d)
  
  
  # if separate or color used, include those "by" variables in title
  var_title <- paste(ifelse(value == "CHG", "Mean Change from Baseline", "Mean"), yvar_label, "by", xl)
  by_title <- case_when(
    separate != "NONE" & color != "NONE" ~ paste("\nby", attr(data[[color]], "label"), "and", attr(data[[separate]], "label")),
    separate != "NONE" ~ paste("\nby", attr(data[[separate]], "label")),
    color != "NONE" ~ paste("\nby", attr(data[[color]], "label")), 
    TRUE ~ ""
  )
  
  dodge <- ggplot2::position_dodge(.9)
  
  # Add common layers to plot
  p <- d %>%
    ggplot2::ggplot() +
    ggplot2::aes_string(y = "MEAN", x = time, group = 1)  +
    ggplot2::geom_line(position = ggplot2::position_dodge(.91)) +
    ggplot2::geom_point(position = dodge, na.rm = TRUE) +
    ggplot2::labs(x = xl, y = yl, title = paste(var_title, by_title)) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.title = element_text(size = 16)
                   )
  
  # Add in plot layers conditional upon user selection
  if (color != "NONE") { p <- p + ggplot2::aes_string(color = color, group = color) }
  if (err_bars) {
    p <- p + ggplot2::aes(ymin = Lower, ymax = Upper) +
    ggplot2::geom_errorbar(position = dodge, width = 1.5)
  }
  if (separate != "NONE") { p <- p + ggplot2::facet_wrap(stats::as.formula(paste(".~", separate))) }
  if (by_title != "") {p <- p + theme(plot.margin = margin(t = 1.2, unit = "cm"))}
  
  if(label_points){
    x_scale <- layer_scales(p)$x$range$range
    if(all(!is.numeric(x_scale))){
      x_nums <- sort(as.numeric(as.factor(x_scale)))
      range <- diff(c(min(x_nums), max(x_nums)))
    } else {
      range <- diff(x_scale)
    }
    x_nudge_val <- range * .04 #* (plot_col_num /2)
    y_nudge_val <- diff(layer_scales(p)$y$range$range)*.04
    # gtxt_x_pos <- "right" #c("left", "middle", "right")
    # gtxt_y_pos <- "top"   #c("bottom", "middle", "top")
    gglook <- ggplot2::layer_data(p) %>% # to grab accurate x coordinates from existing ggplot obj since they've been transformed through position_dodge()
      mutate(lab = sprintf("%.1f",y))
    
    ps <- length(unique(gglook$PANEL))
    
    colour_vector <- gglook %>%
      select(colour, PANEL) %>%
      slice(rep(1:n(), ps)) %>%
      mutate(id = rep(1:ps, each = nrow(gglook))
             , colour2 = ifelse(id == PANEL, colour, NA_character_)
      ) %>% pull(colour2) %>% as.character()
    
    p <- p + geom_text(data = gglook, inherit.aes = FALSE, show.legend = F,
                     aes(x = x, y = y, label = lab, group = colour, text = "")
                     , color = colour_vector
                     # , hjust = .5, vjust = -1 # position = dodge, # these all don't work with plotly
                     , nudge_y = translate_pos(gtxt_y_pos) * y_nudge_val
                     , nudge_x = translate_pos(gtxt_x_pos) * x_nudge_val,
      )
    # p + geom_label(aes(label = MEAN), position = dodge, size = 3)
  }
  
  
  return(p)
}