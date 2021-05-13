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
IDEA_lineplot <- function(data, yvar, time, value = NULL, separate = "NONE", color = "NONE") {
  
  # library(dplyr)
  data0 <- data #%>% IDEA::varN_fctr_reorder()
    
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
  xl <- attr(d0[[time]], "label")
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
              SEM = round(std_err(!!val_sym, na.rm = T),2),
              N = n_distinct(USUBJID, na.rm = T),
              # n = n(),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(lower = MEAN - SEM, upper = MEAN + SEM)
  print(d)
  
  
  # if separate or color used, include those "by" variables in title
  var_title <- paste(yvar_label, "by", xl)
  print("var_title:")
  print(var_title)
  by_title <- case_when(
    separate != "NONE" & color != "NONE" ~ paste("\nby", attr(data[[color]], "label"), "and", attr(data[[separate]], "label")),
    separate != "NONE" ~ paste("\nby", attr(data[[separate]], "label")),
    color != "NONE" ~ paste("\nby", attr(data[[color]], "label")), 
    TRUE ~ ""
  )
  
  # Add common layers to plot
  p <- d %>%
    ggplot2::ggplot() +
    ggplot2::aes_string(x = time, y = "MEAN")  +
    ggplot2::geom_line() +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::labs(x = xl, y = yl, title = paste(var_title, by_title)) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   # plot.title = element_text(size = 16)
                   )
  
  # Add in plot layers conditional upon user selection
  if (separate != "NONE") { p <- p + ggplot2::facet_wrap(stats::as.formula(paste(".~", separate))) }
  if (color != "NONE") { p <- p + ggplot2::aes_string(color = color)}
  if (by_title != "") {p <- p + theme(plot.margin = margin(t = 1.2, unit = "cm"))}
  
  return(p)
}