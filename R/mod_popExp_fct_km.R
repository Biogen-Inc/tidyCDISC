#' IDEA Kaplan-Meier Curve
#' 
#' Create scatter plot where if the variables are numeric then they
#' are plotted, and if they are PARAMCD's then a week and value 
#' must be selected for plotting.
#' 
#' @param data Merged data to be used in plot
#' @param yvar Selected xy-axis 
#' @param group namne of grouping variable (categorical or factor)
#' @param points logical, whether to plot + symbols when patients censored
#' @param ci logical, whether the curve(s) should be accompanied with a 95% CI
#' 
#' @importFrom stats as.formula
#' @importFrom GGally ggsurv
#' @importFrom survival survfit Surv
#' 
#' @family popExp functions
#' 
IDEA_km_curve <- function(data, yvar, resp_var, group = "NONE", points = TRUE, ci = FALSE) {
    
  # resp_var_sym <- rlang::sym(resp_var)
  
  # Filter data by param selected
  suppressWarnings(
    d <- data %>% 
      dplyr::filter(PARAMCD == yvar) %>%
      dplyr::select(USUBJID, PARAM, PARAMCD, one_of(resp_var), CNSR, one_of(group)) %>%
      dplyr::distinct()
  )
  # print(d)
  
  fit <- survival::survfit(
    as.formula(paste0("survival::Surv(",resp_var,", CNSR) ~ ", 
       if(group != "NONE" & !rlang::is_empty(group)) group else 1)),
    data = d)
  # print(fit)
  
  # Initialize title of variables plotted
  # if group used, include those "by" variables in title
  by_title <- case_when(
    group != "NONE" ~ paste("\nby", attr(data[[group]], "label")), 
    TRUE ~ ""
  )
  
  # generate plot
  p <- 
    GGally::ggsurv(fit,
                    order.legend = FALSE, # use data order, not survival order
                    CI = ci,
                    plot.cens = points) +
    # survminer::ggsurvplot(fit,conf.int = ci) +
    ggplot2::xlab(glue::glue("{unique(d$PARAM)}")) +
    ggplot2::theme_bw() +
    theme(
      text = element_text(size = 12),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16)
    ) +
    ggplot2::ggtitle(paste(unique(d$PARAM), by_title)
                     # ,subtitle = paste(by_title) # plotly won't automatically accept this
    )
  
  # Add in plot layers conditional upon user selection
  if (by_title != "") {
    p <- p + theme(plot.margin = margin(t = .6, unit = "cm")) #+
        # survminer::ggsurvplot_group_by(fit, group.by = group)
  }
  
  return(p)
}
