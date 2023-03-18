#' Kaplan-Meier Curve
#' 
#' Create scatter plot where if the variables are numeric then they
#' are plotted, and if they are PARAMCD's then a week and value 
#' must be selected for plotting.
#' 
#' @param data Merged data to be used in plot
#' @param yvar Selected xy-axis 
#' @param resp_var character, the response variable (paramcd)
#' @param cnsr_var character, the censor variable. Usually CNSR
#' @param group character, variable name of grouping variable (categorical or factor)
#' @param points logical, whether to plot + symbols when patients censored
#' @param ci logical, whether the curve(s) should be accompanied with a 95\% CI
#' 
#' @importFrom stats as.formula
#' @importFrom GGally ggsurv
#' @importFrom survival survfit Surv
#' 
#' @family popExp functions
#' @keywords popEx
#' 
#' @return A ggplot object containing the KM curve plot
#' 
#' @noRd
app_km_curve <- function(data, yvar, resp_var, cnsr_var, group = "NONE", points = TRUE, ci = FALSE) {
    
  resp_var_sym <- rlang::sym(resp_var)
  
  # Filter data by param selected
  suppressWarnings(
    d <- data %>% 
      dplyr::filter(PARAMCD == yvar) %>%
      dplyr::select(USUBJID, PARAM, PARAMCD, one_of(resp_var), one_of(cnsr_var), one_of(group)) %>%
      filter(!is.na(!!resp_var_sym)) %>%
      dplyr::distinct()
  )
  # print(d)
  # if(group != "NONE"){
  #   group_counts <- 
  #     d %>%
  #     group_by_at(vars(one_of(group))) %>%
  #     summarize(n = n())
  #   print(group_counts)
  # }
  
  
  fit_formula <- paste0("survival::Surv(",resp_var,", ",cnsr_var,") ~ ", 
                        if(group != "NONE" & !rlang::is_empty(group)) group else 1)
  fit <- survival::survfit(as.formula(fit_formula),data = d)
  # print(fit_formula)
  # print(fit)  
  # print(summary(fit))
  
  # Initialize title of variables plotted
  # if group used, include those "by" variables in title
  by_title <- case_when(
    group != "NONE" ~ paste("\nby", best_lab(data, group)),
    TRUE ~ ""
  )
  
  # generate plot
  p <- 
    GGally::ggsurv(fit,
                    order.legend = FALSE, # use data order, not survival order
                    CI = ci,
                    plot.cens = points) +
    # survminer::ggsurvplot(fit,conf.int = ci) + # didn't work
    ggplot2::xlab(glue::glue("{unique(d$PARAM)}")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 16)
    ) +
    ggplot2::ggtitle(paste(unique(d$PARAM), by_title)
                     # ,subtitle = paste(by_title) # plotly won't automatically accept this
    )
  
  # Add in plot layers conditional upon user selection
  if (by_title != "") {
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(t = 1.2, unit = "cm")) #+
        # survminer::ggsurvplot_group_by(fit, group.by = group)
  }
  
  return(p)
}
