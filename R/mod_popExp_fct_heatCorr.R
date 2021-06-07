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
IDEA_heatmap <- function(data, yvar_x, yvar_y, time, value = "AVAL", cor_mthd = "pearson") {
  
  
  
  # if(!(yvar %in% colnames(data0))){
  
  pcd_exists <- any(!yvar_x %in% colnames(data)) | any(!yvar_y %in% colnames(data))
  yvar_x_pcd <- yvar_x[!yvar_x %in% colnames(data)]
  yvar_y_pcd <- yvar_y[!yvar_y %in% colnames(data)]
  
  non_pcd_exists <- any(yvar_x %in% colnames(data)) | any(!yvar_x %in% colnames(data))
  yvar_x_norm <- yvar_x[yvar_x %in% colnames(data)]
  yvar_y_norm <- yvar_y[yvar_y %in% colnames(data)]
  
  
  # create a smaller dataset with just the variables needed by function, and
  # reorder var w/ varn's
  data0 <- data %>%
    select(USUBJID, AVISIT, one_of("PARAMCD", "AVAL"), one_of(time, paste0(time,"N")),
           yvar_x_norm, yvar_y_norm)  %>%
    varN_fctr_reorder2()
  
  # Start of function
  if(time != "NONE"){ # results need to be grouped by time var
    
    time_vals0 <- getLevels(data0[[time]])
    print(time_vals0)
    time_vals <- time_vals0[time_vals0 != ""]
    time_sym <- rlang::sym(time)
    
    g <- purrr::map_dfr(time_vals, function(time_val){
      # time_val <- time_vals[2]
      
      # if any params were selected
      if(pcd_exists){
        param_dat <- data0 %>%
          filter(PARAMCD %in% unique(c(yvar_x_pcd, yvar_y_pcd))) %>%
          filter(!!time_sym == time_val) %>% #group_by(!!time_sym) ?
          select(USUBJID, PARAMCD, value) %>%
          tidyr::pivot_wider(names_from = "PARAMCD",
                             values_from = value)
      }
      # if any normal vars (non-params) were selected
      if(non_pcd_exists){
        non_pcd_dat <- data0 %>%
          filter(!!time_sym == time_val) %>% 
          select(USUBJID, yvar_x_norm, yvar_y_norm) %>% 
          distinct()
      }
      
      if(pcd_exists & non_pcd_exists){
        wide_dat <- param_dat %>% left_join(non_pcd_dat)
      } else if(pcd_exists & !non_pcd_exists){
        wide_dat <- param_dat
      } else if(!pcd_exists & non_pcd_exists){
        wide_dat <- non_param_dat
      } else {
        stop("No Valid X or Y Vars chosen")
      }
      
      c <- wide_dat %>% select(-USUBJID) %>% select_if(is.numeric) %>%
        cor(., wide_dat[,yvar_x], use = "na.or.complete", method = cor_mthd) %>%
        round(5) %>% as.data.frame() %>%
        mutate(!!time_sym := time_val,
               param_y = row.names(.))
      # print(c)
      # print(yvar_y)
      c <- c %>% filter(param_y %in% yvar_y)
      
      row.names(c) <- NULL
      return(c)
    })
    
    gathered <- g %>%
      mutate(across(yvar_x, function(col) sprintf("%.3f", col))) %>%
      tidyr::pivot_longer(cols = yvar_x, names_to = "param_x", values_to = "cor") %>%
      select(one_of(time), param_x, param_y, everything()) %>%
      arrange_at(vars("param_x", "param_y")) %>% #, one_of(time)  
      filter(param_y != param_x) %>%
      tidyr::pivot_wider(names_from = time,
                         values_from = "cor") %>%
      rename_with(~"Parameter Y", "param_y") %>%
      rename_with(~"Parameter X", "param_x")
    
    # head(gathered, 10)
    p <- qplot(x = mtcars$wt, y = mtcars$mpg)

  } else { 
    
    ####################
    # no time/ by_var
    ####################
    
    # if any params were selected
    if(pcd_exists){
      param_dat <- data0 %>%
        filter(PARAMCD %in% unique(c(yvar_x_pcd, yvar_y_pcd))) %>%
        filter(AVISIT != "") %>%
        select(USUBJID, AVISIT, PARAMCD, value) %>% 
        tidyr::pivot_wider(names_from = "PARAMCD",
                           values_from = value)
    }
    # if any normal vars (non-params) were selected
    if(non_pcd_exists){
      non_pcd_dat <- data0 %>%
        select(USUBJID, yvar_x_norm, yvar_y_norm) %>% 
        distinct()
    }
    
    if(pcd_exists & non_pcd_exists){
      wide_dat <- param_dat %>% left_join(non_pcd_dat)
    } else if(pcd_exists & !non_pcd_exists){
      wide_dat <- param_dat
    } else if(!pcd_exists & non_pcd_exists){
      wide_dat <- non_param_dat
    } else {
      stop("No Valid X or Y Vars chosen")
    }
    
    m0 <- wide_dat %>% select_if(is.numeric) %>%
      cor(., wide_dat[,yvar_x], use = "na.or.complete", method = cor_mthd)
    m <- m0[rownames(m0) %in% yvar_y,]
    
    gathered <- m %>%
      round(5) %>% as.data.frame() %>%
      mutate(param_y = row.names(.)) %>%
      mutate(across(yvar_x, function(col) sprintf("%.3f", col))) %>% 
      select(param_y, everything()) %>%
      # filter(param_y %in% yvar_y) %>%
      rename_with(~"Parameter Y", "param_y")  
    row.names(gathered) <- NULL
    # head(gathered, 10)

    # Check correlations (as scatterplots), distribution and print corrleation coefficient 
    # p <- GGally::ggpairs(as.data.frame(m), title="correlogram with ggpairs()") 
    # p <- GGally::ggcorr(data = wide_dat %>% select_if(is.numeric),
    #                method = c("na.or.complete", cor_mthd),
    #                # cor_matrix = m,
    #                label = T, #label_alpha = T,
    #                title="correlogram with ggpairs()")
    # http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
    
    p.mat <- wide_dat %>% dplyr::select_if(is.numeric) %>% ggcorrplot::cor_pmat()
    p <- ggcorrplot::ggcorrplot(t(m),
                                lab = T,
                                colors = c(low = "#3B9AB2",mid = "#EEEEEE",high = "#F21A00"),
                                p.mat = t(p.mat[yvar_y, yvar_x]),
                                title = "Correlation Matrix")
    # Can I facet this?
    # library(ggplot2)
    # ggplot2::ggplot_build(p)
    # p + ggplot2::facet_grid(~AGE)
  }
  
  my_d <- gathered #%>%
  #   ungroup() %>%
  #   rename_with(toupper) %>%
  #   rename_with(~y_lab, "MEAN") %>%
  #   rename_with(~"Std. Error", "SEM") %>%
  #   rename_with(~"Std. Deviation", "STD") %>%
  #   rename_with(~"Visit", time) 
  # 
  # if(err_bars) {
  #   my_d <- my_d %>%
  #     rename_with(~"Upper Bound", "UPPER") %>%
  #     rename_with(~"Lower Bound", "LOWER")
  # } else {
  #   my_d <- my_d %>% select(-UPPER, -LOWER)
  # }
  
  # if separate or color used, include those "by" variables in title
  # var_title <- paste(y_lab, "by", xl)
  # by_title <- case_when(
  #   separate != "NONE" & color != "NONE" ~ paste("\nby", attr(data[[color]], "label"), "and", attr(data[[separate]], "label")),
  #   separate != "NONE" ~ paste("\nby", attr(data[[separate]], "label")),
  #   color != "NONE" ~ paste("\nby", attr(data[[color]], "label")), 
  #   TRUE ~ ""
  # )
  # 
  # dodge <- ggplot2::position_dodge(.9)
  # time_sym <- rlang::sym(time)
  # color_sym <- rlang::sym(color)
  # 
  # # Add common layers to plot
  # p <- d %>%
  #   ggplot2::ggplot() +
  #   ggplot2::aes(x = !!time_sym, y = MEAN, group = 1,
  #     text = paste0(
  #       "<b>", time,": ", !!time_sym, "<br>",# y_lab, 
  #       ifelse(rep(err_bars, nrow(d)), paste0("MEAN + 1.96*SE: ", sprintf("%.1f",Upper), "<br>"), ""),# y_lab, 
  #       "MEAN: ", sprintf("%.1f",MEAN),
  #       ifelse(rep(err_bars, nrow(d)), paste0("<br>MEAN - 1.96*SE: ", sprintf("%.1f",Lower)), ""),
  #       "<br>SE: ", SEM,
  #       "<br>SD: ", STD,
  #       "<br>N: ", N,
  #       ifelse(rep(color == "NONE", nrow(d)), "", paste0("<br>",color,": ", !!color_sym)),
  #       # ifelse(rep(color, nrow(d)), paste0("<br>Color: ", sprintf("%.1f",color)), ""),
  #       "</b>"))  +
  #   ggplot2::geom_line(position = ggplot2::position_dodge(.91)) +
  #   ggplot2::geom_point(position = dodge, na.rm = TRUE) +
  #   ggplot2::labs(x = xl, y = y_lab, title = paste(var_title, by_title)) +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(text = element_text(size = 12),
  #                  axis.text = element_text(size = 12),
  #                  plot.title = element_text(size = 16)
  #                  )
  # 
  # # Add in plot layers conditional upon user selection
  # if (color != "NONE") { p <- p + ggplot2::aes_string(color = color, group = color) }
  # if (err_bars) {
  #   p <- p + ggplot2::aes(ymin = Lower, ymax = Upper) +
  #   ggplot2::geom_errorbar(position = dodge, width = 1.5)
  # }
  # if (separate != "NONE") { p <- p + ggplot2::facet_wrap(stats::as.formula(paste(".~", separate))) }
  # if (by_title != "") {p <- p + theme(plot.margin = margin(t = 1.2, unit = "cm"))}
  # 
  # if(label_points){
  #   x_scale <- layer_scales(p)$x$range$range
  #   if(all(!is.numeric(x_scale))){
  #     x_nums <- sort(as.numeric(as.factor(x_scale)))
  #     range <- diff(c(min(x_nums), max(x_nums)))
  #   } else {
  #     range <- diff(x_scale)
  #   }
  #   x_nudge_val <- range * .04 #* (plot_col_num /2)
  #   y_nudge_val <- diff(layer_scales(p)$y$range$range)*.04
  #   # gtxt_x_pos <- "right" #c("left", "middle", "right")
  #   # gtxt_y_pos <- "top"   #c("bottom", "middle", "top")
  #   gglook <- ggplot2::layer_data(p) %>% # to grab accurate x coordinates from existing ggplot obj since they've been transformed through position_dodge()
  #     mutate(lab = sprintf("%.1f",y))
  #   
  #   ps <- length(unique(gglook$PANEL))
  #   
  #   colour_vector <- gglook %>%
  #     select(colour, PANEL) %>%
  #     slice(rep(1:n(), ps)) %>%
  #     mutate(id = rep(1:ps, each = nrow(gglook))
  #            , colour2 = ifelse(id == PANEL, colour, NA_character_)
  #     ) %>% pull(colour2) %>% as.character()
  #   
  #   p <- p + geom_text(data = gglook, inherit.aes = FALSE, show.legend = F,
  #                    aes(x = x, y = y, label = lab, group = colour, text = "")
  #                    , color = colour_vector
  #                    # , hjust = .5, vjust = -1 # position = dodge, # these all don't work with plotly
  #                    , nudge_y = translate_pos(gtxt_y_pos) * y_nudge_val
  #                    , nudge_x = translate_pos(gtxt_x_pos) * x_nudge_val,
  #     )
  # }
  # if(add_vert){
  #   if(is.character(vert_x_int)){
  #     time_lvls <- getLevels(d[[time]])
  #     p <- p + geom_vline(xintercept = which(time_lvls == vert_x_int), color = "darkred")
  #   } else { # numeric
  #     p <- p + geom_vline(xintercept = as.numeric(vert_x_int), color = "darkred")
  #   }
  # }
  # if(add_hor){
  #   p <- p + geom_hline(yintercept = hor_y_int, color = "darkred")
  # }
  # p <- qplot(x = mtcars$wt, y = mtcars$mpg)
  
  
  return(list(plot = p, data = my_d))
}