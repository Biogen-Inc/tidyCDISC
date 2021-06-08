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
IDEA_heatmap <- function(data, yvar_x, yvar_y, time, value = "AVAL",
                         cor_mthd = "pearson", show_sig = F, sig_level = .05) {
  
  
  
  # if(!(yvar %in% colnames(data0))){
  
  pcd_exists <- any(!yvar_x %in% colnames(data)) | any(!yvar_y %in% colnames(data))
  yvar_x_pcd <- yvar_x[!yvar_x %in% colnames(data)]
  yvar_y_pcd <- yvar_y[!yvar_y %in% colnames(data)]
  
  non_pcd_exists <- any(yvar_x %in% colnames(data)) | any(yvar_y %in% colnames(data))
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
    
    # library(dplyr)
    m0 <- wide_dat %>% select_if(is.numeric) %>%
      cor(., wide_dat[,yvar_x], use = "na.or.complete", method = cor_mthd)
    m <- subset(m0, rownames(m0) %in% yvar_y)
    
    gathered <- m %>%
      round(5) %>% as.data.frame() %>%
      mutate(param_y = row.names(.)) %>%
      mutate(across(yvar_x, function(col) sprintf("%.3f", col))) %>% 
      select(param_y, everything()) %>%
      rename_with(~"Parameter Y", "param_y")  
    row.names(gathered) <- NULL
    # head(gathered, 10)
   
    
    # Calculate p-value matrix
    p.mat <- wide_dat %>% dplyr::select_if(is.numeric) %>% ggcorrplot::cor_pmat()
    
    # Create geom_tile
    tile_data <- t(m) %>%
      as.data.frame.table(responseName = "corr") %>%
      left_join(
        t(p.mat[yvar_y, yvar_x, drop = F]) %>%
          as.data.frame.table(responseName = "pval")
      ) %>%
      rename_with(~"param_x", "Var1") %>%
      rename_with(~"param_y", "Var2") %>%
      mutate(
        corr_lab = case_when(show_sig == F ~ sprintf("%.2f", corr),
                             pval <= sig_level ~ sprintf("%.2f", corr),
                             TRUE ~ ""),
        param_y = factor(param_y, levels = rev(yvar_y))
      )
   
    
    # library(ggplot2)
    p <- ggplot(tile_data, aes(param_x, param_y, fill=corr, label = corr_lab)) +
      geom_tile(height=0.8, width=0.8) +
      geom_text(cex = 4.5) +
      scale_fill_gradient2(low = "#3B9AB2", mid = "#EEEEEE", high = "#F21A00") +
      theme_minimal() +
      # coord_equal() +
      labs(x="X Parameter(s)",y="Y Parameter(s)",fill="Corr") +
      theme(axis.title=element_text(colour="gray20"),
            axis.text.x=element_text(size=13, angle=0, vjust=1, hjust=1, 
                                     margin=margin(-3,0,10,0)),
            axis.text.y=element_text(size=13, margin=margin(0,-3,0,10)),
            panel.grid.major=element_blank())
    # if (time != "NONE") { p <- p + ggplot2::facet_wrap(stats::as.formula(paste(".~", separate))) }
    p
  }
  
  my_d <- gathered #%>%
  #   ungroup() %>%
  #   rename_with(toupper) %>%
  #   rename_with(~y_lab, "MEAN") %>%
  #   rename_with(~"Std. Error", "SEM") %>%
  #   rename_with(~"Std. Deviation", "STD") %>%
  #   rename_with(~"Visit", time) 
 
  
  return(list(plot = p, data = my_d))
}









# Check correlations (as scatterplots), distribution and print corrleation coefficient 
# p <- GGally::ggpairs(as.data.frame(m), title="correlogram with ggpairs()") 
# p <- GGally::ggcorr(data = wide_dat %>% select_if(is.numeric),
#                method = c("na.or.complete", cor_mthd),
#                # cor_matrix = m,
#                label = T, #label_alpha = T,
#                title="correlogram with ggpairs()")
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

# p.mat <- wide_dat %>% dplyr::select_if(is.numeric) %>% ggcorrplot::cor_pmat()
# t(p.mat[yvar_y, yvar_x])
# p <- ggcorrplot::ggcorrplot(t(m),
#                             lab = T,
#                             colors = c(low = "#3B9AB2",mid = "#EEEEEE",high = "#F21A00"),
#                             p.mat = t(p.mat[yvar_y, yvar_x]),
#                             title = "Correlation Matrix")
# p

