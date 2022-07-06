#' tidyCDISC heatmap plot of endpoint correlations
#'
#' Create a heatmap comparing param/endpoints variable on both axis
#'
#' @param data Merged data to be used in plot
#' @param yvar_x character string of variable or paramcd names for x-axis
#' @param yvar_y character string of variable or paramcd names for y-axis
#' @param time character string of time variable
#' @param value character value: permitted values include "AVAL", "CHG", or
#'   "BASE"
#' @param cor_mthd character string. Defaults to 'pearson' for calculating
#'   correlation coefficients. See `?cor` for more supported test stats.
#' @param show_sig logical, whether or not to display p-values for significant
#'   tests statistics
#' @param sig_level dbl, defaulting to .05. Used to determine significance level
#'   for correlation tests performed
#'
#' @importFrom ggcorrplot cor_pmat
#'
#' @family popExp Functions
#' @export
#' @keywords popEx
#' 
#' @return A list object containing a ggplot object and a data frame with the corresponding correlations
#'   
app_heatmap <- function(data, yvar_x, yvar_y, time, value = "AVAL",
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
  # library(dplyr)
  data0 <- data %>%
    select(USUBJID, AVISIT, one_of("PARAMCD", "AVAL"), one_of(time, paste0(time,"N")),
           yvar_x_norm, yvar_y_norm)  %>%
    varN_fctr_reorder()
  
  # Start of function
  time_sym <- rlang::sym(time)
  if(time != "NONE"){ # results need to be grouped by time var
    
    time_vals0 <- get_levels(data0[[time]])
    # print(time_vals0)
    time_vals <- time_vals0[time_vals0 != "" & !is.na(time_vals0)]
    
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
        wide_dat <- non_pcd_dat
      } else {
        stop("No Valid X or Y Vars chosen")
      }
      
      # get corrs
      m0 <- wide_dat %>% select_if(is.numeric) %>%
        cor(., wide_dat %>% select(yvar_x), use = "na.or.complete", method = cor_mthd)
      m <- subset(m0, rownames(m0) %in% yvar_y)
      c <- m %>% round(5) %>% as.data.frame() %>%
        mutate(!!time_sym := time_val,
               param_y = row.names(.))
      row.names(c) <- NULL
      
      # Calculate p-value matrix
      p.mat <- wide_dat %>% dplyr::select_if(is.numeric) %>% ggcorrplot::cor_pmat()
      p.vals <- t(p.mat[yvar_y, yvar_x, drop = F]) %>%
        as.data.frame.table(responseName = "pval") %>%
        rename_with(~"param_x", "Var1") %>%
        rename_with(~"param_y", "Var2")
      
      # bring it all together
      c_long <- c %>%
        # mutate(across(yvar_x, function(col) sprintf("%.3f", col))) %>%
        tidyr::pivot_longer(cols = yvar_x, names_to = "param_x", values_to = "corr") %>%
        select(time, param_x, param_y, everything()) %>%
        arrange(across(vars("param_x", "param_y"))) %>% #, one_of(time)  
        filter(param_y != param_x) %>%
        left_join(p.vals)
      
      return(c_long)
    })
    
    g
    
    gathered <- g %>%
      select(-pval) %>%
      mutate(corr = sprintf("%.3f", corr)) %>%
      tidyr::pivot_wider(names_from = time,
                         values_from = "corr") %>%
      rename_with(~"Parameter Y", "param_y") %>%
      rename_with(~"Parameter X", "param_x")
    
    # head(gathered, 10)
    
    # Create geom_tile
    tile_data <- g %>%
      mutate(
        pval_hover = sprintf("%.4f", pval),
        corr_lab_hover = sprintf("%.3f", corr),
        corr_lab = case_when(show_sig == F ~ sprintf("%.2f", corr),
                             pval <= sig_level ~ sprintf("%.2f", corr),
                             TRUE ~ ""),
        param_y = factor(param_y, levels = rev(yvar_y))
      )

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
      wide_dat <- non_pcd_dat
    } else {
      stop("No Valid X or Y Vars chosen")
    }
    
    # library(dplyr)
    m0 <- wide_dat %>% select_if(is.numeric) %>%
      cor(., wide_dat %>% select(yvar_x), use = "na.or.complete", method = cor_mthd)
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
        pval_hover = sprintf("%.4f", pval),
        corr_lab_hover = sprintf("%.3f", corr),
        corr_lab = case_when(show_sig == F ~ sprintf("%.2f", corr),
                             pval <= sig_level ~ sprintf("%.2f", corr),
                             TRUE ~ ""),
        param_y = factor(param_y, levels = rev(yvar_y))
      )
   
    
    
  }
  # make sure the factor levels of tile_data match those of data0
  if (time != "NONE") {
    lvls <- get_levels(data0[[time]])
    tile_data <- tile_data %>%
      mutate(!!time_sym := factor(!!time_sym, levels = lvls))
  }
  
  time_lab <- ifelse(rlang::is_empty(attr(data[["time"]], "label")), time, attr(data[[time]], "label"))
  
  # library(ggplot2)
  p <- ggplot2::ggplot(tile_data, aes(param_x, param_y, fill=corr, label = corr_lab,
         text = paste0(
           "<b>Param X: ", param_x,
           "<br>Param Y: ", param_y,
           "<br>Corr coeff: ", corr_lab_hover,
           "<br>Corr method: ", cor_mthd,
           "<br>P-value: ", pval_hover,
           ifelse(rep(time == "NONE", nrow(tile_data)), "", paste0("<br>",time,": ", !!time_sym)),
           "</b>")
  ))  +
    ggplot2::geom_tile(height=0.8, width=0.8) +
    ggplot2::geom_text(cex = 4.5) +
    ggplot2::scale_fill_gradient2(low = "#3B9AB2", mid = "#EEEEEE", high = "#F21A00") +
    ggplot2::theme_minimal() +
    # coord_equal() +
    ggplot2::labs(title = paste("Endpoint Corrlation Matrix", ifelse(time == "NONE", "", paste("by", time_lab))),
      x ="X Parameter(s)",y = "Y Parameter(s)", fill = "Corr") +
    ggplot2::theme(axis.title=ggplot2::element_text(colour="gray20"),
          axis.text.x=ggplot2::element_text(size=13, angle=0, vjust=1, hjust=1, 
                                   margin = ggplot2::margin(-3,0,10,0)),
          axis.text.y=ggplot2::element_text(size=13, margin = ggplot2::margin(0,-3,0,10)),
          panel.grid.major=ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 16))
  if (time != "NONE") { 
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste(".~", time)), scales = "free")
  }
  p
  

  my_d <- gathered #%>%
  #   ungroup() %>%
  #   rename_with(toupper) %>%
  #   rename_with(~y_lab, "MEAN") %>%
  #   rename_with(~"Std. Error", "SEM") %>%
  #   rename_with(~"Std. Deviation", "STD") %>%
  #   rename_with(~"Visit", time) 
 
  
  return(list(plot = p, data = my_d))
}






