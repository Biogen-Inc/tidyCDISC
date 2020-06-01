
fnIndvExplVisits <- function(
  watermark = FALSE,
  graph_output = "plotly",
  bds_data,
  usubjid,
  input_plot_hor,
  input_visit_var,
  input_plot_param,
  input_plot_adam,
  input_overlay_events,
  vline_dat,
  vv_dy_name
){
  # In the labs, label what the blue lines are in the legend or hover text.
  # make sure a LabCode has been selected
  
  INPUT_visit_var <- sym(input_visit_var)
  
  plot_dat <- 
    bds_data %>%
    filter(!(is.na(!!INPUT_visit_var)) & PARAMCD == input_plot_param) # make sure AVISITN is not missing
  
  if("Screening" %in% input_plot_hor){
    plot_scr <- plot_dat %>% subset(toupper(VISIT) == "SCREENING") %>% distinct(AVAL) %>% mutate(Visit = "Screening")
    # plot_dat <- plot_dat %>% subset(!(toupper(plot_dat$VISIT) == "SCREENING")) # removes screening point from plot
  }
  if("Baseline" %in% input_plot_hor){
    plot_base <- plot_dat %>% subset(toupper(AVISIT) == "BASELINE") %>% distinct(AVAL) %>% mutate(Visit = "Baseline")
    # plot_dat <- plot_dat %>% subset(!(toupper(plot_dat$AVISIT) == "BASELINE")) # removes baseline point from plot
  }
  
  if (nrow(plot_dat) > 0) {
    
    prm   <- unique(plot_dat$PARAM)
    
    if(input_plot_adam == "ADLB"){
      lohi <- paste("LO:",unique(plot_dat$LBSTNRLO),"HI:",unique(plot_dat$LBSTNRHI))
    }
    
    # GGPLOT2 OBJECT
    lb_plot <- ggplot(plot_dat, aes(x = !!INPUT_visit_var, y = AVAL)) + 
      geom_line() +
      geom_point(na.rm = TRUE, 
                 aes(text =
                       paste0(AVISIT,
                              "<br>",input_visit_var, ": ",!!INPUT_visit_var,
                              "<br>",input_plot_param ,": ",AVAL
                       )
                 )) +
      scale_x_continuous(breaks = seq(min(plot_dat[,input_visit_var]), max(plot_dat[,input_visit_var]), 30)) +
      labs(x = paste0("Study Visit (",input_visit_var,")"),
           y = prm,
           title = paste(prm,"by Relative Study Day"),
           subtitle = paste0(
             ifelse(input_plot_adam == "ADLB",
                    paste0("Note: Study's average ",input_plot_param," range shown in blue - ",lohi,"\n")
                    ,""),
             "USUBJID: ",usubjid)
      )	
    
    if(watermark & graph_output == "ggplot"){
      
      ## custom draw method to calculate expansion factor on-the-fly
      # drawDetails.watermark <- function(x, rot = 45, ...){
      #   cex <- convertUnit(unit(1,"npc"), "mm", val=TRUE) /
      #     convertUnit(unit(1,"grobwidth", textGrob(x$val)), "mm",val=TRUE)
      #   grid.text(x$lab,  rot=rot, gp=gpar(cex = cex, col="white",
      #                                      fontface = "bold", alpha = 0.5))
      # }
      
      lb_plot <- lb_plot +
        # annotation_custom(xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf,
        #                   grob(lab="IDEA: PROOF ONLY", cl="watermark"))
        
        # Smaller watermark
        annotate("text", x = Inf, y = -Inf, label = "IDEA: PROOF ONLY",
               hjust=1.1, vjust=-3.3, col="white", cex=20,
               fontface = "bold", alpha = 0.8)

    }
      
    
    # if a lengend is needed, let's just define the line colors and types in one place
    if(length(input_plot_hor) > 0 | length(input_overlay_events) > 0 & input_visit_var %in% vv_dy_name){
      
      names2 <- c("Milestones","Concomitant Meds","Adverse Events","Baseline","Screening") # ac: labels
      vline_eventtype_cols <- c(my_cols[1:3],my_gg_color_hue(2))
      v_event_cols <- setNames(vline_eventtype_cols,names2)
      
      # two dimensions in legend not really support in plotly
      # https://github.com/ropensci/plotly/issues/1164
      # dashes <- c("solid","dotted","dashed","solid","solid") 
      # v_event_lines <- setNames(dashes,names2)
      
      # leg_name <- ifelse(length(input_plot_hor) > 0 & length(input_overlay_events) > 0 & input_visit_var %in% vv_dy_name,NULL,"Event")
      
      lb_plot <- lb_plot +
        scale_color_manual(values= v_event_cols) #+ # , name = "Event"
      # two dimensions in legend not really support in plotly
      # https://github.com/ropensci/plotly/issues/1164
      # scale_linetype_manual(values = v_event_lines, name = NULL)
    }
    
    
    # plot vlines using events dataset
    if(length(input_overlay_events) > 0 & input_visit_var %in% vv_dy_name){ #& "ADLB" %in% loaded_adams() # overlay checkbox won't appear unless this is true
      if (!is.null(vline_dat)){
        if(nrow(vline_dat) > 0){
          
          lb_plot <- lb_plot + 
            geom_vline(data = vline_dat, aes(xintercept = !!INPUT_visit_var,
                                               colour = Event,
                                               # linetype = Event, # two dimensions in legend not really support in plotly
                                               text = paste0(input_visit_var, ": ",!!INPUT_visit_var,
                                                             "<br>", DECODE
                                               )
            ))
        }
      }
    }
    
    # If lab data, plot the normal low and high values for the drug, add a little space in the bottom margin
    if(input_plot_adam == "ADLB"){
      lb_plot <- lb_plot + 
        geom_hline(aes(yintercept = mean(LBSTNRLO)), colour = "blue") +
        geom_hline(aes(yintercept = mean(LBSTNRHI)), colour = "blue") +
        theme(
          plot.margin = margin(b = 1.2, unit = "cm") #t = 1, # used to put margin at top of graph for caption
        ) 
    }
    
    # Plotting hortizontal line
    if("Screening" %in% input_plot_hor){
      lb_plot <- lb_plot +
        geom_hline(plot_scr, mapping = aes(yintercept = AVAL, colour = Visit))
    }
    if("Baseline" %in% input_plot_hor){
      lb_plot <- lb_plot +
        geom_hline(plot_base, mapping = aes(yintercept = AVAL, colour = Visit))
    }
    # End: ggplot2 object
    
    
    
    # PLOTLY OBJECT
    if(graph_output == "plotly"){
      ly <- ggplotly(lb_plot, tooltip = "text") %>%
        layout(title = list(text = 
                              paste0(prm," by Study Visit<sup>",
                                     "<br>USUBJID: ",usubjid
                              ))) %>%
        config(displaylogo = FALSE, 
               modeBarButtonsToRemove= c('sendDataToCloud', 'hoverCompareCartesian','hoverClosestCartesian','autoScale2d'
                                         ,'select2d', 'lasso2d', 'toggleSpikelines'
                                         # , 'toImage', 'resetScale2d', 'zoomIn2d', 'zoomOut2d','zoom2d', 'pan2d'
               ))
      
      
      # Used to have lab param range of values in title
      # , ifelse(input_plot_adam == "ADLB",paste0("<br>Study's average range shown in ",'<em style="color:blue">',"blue",'</em> ',lohi),""),
      # "</sup>")))
      
      # instead, request was made to add caption to bottom of graph
      if(input_plot_adam == "ADLB"){
        ly <- ly %>%
          add_annotations(x = ggplot_build(lb_plot)$layout$panel_params[[1]]$x.range[1],
                          y = -.15, # 15% below graph
                          yref = "paper",
                          text = paste0("<br>Note: Study's average ",input_plot_param," range shown in ",'<em style="color:blue">',"blue",'</em> ',lohi),
                          xanchor = 'left',
                          showarrow = F)
      }
      
      # if watermark is desired, it can be added here
      if(watermark){
        ly <- ly %>%
          layout(annotations = list(text="IDEA: PROOF ONLY",
                                    xref = "paper",
                                    yref = "paper",
                                    opacity = 0.1,
                                    showarrow = F,
                                    font=list(size = 40),
                                    textangle=-35)
          )
        
        # # Doesn't work
        # plotly_IMAGE(
        #   ly,
        #   width = 800,
        #   height = 600,
        #   format = "png",
        #   scale = 1,
        #   out_file = "output.png")
        
      } #else {
        # Doesn't work
        # The plotly image that get's downloaded should always have the image
        # plotly_IMAGE(
        #   ly %>%
        #     layout(annotations = list(text="IDEA: PROOF ONLY",
        #                               xref = "paper",
        #                               yref = "paper",
        #                               opacity = 0.1,
        #                               showarrow = F,
        #                               font=list(size = 40),
        #                               textangle=-35)
        #     )
        #   ,
        #   width = 800,
        #   height = 600,
        #   format = "png",
        #   scale = 1,
        #   out_file = "output.png")
      # }
      
    }
    
    return(if(graph_output == "ggplot") lb_plot else ly)
  } # if (nrow(plot_dat) > 0)
  
  
  
  
}






