######################################################################################
# a module that will interface with the data import module and either (I) display an
# error if needed variables don't exist and stop them from proceeding or (II) warn the
# user if if some columns are missing that are vital for the app to make sense, but 
# they can continue if they wish.
######################################################################################




############################################################################################
# User Interface (Modals)
############################################################################################

dataComplyUI <- function(id, label = "Check if Data Complies with Rules") {
  
  ns <- NS(id)
  tagList(
    # tableOutput(ns("console")) #,
    # gt_output(ns("err_gt")),
    # "" # Just need something in the UI, otherwise module produces an error
    # uiOutput(ns("data_comply_msg_ui")) ,
    
    # # create a hidden checkbox to use to manually trigger a modal?
    # checkboxInput(ns("trig"), label = NULL, value = T),
    # bsModal(ns("data_comply_msg"), "Data Compliance Error", trigger = NULL, size = "large",
    #       gt_output(ns("err_gt"))
    # )
    # bsExample("Modals")
    
  )
}




############################################################################################
# datalist() is a list of data frames
# rules is a DF with 3 columns: 
# (1) data frame name
# (2) Variable names needed for #3 or #4 below
# (3) Variables that should be in there for the app to make sense, but sure, if you're
#     desperate, they can make the column blank and it won't break the app
# (4) variables and every one of them has to be populated or things are going to break hard
############################################################################################

dataComply <- function(input, output, session,
                       datalist = reactive(NULL)#,
                       # expl_rules = list( list(error = c(""),warn = c("")) ),
                       # df_incl_rules = list( list(error = c(""),warn = c("")) )
                       ) {
  
  ns <- session$ns
  shinyjs::hide(id = "trig")
  
  
  # datafile <- reactive(datalist())
  # output$console <- renderTable({
  #   err_tab$df
  # })
  # any time the reactive datalist() changes, run this code
  observeEvent(datalist(), {
    # Run "the check" to see if any rules are violated
    err_tab <- gather_reqs(disp_type = "error",
                           datalist = datalist,
                           expl_rules = hard_rules,
                           df_incl_rules = dfWith_rules)
      
    # cat(paste("\nnrow:",nrow(err_tab$df)))
    
    # Display Modal Conditionally, don't allow escape
    if(nrow(err_tab$df) > 0){
      # output data compliance errors
      output$err_gt <- render_gt({
        err_tab$gt
      })
      showModal( modalDialog(footer = NULL, title = "ERROR!",
        tagList(
            gt_output(ns("err_gt")),
            br(),br()
          )
      ))
    }
    else {
      # if applicable Check for violationsi to "warnings" rules
      wrn_tab <- gather_reqs(disp_type = "warn",
                             datalist = datalist,
                             expl_rules = hard_rules,
                             df_incl_rules = dfWith_rules)
      # Display Modal Conditionally, allow escape
      if(nrow(wrn_tab$df) > 0){
        # output data compliance errors
        output$wrn_gt <- render_gt({
          wrn_tab$gt
        })
        showModal( modalDialog(title = "WARNING!",
           tagList(
             gt_output(ns("wrn_gt")),
             br(),br()
           )
        ))
      }
    }
  })
  
  # time the reactive datalist() changes, run this code
  # observeEvent(datalist(), {
  #   # Run "the check" to see if any rules are violated
  #   # err_tab <- gather_reqs(disp_type = "error",
  #   #                        datalist = datalist,
  #   #                        expl_rules = hard_rules,
  #   #                        df_incl_rules = dfWith_rules)
  #   cat(paste("\ntest"))
  #   
  #   disp_type = "error"
  #   datalist = datalist()
  #   expl_rules = hard_rules
  #   df_incl_rules = dfWith_rules
  # 
  #   # Create a DF of the hard rules
  #   hdfs <- lapply(expl_rules, data.frame, stringsAsFactors = FALSE)
  #   hdf <- data.table::rbindlist(hdfs, fill=TRUE, idcol = "df")
  # 
  #   # cat(paste("\n",paste(unlist(hdf),collapse = ", ")))
  #   # cat(paste("\n",hdf))
  # 
  #   # hdf
  #   # combine hard rules with dfWith Rules
  #   # Any of the rules df_vars in any of our datalists?
  #   # if(!is.null(dataWith_dfs)) # then proceed
  # 
  #   # if so, proceed
  #   dfw <- lapply(df_incl_rules, data.frame, stringsAsFactors = FALSE) %>%
  #     data.table::rbindlist(fill=TRUE, idcol = "df_var")
  # 
  #   # get concise initial reqs
  #   dfw_type<- dfw %>%
  #     mutate(type_col = if(disp_type == "error") error else warn) %>%
  #     distinct(df_var, type_col)
  # 
  #   # expand args to all loaded df's
  #   dfw_args <-
  #     expand.grid(df = names(datalist()), df_var = dfw_type$df_var, type_col = dfw_type$type_col) %>%
  #     inner_join(dfw_type)
  # 
  #   # run args through df's to see which one's exist
  #   dw <-
  #     dfw_args %>%
  #     mutate(
  #       col_exist = unlist(pmap(dfw_args,function(df, df_var, type_col)
  #         dfw_type$type_col[dfw_type$df_var == df_var & dfw_type$type_col  == type_col ] %in% colnames(datalist()[[df]])))
  #     ) %>%
  #     subset(col_exist == T) %>%
  #     distinct(df, df_var, type_col)
  #   # dw
  # 
  #   # now stack the hard & df_with rules to get a unique set of rules to calc if pass / fail (doesn't exist or missing)
  #   pf <-
  #     hdf %>%
  #     mutate(type_col = if(disp_type == "error") error else warn) %>%
  #     distinct(df, type_col) %>%
  #     union( dw %>% select(-df_var) ) %>%
  #     distinct(df, type_col) %>%
  #     arrange(df, type_col) %>%
  #     mutate(
  #       type = disp_type,
  #       # exist0 = map(.x = df, function(x) type_col[df == x] %in% colnames(datalist()[[x]])),
  #       not_exist = !unlist(map2(.x = df, .y = type_col, function(x,y) y %in% colnames(datalist()[[x]]))),
  #       # unfortunately, the variables that don't exist throw this calculation off.. so we were extremely explicit below
  #       missing = ifelse(not_exist == TRUE,
  #                        ifelse(disp_type == "error", FALSE,TRUE),
  #                        unlist(map2(.x = df, .y = type_col, function(x, y)
  #                          all(as.character(datalist()[[x]][,type_col[df == x & type_col == y & not_exist == F]]) == "") |
  #                            all(is.na(datalist()[[x]][,type_col[df == x & type_col == y & not_exist == F]]))
  #                        )) ) # Here, we'd rather point out that a column doesn't exist instead of being missing
  # 
  #     ) %>%
  #     mutate(not_exist_disp = ifelse(not_exist,"X",""),
  #            missing_disp = ifelse(missing,"X",""),
  #     )%>%
  #     subset(not_exist | missing) %>%
  #     select(df, type_col, not_exist_disp, missing_disp)
  #   # pf
  # 
  #   # modify the table displayed using gt, remove a column if just exporting warnings
  #   tab <- pf %>%
  #     gt(rowname_col = "type_col" , groupname_col = "df") %>%
  #     cols_label(not_exist_disp = "Doesn't Exist", missing_disp = "Missing Data") %>%
  #     text_transform(
  #       locations = list(cells_body(columns = vars(not_exist_disp), rows = not_exist_disp == "X"),
  #                        cells_body(columns = vars(missing_disp), rows = missing_disp == "X")),
  #       fn = function(X) local_image(filename = "www/red_x.png", height = 15) # test_image(type = "png") # web_image(url = r_png_url, height = 15)
  #     ) %>%
  #     tab_header(title = "Loaded Data not in Expected Format", subtitle = "Please reconcile variables below and reload") %>%
  #     tab_stubhead(label = "Data") %>%
  #     tab_style(style = cell_text(weight = "bold"), locations = cells_stubhead()) %>%
  #     cols_align("center") %>%
  #     tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>% # bold group col groupnames
  #     tab_source_note(html(paste(local_image(filename = "www/red_x.png", height = 15)
  #                                , "indicates variables that need attention")))
  # 
  #   if(disp_type == "warn") {
  #     tab <- tab %>% cols_hide(vars(not_exist_disp))
  #   }
  # 
  #   # return(list(gt = tab, df = pf))
  # 
  # 
  # 
  # 
  #   # Display Modal Conditionally, don't allow escape
  #   if(nrow(pf) > 0){
  #     showModal( modalDialog(footer = NULL, glue("Error!")))
  #   }
  #   # else {
  #   #   # if applicable Check for violationsi to "warnings" rules
  #   #   wrn_tab <- gather_reqs(disp_type = "warn",
  #   #                          datalist = datalist,
  #   #                          expl_rules = hard_rules,
  #   #                          df_incl_rules = dfWith_rules)
  #   #   # Display Modal Conditionally, allow escape
  #   #   if(nrow(wrn_tab$df) > 0){
  #   #     showModal( modalDialog(glue("Warning!")))
  #   #   }
  #   # }
  # })
  
}
















