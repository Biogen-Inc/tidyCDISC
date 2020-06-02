
######################################################################################
# Inputs for Data Compliance Module
#
# Module found in "modules/data_compliance.R"
# Module description:
# a module that will interface with a list of data frames and either (I) display an
# error if needed variables don't exist and stop them from proceeding or (II) warn the
# user if if some columns are missing that are vital for the app to make sense, but 
# they can continue if they wish.
######################################################################################

# RULES for all dfs
# alldf_rules 
all_df_rules <- list(
  error = c("USUBJID"), # if error = "", then throwing an error. This needs
  warn = c("")
)

expl_rules <- 
  list(
    ADLB = list(error = c(""),
                warn = c("LBDT", "LBSTNRLO", "LBSTNRHI")),
    ADMH = list(error = c("","MHCAT"),
                warn = c("MHSTDTC", "MHENDTC", "MHDECOD", "MHTERM")),
    ADCM = list(error = c(""),
                warn = c("CMSTDT", "CMDECOD")),
    ADAE = list(error = c(""),
                warn = c("AESTDT", "AEDECOD", "AESEV", "AESER"))
  )


# dfWith_rules
df_incl_rules <- 
  list(
    PARAMCD = list(error = c(""),
                   warn = c("AVISITN", "VISIT", "AVISIT", "PARAMCD", "PARAM", "AVAL", "CHG", "BASE")) # GOOD
  )




############################################################################################
# Define the UI for the "Help" module, which simply displays all the rules in a nice format
############################################################################################
#' Some Roxygen notes
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import rlang
#' @noRd
#' 
gather_rules <- function(input, output, session,
                         all_df_rules = list( list(error = c(""), warn = c("")) ),
                         expl_rules = list( list(error = c(""), warn = c("")) ),
                         df_incl_rules = list( list(error = c(""), warn = c("")) )
) {
  if((is.null(expl_rules) | is.null(names(expl_rules))) & 
     (is.null(df_incl_rules) | is.null(names(df_incl_rules))) &
     (is.null(all_df_rules) | is.null(names(all_df_rules))) 
  ) {
    stop("No Rules Supplied. Without rules, the data compliance module is useless. Please remove the Module.")
  }
  
  # rules that apply to all df's loaded
  if(!is.null(all_df_rules) & !is.null(names(all_df_rules))) {
    err0 <- unique(unlist(all_df_rules$error)) 
    err <- if(err0 == "") character(0) else err0
    wrn0 <- unique(unlist(all_df_rules$warn))
    wrn <- if(wrn0 == "") character(0) else wrn0
  } else {
    err <- character(0) # ""
    wrn <- character(0) # ""
  }
  
  #  explicit rules for specific df's
  if(!is.null(expl_rules) & !is.null(names(expl_rules))) {
    hdf <- 
      lapply(expl_rules, data.frame, stringsAsFactors = FALSE) %>%
      data.table::rbindlist(fill=TRUE, idcol = "df")
    
    hdf_err <- 
      hdf %>%
      distinct(df,error) %>%
      subset(error != '') %>%
      group_by(df) %>%
      summarize(p = paste(error, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df, p, sep = ": ")) %>%
      distinct%>%
      pull(f)
    
    hdf_wrn <-
      hdf %>%
      distinct(df,warn) %>%
      subset(warn != "") %>%
      group_by(df) %>%
      summarize(p = paste(warn, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df, p, sep = ": ")) %>%
      pull(f)
    
  } else {
    hdf_err <- character(0) # "" # data.frame(df = character(), error = character(), warn = character())
    hdf_wrn <- character(0) # ""
  }
  
  # Rules for data frames containing certain vars (df_vars)
  if(!is.null(df_incl_rules) & !is.null(names(df_incl_rules))) {
    dfw <-
      lapply(df_incl_rules, data.frame, stringsAsFactors = FALSE) %>%
      data.table::rbindlist(fill=TRUE, idcol = "df_var")
    
    dfw_err <-
      dfw %>%
      distinct(df_var,error) %>%
      subset(error != "") %>%
      group_by(df_var) %>%
      summarize(p = paste(error, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df_var, p, sep = ": ")) %>%
      pull(f)
    
    dfw_wrn <-
      dfw %>%
      distinct(df_var,warn) %>%
      subset(warn != "") %>%
      group_by(df_var) %>%
      summarize(p = paste(warn, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df_var, p, sep = ": ")) %>%
      pull(f)
    
  } else {
    # dfw <- data.frame(df_var = character(), error = character(), warn = character())
    dfw_err <- character(0) # ""
    dfw_wrn <- character(0) # ""
  }
  
  ui<- tagList( # start UI of modal
    
    # Notes... can't add footer to bsModal
    # HTML("Note: The user will be alerted if files are uploaded and these variables (1) don't exist or (2) are completely empty / missing"),
    
    # Rules for All Data Sets
    tagList(
      if(!is_empty(err) | !is_empty(wrn)){
        tagList(
          br(),
          h5(strong("Rules for All Data Sets")),
          div(style = "font-size: 12px;", tagList(
            if(!is_empty(err)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Required: ",paste(err, collapse = ", "))),
                br(),br(),
              )
            } else {""},
            if(!is_empty(wrn)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended: ",paste(wrn, collapse = ", "))),
                br(),br(),
              )
            } else {""}
          ))
        )
      }),
    
    # Specific Rules for Specific Data Sets
    tagList(
      if(!is_empty(hdf_err) | !is_empty(hdf_wrn)){
        tagList(
          br(),
          h5(strong("Specific Rules for Specific Data Sets")),
          div(style = "font-size: 12px;", tagList(
            if(!is_empty(hdf_err)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Required:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(hdf_err, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""},
            if(!is_empty(hdf_wrn)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(hdf_wrn, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""}
          )) # end div
        )
      }),
    
    # Rules for Data Sets That Contain Certain Variables
    tagList(
      if(!is_empty(dfw_err) | !is_empty(dfw_wrn)){
        tagList(
          br(),
          h5(strong("Rules for Data Sets That Contain Certain Variables")),
          div(style = "font-size: 12px;", tagList(
            if(!is_empty(dfw_err)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Required:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(dfw_err, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""},
            if(!is_empty(dfw_wrn)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(dfw_wrn, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""}
          )) # end div
        )
      })
  ) # end of tagList UI
  
  return(ui)
  
  
}
###########################################################
# Run it help module UI once, before loading the app
###########################################################
rulesUI <- gather_rules(all_df_rules = all_df_rules,
                        expl_rules = expl_rules,
                        df_incl_rules = df_incl_rules)









#' Helper function to gather requirements for modals. Output produces gt & df object
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import rlang
#' @import gt
#' @noRd
#################################################################################
# This function validates the rules entered and creates data frames & gt objects
# for display in modals that pop up upon load. The function also returns a list
# of data frames just in case a loaded df doesn't meet one of the requirements...
# it will be removed.
#################################################################################

gather_reqs <- function(input, output, session, 
                        disp_type = c("error","warn"),
                        datalist = reactive(NULL),
                        all_df_rules = list( list(error = c(""), warn = c("")) ),
                        expl_rules = list( list(error = c(""), warn = c("")) ),
                        df_incl_rules = list( list(error = c(""), warn = c("")) )
) {
  
  if(!(disp_type %in% c("error","warn"))) {
    stop("User must specify either 'error' or 'warn' for disp_type arugment")
  }
  
  if((is.null(expl_rules) | is.null(names(expl_rules))) & 
     (is.null(df_incl_rules) | is.null(names(df_incl_rules))) &
     (is.null(all_df_rules) | is.null(names(all_df_rules))) 
  ) {
    stop("No Rules Supplied. Without rules, the data compliance module is useless. Please remove the Module.")
  }
  
  
  
  # Create rules that apply to all df's loaded
  if(!is.null(all_df_rules) & !is.null(names(all_df_rules))) {
    
    # Validate that rules lists were constructed correctly: sublists are error and warn
    ad_sl_nms_correct <- all(unlist(map(.x = 1:length(all_df_rules), function(x) all(names(all_df_rules[[x]]) %in% c("error","warn")))))
    if(!ad_sl_nms_correct) stop("Names must be 'error' and 'warn' for each element of 'all_df_rules'")
    
    alldf <- list()
    alldf <- map(.x = names(datalist()), function(x) alldf[[x]] <- all_df_rules) %>%
      setNames(names(datalist()))%>%
      lapply(data.frame, stringsAsFactors = FALSE) %>%
      rbindlist(fill=TRUE, idcol = "df") %>%
      subset(df %in% names(datalist())) %>% 
      mutate(type_col = if(disp_type == "error") error else warn) %>% 
      subset(type_col != "") %>% 
      distinct(df, type_col)
    # alldf # peek
  } else{
    # empty data frame
    alldf <- data.frame(df = character(), type_col = character())
  }
  
  
  # Create a explicit rules for specific df's
  if(!is.null(expl_rules) & !is.null(names(expl_rules))) {
    
    # Validate that rules lists were constructed correctly: sublists are error and warn
    expl_sl_nms_correct <- all(unlist(map(.x = 1:length(expl_rules), function(x) all(names(expl_rules[[x]]) %in% c("error","warn")))))
    if(!expl_sl_nms_correct) stop("Sublist Names must be 'error' and 'warn' for each element of 'expl_rules'")
    
    hdf <- lapply(expl_rules, data.frame, stringsAsFactors = FALSE) %>%
      data.table::rbindlist(fill=TRUE, idcol = "df") %>%
      subset(df %in% names(datalist())) %>% 
      mutate(type_col = if(disp_type == "error") error else warn) %>% 
      subset(type_col != "") %>% 
      distinct(df, type_col)
  } else{
    # empty data frame
    hdf <- data.frame(df = character(), type_col = character())
  }
  
  
  # Rules for data frames containing certain vars? (df_vars)
  if(!is.null(df_incl_rules) & !is.null(names(df_incl_rules))) {
    df_incl_sl_nms_correct <- all(unlist(map(.x = 1:length(df_incl_rules), function(x) all(names(df_incl_rules[[x]]) %in% c("error","warn")))))
    if(!df_incl_sl_nms_correct) stop("Sublist Names must be 'error' and 'warn' for each element of 'df_incl_rules'")
    
    # Organize into a dataframe & get concise initial reqs
    dfw_type <-
      lapply(df_incl_rules, data.frame, stringsAsFactors = FALSE) %>%
      data.table::rbindlist(fill=TRUE, idcol = "df_var") %>%
      mutate(type_col = if(disp_type == "error") error else warn) %>%
      subset(type_col != "") %>% 
      distinct(df_var, type_col) 
    
    if(nrow(dfw_type) > 0){
      # expand args to all loaded df's
      dfw_args <- 
        expand.grid(df = names(datalist()), df_var = dfw_type$df_var, type_col = dfw_type$type_col) %>% 
        mutate_if(is.factor, as.character) %>% 
        inner_join(dfw_type, by = c("df_var","type_col"))
      
      # run args through df's to see which one's exist
      dw <-
        dfw_args %>%
        mutate(
          col_exist = unlist(pmap(dfw_args,function(df, df_var, type_col) 
            dfw_type$type_col[dfw_type$df_var == df_var & dfw_type$type_col  == type_col ] %in% colnames(datalist()[[df]])))
        ) %>%
        subset(col_exist == T) %>%
        distinct(df, df_var, type_col) %>%
        subset(df %in% names(datalist())) %>%
        select(-df_var)
    } else {
      # empty data frame
      dw <- data.frame(df = character(), type_col = character())
    }
    
    # dw
  } else {
    # empty data frame
    dw <- data.frame(df = character(), type_col = character())
    
  }
  
  # now stack the hard & df_with rules to get a unique set of rules to calc if pass / fail (doesn't exist or missing)
  if(
    suppressWarnings(
      alldf %>%
      union(hdf) %>%
      union(dw) 
    ) %>%
    distinct(df, type_col) %>% 
    subset(type_col != "") %>% 
    nrow() == 0
  ) {
    # stop("No Rules Supplied. Without rules, the data compliance module is useless. Please remove the Module.")
    pf <- data.frame(df = character(), type_col = character(),
                     not_exist_disp = character(), missing_disp = character())
    tab <- NULL
    
  } else {
    
    pf <-
      suppressWarnings(
        alldf %>%
          union(hdf) %>%
          union(dw) 
      ) %>%
      distinct(df, type_col) %>%
      arrange(df, type_col) %>%
      mutate(
        type = disp_type,
        # exist0 = map(.x = df, function(x) type_col[df == x] %in% colnames(datalist()[[x]])),
        not_exist = !unlist(map2(.x = df, .y = type_col, function(x,y) y %in% colnames(datalist()[[x]]))),
        # unfortunately, the variables that don't exist throw this calculation off.. so we were extremely explicit below
        missing = ifelse(not_exist == TRUE,
                         ifelse(disp_type == "error", FALSE,TRUE),
                         unlist(map2(.x = df, .y = type_col, function(x, y)
                           all(as.character(datalist()[[x]][,type_col[df == x & type_col == y & not_exist == F]]) == "") |
                             all(is.na(datalist()[[x]][,type_col[df == x & type_col == y & not_exist == F]]))
                         )) ) # Here, we'd rather point out that a column doesn't exist instead of being missing
        
      ) %>%
      mutate(not_exist_disp = ifelse(not_exist,"X",""),
             missing_disp = ifelse(missing,"X",""),
      )%>%
      subset(not_exist | missing) %>%
      select(df, type_col, not_exist_disp, missing_disp)
    
    # pf  
    
    # modify the table displayed using gt, remove a column if just exporting warnings
    tab <- pf %>%
      gt(rowname_col = "type_col" , groupname_col = "df") %>%
      cols_label(not_exist_disp = "Doesn't Exist", missing_disp = "Missing Data") %>%
      text_transform(
        locations = list(cells_body(columns = vars(not_exist_disp), rows = not_exist_disp == "X"),
                         cells_body(columns = vars(missing_disp), rows = missing_disp == "X")),
        fn = function(X) local_image(filename = "www/red_x.png", height = 15) # test_image(type = "png") # web_image(url = r_png_url, height = 15)
      ) %>%
      tab_header(
        title = paste(ifelse(disp_type == "error", "Please", "Optional:"),"reconcile variables below"),
        subtitle = ifelse(disp_type == "error", "and re-upload data",
                          "to experience the app's full functionality")
      ) %>%
      tab_stubhead(label = "Data") %>%
      tab_style(style = cell_text(weight = "bold"), locations = cells_stubhead()) %>%
      cols_align("center") %>%
      tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) #%>% # bold group col groupnames
    # tab_source_note(html(paste(local_image(filename = "www/red_x.png", height = 15)
    #                            , "indicates variables that need attention")))
    
    if(disp_type == "warn") {
      tab <- tab %>% cols_hide(vars(not_exist_disp))
    }
  }
  
  return(list(df_list = 
                if(disp_type == "warn") {
                  datalist()
                } else {
                  datalist()[!(names(datalist()) %in% unique(pf$df))]
                },
              gt = tab,
              df = pf)
  )
}





















