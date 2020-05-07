library(shiny)
library(shinyjs)
library(tidyverse)
# remotes::install_github("JohnCoene/tippy")
library(tippy)
library(rvest)
# devtools::install_github("MayaGans/IDEAFilter") # don't use for production
# drat::addRepo("aaron-clark") # add a repo where the source file (tar.gz) file is hosted on gitHub
# getOption("repos") # check if the repo was added
# install.packages("IDEAFilter")
library(IDEAFilter)
library(shinyTime) # for IDEAFilter
library(haven)
library(DT)
library(shinyWidgets)
library(plotly)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(janitor)
library(rtf)
library(shinythemes)
library(rmarkdown)
library(shinytest)
library(reactable)
library(waiter)
library(timevis)
library(glue)
library(sjlabelled) 
library(data.table) 
library(gt)
library(shinyBS)

###############################################################
# make sure this repo exists before writing to manifest file!
###############################################################
# drat::addRepo("aaron-clark")
# getOption("repos")
# # devtools::install_github("rstudio/rsconnect")
# rsconnect::writeManifest()



options(shiny.sanitize.errors = FALSE)
options(bitmapType='cairo') 


source("global.R")

ui <- 
  tagList(
    tags$head(
      tags$script(HTML(htmljs)),
      tags$link(rel = "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      tags$head(tags$link(rel="shortcut icon", href="IDEA_FAVICON.ico")),
      tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js")
    ),
    useShinyjs(),
    use_waiter(), # include dependencies
    extendShinyjs(text = jscode),
    navbarPage(theme = "yeti.css",
               title = div(id="logo-id","IDEA", img(src="IDEA_ICON.png", style="float:left; padding-right:3px; height:25px; width:30px")), 
               id = "navbarID",
               windowTitle = "IDEA",
               tabPanel(
                 title = "Data",
                 dataUploadUI("datafile", "Import CSV")
                 ,dataComplyUI(id = "comply_id")
                 # ,verbatimTextOutput("console")
                 # ,tableOutput("console")
               ),
               tabPanel(
                 title = "TableGenerator", id = 't_gen',
                 tableGeneratorUI("table_generator")
               ),
               tabPanel(
                 title = "Population Explorer",
                 # dataUploadUI("popul", "Import CSV"),  
                 selectDataUI(id = "popul"),
                 PopuExplorUI(id = "popul")
               ),
               tabPanel(
                 title = "Individual Explorer",
                 # dataUploadUI("indvl", "Import CSV"),  
                 # selectDataUI(id = "indvl"), # Removed - Issue 74
                 IndvExplorUI(id = "indvl")
               )
    ),
    # Custom styling to override theme
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    # Add logo to top right corner
    tags$script(HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"logo.svg\" alt=\"alt\" style=\"float:right;width:66px;height:41px;\"> </a>`</div>');")),
    tags$script(src = "script.js"),
    tags$script(src = "recipe.js"),
    tags$style(HTML("
 
                    #browserModal .modal-dialog,
                    #browserModal .modal-body,
                    #browserModal .modal-footer {
                    background-color: #CF000F;
                    border-color: #CF000F;
                    color: white;
                    font-size: 20px;
                    }
                    
                    ")),
    inlineCSS(css),
    tags$head(tags$script(src = "analytics.js"))
  )

server <- function(input, output, session) {
  
  observeEvent(input$myBrowser , {
    if(str_detect(input$myBrowser, "IE")){
      showModal(tags$div(id="browserModal", modalDialog(footer = NULL,
        glue("This web app doesn't function with Internet Explorer. Please use a modern browser such as Google Chrome.")
      )))
    }    
  })
  
  
  # disable tab2 on page load
  js$disableTab()
  
  observeEvent(datafile()$ADSL, {
    # enable tab2 when clicking the button
    js$enableTab()
  })
  
  # Increase allowed file size to 4GB
  options(shiny.maxRequestSize = 4096*1024^2)
  
  # render the dataUpload module in Data tab
  datafile <- callModule(dataUpload, "datafile", stringsAsFactors = FALSE)
  
  ### Yo, this needs to be in the dataUpload module, so I can use the upload button to trigger
  ### my code to run...
  # Data compliance Modals: any time the reactive datalist() changes, run this code
  callModule(dataComply, "comply_id", datalist = datafile) # ,stringsAsFactors = FALSE
  
  # # # Data compliance Modals (if necessary): any time the reactive datalist() changes, run this code
  # observeEvent(datafile(), {
  #   # Run "the check" to see if any rules are violated
  #   err_tab <- gather_reqs(disp_type = "error",
  #                          datalist = datafile,
  #                          expl_rules = hard_rules,
  #                          df_incl_rules = dfWith_rules)
  #   # Display Modal Conditionally, don't allow escape
  #   if(nrow(err_tab$df) > 0){
  #     showModal( modalDialog(footer = NULL, glue("Error!")))
  #   }
  #   else {
  #     # if applicable Check for violationsi to "warnings" rules
  #     wrn_tab <- gather_reqs(disp_type = "warn",
  #                          datalist = datafile,
  #                          expl_rules = hard_rules,
  #                          df_incl_rules = dfWith_rules)
  #     # Display Modal Conditionally, allow escape
  #     if(nrow(wrn_tab$df) > 0){
  #       showModal( modalDialog(glue("Warning!")))
  #     }
  #   }
  # })
  
  
  # # time the reactive datalist() changes, run this code
  # observeEvent(datafile(), {
  #   # Run "the check" to see if any rules are violated
  #   # err_tab <- gather_reqs(disp_type = "error",
  #   #                        datalist = datafile,
  #   #                        expl_rules = hard_rules,
  #   #                        df_incl_rules = dfWith_rules)
  #   disp_type = "error"
  #   datalist = datafile()
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
  #   # Any of the rules df_vars in any of our datafiles?
  #   # if(!is.null(dataWith_dfs)) # then proceed
  #   
  #   # if so, proceed
  #   dfw <- lapply(df_incl_rules, data.frame, stringsAsFactors = FALSE) %>%
  #     data.table::rbindlist(fill=TRUE, idcol = "df_var") 
  #     
  # 
  #   # cat(paste("\ndfw$need:",dfw$need))
  #   
  #   # get concise initial reqs
  #   dfw_type<- dfw  %>%
  #     mutate(type_col = if(disp_type == "error") error else warn) %>%
  #     distinct(df_var, type_col)
  # 
  #   output$console <- renderTable({
  #     dfw_type
  #   })
  #   
  #   # expand args to all loaded df's
  #   dfw_args <-
  #     expand.grid(df = names(datalist), df_var = dfw_type$df_var, type_col = dfw_type$type_col) %>%
  #     inner_join(dfw_type)
  # 
  #   # run args through df's to see which one's exist
  #   dw <-
  #     dfw_args %>%
  #     mutate(
  #       col_exist = unlist(pmap(dfw_args,function(df, df_var, type_col)
  #         dfw_type$type_col[dfw_type$df_var == df_var & dfw_type$type_col  == type_col ] %in% colnames(datalist[[df]])))
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
  #       # exist0 = map(.x = df, function(x) type_col[df == x] %in% colnames(datalist[[x]])),
  #       not_exist = !unlist(map2(.x = df, .y = type_col, function(x,y) y %in% colnames(datalist[[x]]))),
  #       # unfortunately, the variables that don't exist throw this calculation off.. so we were extremely explicit below
  #       missing = ifelse(not_exist == TRUE,
  #                        ifelse(disp_type == "error", FALSE,TRUE),
  #                        unlist(map2(.x = df, .y = type_col, function(x, y)
  #                          all(as.character(datalist[[x]][,type_col[df == x & type_col == y & not_exist == F]]) == "") |
  #                            all(is.na(datalist[[x]][,type_col[df == x & type_col == y & not_exist == F]]))
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
  #   #                          datalist = datafile,
  #   #                          expl_rules = hard_rules,
  #   #                          df_incl_rules = dfWith_rules)
  #   #   # Display Modal Conditionally, allow escape
  #   #   if(nrow(wrn_tab$df) > 0){
  #   #     showModal( modalDialog(glue("Warning!")))
  #   #   }
  #   # }
  # })
  
  # render the tablegenerator module using the datafile from dataupload as an input
  table_generator <- callModule(tableGenerator, "table_generator", datafile = datafile)
  output$all_rows <- renderUI({ table_generator() })
  
  # Population Explorer
  callModule(PopuExplor, id = "popul", datafile = datafile)
  
  # Individual Explorer
  user_dat <- callModule(IndvExpl1Initial, "indvl", datafile = datafile)
  usubjid  <- callModule(IndvExpl2SelPatno , "indvl", datafile = datafile,  loaded_adams = user_dat$my_loaded_adams, filtered_dat = user_dat$all_data) #, dataselected
  callModule(IndvExpl3CheckGroup,  "indvl", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected
  callModule(IndvExpl4ChartPlotly, "indvl", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected
  
}

shinyApp(ui, server)
