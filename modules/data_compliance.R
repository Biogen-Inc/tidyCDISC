
######################################################################################
# a module that will interface with the data import module and either (I) display an
# error if needed variables don't exist and stop them from proceeding or (II) warn the
# user if if some columns are missing that are vital for the app to make sense, but 
# they can continue if they wish.
######################################################################################



############################################################################################
# User Interface
############################################################################################

dataComplyUI <- function(id, label = "Check if Data Complies with Rules", showRules = T) {

  ns <- NS(id)
  tagList(
    if(showRules == TRUE){
      tagList(
        actionButton(ns("clickRules") #actionBttn
           , label = NULL
           , icon = icon("question-circle")
           , class = "btn-start"
        ),
        
        bsModal(id = ns("upload_rules"),
                title = div(style = "text-align:center; font-weight:bold;","ADaM-ish Upload Rules"),
                trigger = ns("clickRules"),

                rulesUI # defined in "data_compliance_objects.R"
                
              ) # end of bsModal
        
        
      )
    }
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

dataComply <- function(input, output, session, datalist = reactive(NULL), dismissErrBttn = T) {
  
  ns <- session$ns
  rv <- reactiveValues(return_dl = datalist())
  
  # If upload help button is pressed in error modal
  # observeEvent(input$clickRulesError, {
  #   removeModal()
  #   updateActionButton(session, "clickRules", class = "btn-postModal")
  #   # Sys.sleep(2)
  #   # output$q_color <- renderUI({
  #   #   tags$head(
  #   #     tags$style(HTML('#clickRules{background-color:red}'))
  #   #   )
  #   # })
  #   # click("clickRules")
  #   # showModal(modalDialog(div("Test error")))
  #   # toggleModal(session, modalId = 'upload_rules', toggle = "open")
  # })
  
  # observeEvent(input$clickRulesWarn, {
  #   removeModal()
  #   updateActionButton(session, "clickRules", class = "btn-postModal")
  #   # Sys.sleep(2)
  #   # click("clickRules")
  #   # showModal(modalDialog(div("Test warn")))
  #   # toggleModal(session, modalId = 'upload_rules', toggle = "open")
  # })
  
  

  
  # any time the reactive datalist() changes, run the code below which creates
  # a new datalist (if data compliance error) and updates gt outputs if needed
  # observeEvent(datalist(), {
  return_datalist <- eventReactive(datalist(), {
    
    # Run "the check" to see if any rules are violated
    err_tab <- gather_reqs(disp_type = "error",
                           datalist = datalist,
                           all_df_rules = all_df_rules, # = alldf_rules,
                           expl_rules = expl_rules, # = hard_rules,
                           df_incl_rules = df_incl_rules) # = dfWith_rules)
    
    # Check for violations to "warnings" rules
    wrn_tab <- gather_reqs(disp_type = "warn",
                           datalist = datalist,
                           all_df_rules = all_df_rules, # = alldf_rules,
                           expl_rules = expl_rules, # = hard_rules,
                           df_incl_rules = df_incl_rules) # = dfWith_rules)
    
    # Display Modal Conditionally, don't allow escape
    if(nrow(err_tab$df) > 0){
      
      rv$return_dl <- err_tab$df_list
      
      output$err_gt <- render_gt({ err_tab$gt })
      output$wrn_gt <- if(nrow(wrn_tab$df) > 0) render_gt({ wrn_tab$gt })
      
      showModal( modalDialog(
        title = div(style = "text-align:center; font-weight:bold;",
                    "Error: Loaded Data not in Expected Format"),
         footer = 
          if(dismissErrBttn){
            tagList(
              div(style = "text-align:center; font-size: 14px;",
                  html(paste(local_image(filename = "www/red_x.png", height = 15)
                             , "= indicates variable(s) that need attention"))),
              # actionButton(ns("clickRulesError") #actionBttn
              #              , label = NULL
              #              , icon = icon("question-circle")
              # ),
              # actionButton(bs("close_err_mod"), "Dismiss")
              modalButton("Dismiss")
            )
          } else {
            tagList(
              div(style = "text-align:center; font-size: 14px;",
                  html(paste(local_image(filename = "www/red_x.png", height = 15)
                             , "= indicates variable(s) that need attention"))),
              # actionButton(ns("clickRulesError") #actionBttn
              #              , label = NULL
              #              , icon = icon("question-circle")
              # )
              
            )
          },
         # Content of the Modal
         tagList(
            gt_output(ns("err_gt")),
            br(),br(),
            gt_output(ns("wrn_gt")),
            br()
            # ,materialSwitch(ns("clickRulesError")
            #                , label = "Show Upload Rules"
            #                , status = "primary"
            #                , value = F),
            # conditionalPanel("input.clickRulesError", ns = ns, tagList(h5("Error Test"))) #rulesUI)
          )
      ))
    }
    else { # if no errors...
      
      # Display Modal Conditionally, allow escape
      if(nrow(wrn_tab$df) > 0){
        output$wrn_gt <- render_gt({ wrn_tab$gt })
        
        # Only show modal is most recently uploaded data has a warning
        if(names(datalist())[length(names(datalist()))] %in% wrn_tab$df$df) {
          showModal( modalDialog(
            title = div(style = "text-align:center; font-weight:bold;",
                        "Warning: Loaded Data not in Expected Format"),
            footer = tagList(
                div(style = "text-align:center; font-size: 14px;",
                  html(paste(local_image(filename = "www/red_x.png", height = 15)
                             , "= indicates variable(s) that need attention"))
                ),
                # actionButton(ns("clickRulesWarn") #actionBttn
                #              , label = NULL
                #              , icon = icon("question-circle")
                # ),
                
                modalButton("Dismiss")
              ),
           # Content of the modal
           tagList(
             gt_output(ns("wrn_gt")),
             br(),br()
             # ,materialSwitch(ns("clickRulesWarn")
             #                , label = "Show Data Upload Rules"
             #                , status = "primary"
             #                , value = F),
             # conditionalPanel("input.clickRulesWarn", ns = ns, tagList(h5("Warn Test"))) #rulesUI)
             
           )
          ))
        }
      }
    }
    # return_datalist <- reactive({ 
    #   cat(paste("\nlength(names(rv$return_dl)):",length(names(rv$return_dl))))
    #   cat(paste("\nnames(rv$return_dl):",names(rv$return_dl)))
    #   rv$return_dl 
    # })
    return(rv$return_dl)
  })
  
  return(return_datalist())
}






# observeEvent(input$clickRules, {
# 
#   showModal( modalDialog(
#     title = div(style = "text-align:center; font-weight:bold;",
#                 "ADaM-ish Upload Rules"),
#     footer =
#       tagList(
#         h5("*User will be alerted if these ADaM-ish datasets are uploaded and these variables (1) don't exist or (2) are completely empty / missing"),
#         modalButton("Dismiss")
#       ),
#     tagList(
#       # Rules for All Data Sets
#       tagList(
#         if(!is_empty(r$all_err) | !is_empty(r$all_wrn)){
#           tagList(
#             br(),
#             h5("Rules for All Data Sets"),
#             div(style = "font-size: 12px;", tagList(
#               if(!is_empty(r$all_err)){
#                 tagList(
#                   HTML(paste0("&nbsp;&nbsp;&nbsp;Required: ",paste(r$all_err, collapse = ", "))),
#                   br(),br(),
#                 )
#               } else {""},
#               if(!is_empty(r$all_wrn)){
#                 tagList(
#                   HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended: ",paste(r$all_wrn, collapse = ", "))),
#                   br(),br(),
#                 )
#               } else {""}
#             ))
#           )
#         }),
# 
#       # Specific Rules for Specific Data Sets
#       tagList(
#         if(!is_empty(r$expl_err) | !is_empty(r$expl_wrn)){
#           tagList(
#             br(),
#             h5("Specific Rules for Specific Data Sets"),
#             div(style = "font-size: 12px;", tagList(
#               if(!is_empty(r$expl_err)){
#                 tagList(
#                   HTML(paste0("&nbsp;&nbsp;&nbsp;Required:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
#                               paste(r$expl_err, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
#                   br(),br(),
#                 )
#               } else {""},
#               if(!is_empty(r$expl_wrn)){
#                 tagList(
#                   HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
#                               paste(r$expl_wrn, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
#                   br(),br(),
#                 )
#               } else {""}
#             )) # end div
#           )
#         }),
# 
#       # Rules for Data Sets That Contain Certain Variables
#       tagList(
#         if(!is_empty(r$dfw_err) | !is_empty(r$dfw_wrn)){
#           tagList(
#             br(),
#             h5("Rules for Data Sets That Contain Certain Variables"),
#             div(style = "font-size: 12px;", tagList(
#               if(!is_empty(r$dfw_err)){
#                 tagList(
#                   HTML(paste0("&nbsp;&nbsp;&nbsp;Required:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
#                               paste(r$dfw_err, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
#                   br(),br(),
#                 )
#               } else {""},
#               if(!is_empty(r$dfw_wrn)){
#                 tagList(
#                   HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
#                               paste(r$dfw_wrn, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
#                   br(),br(),
#                 )
#               } else {""}
#             )) # end div
#           )
#         })
#       ) # end of tagList UI
#     )) # end of modal
# })









