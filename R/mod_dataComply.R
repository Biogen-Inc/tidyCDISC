######################################################################################
# Module description:
# a module that will interface with a list of data frames and either (I) display an
# error if needed variables don't exist and stop them from proceeding or (II) warn the
# user if if some columns are missing that are vital for the app to make sense, but 
# they can continue if they wish.
######################################################################################



############################################################################################
# User Interface
############################################################################################

#' dataComply UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyBS bsModal
mod_dataComply_ui <- function(id, label = "Check if Data Complies with Rules", showRules = T){
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
# Module Arguments:
# datalist() is a list of one or more data frames
# dismissErrBttn == TRUE will produce a Dismiss button on Error modals, FALSE will not 
#                        which will essentially trap the user on the modal
# rules (not an argument) below are defined above
############################################################################################

#' dataComply Server Function
#'
#' @noRd 
mod_dataComply_server <- function(input, output, session, datalist = reactive(NULL), dismissErrBttn = T){
  ns <- session$ns
 
  rv <- reactiveValues(return_dl = datalist())
  
  # **Keeping this code in here in case we ever want to revisit channeling 
  # from the error/warning to the help modal. Channeling is only supported
  # by dean attali's "shinyalert" package. Right now, it's difficult for 
  # the server to identify anything that's going on and I think that's because
  # the current location of the callModule() is inside an observeEvent(input$file, {})
  
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
              # **
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
              # **
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
          # **
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
              # **
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
              # **
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
    return(rv$return_dl) # eventReactive return
  })
  
  return(return_datalist()) # module return
}
    
## To be copied in the UI - Done
# mod_dataComply_ui("dataComply_ui_1")
    
## To be copied in the server - Done
# callModule(mod_dataComply_server, "dataComply_ui_1")
 
