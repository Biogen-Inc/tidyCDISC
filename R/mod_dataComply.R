

#' dataComply UI Function
#'
#' @description A shiny Module of a blank UI
#'
#' @param id Internal parameters for {shiny}.
#' 
#' @return An empty shiny tagList
#' 
#' @family dataComply Functions
#' 
mod_dataComply_ui <- function(id){
  # ns <- NS(id)
  tagList(
  )
}





#' dataComply Server Function
#'
#' A module that will interface with a list of data frames and either (I)
#' display an error if needed variables don't exist and or (II) warn the user if
#' some recommended columns don't exist. In the former case, the dataframes that
#' violate the rules (supplied in mod_dataComplyRules_fct_helpers) will not be
#' returned. In the latter case, they will (implying the users will have access
#' to those dataframe(s) in the app).
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param datalist A reactive list of data frames (in this context, from the
#'   mod_dataUpload module)
#' @param dismissErrBttn If \code{TRUE} (the default) then the 'dismiss' button
#'   will appear on the error modal. If \code{FALSE}, the user will not be able
#'   to escape the modal. Instead they will have to reload the app and re-submit
#'   data for upload
#'
#' @import shiny
#' @import dplyr
#'
#' @return A list of dataframes which are compliant with the rules
#'
#' @family dataComply Functions
#'   
mod_dataComply_server <- function(input, output, session,
                                  datalist = reactive(NULL)
                                  , dismissErrBttn = T){
  ns <- session$ns
 
  # Initialize return_dl with the input list of dataframes
  rv <- reactiveValues(return_dl = datalist())
  
  # Any time the reactive datalist() changes, run the code below which creates a
  # new datalist (only including compliant data frames) and updates df & gt outputs
  return_datalist <- eventReactive(datalist(), {
    
    
    # Run "the error check" to see if any required rules were violated
    err_tab <- gather_reqs(disp_type = "error",
                           datalist = datalist,
                           all_df_rules = all_df_rules,
                           expl_rules = expl_rules,
                           df_incl_rules = df_incl_rules,
                           df_incl_rules_except_tte = df_incl_rules_except_tte)
    
    # Run "the warning check" to see if any recommended rules were violated
    wrn_tab <- gather_reqs(disp_type = "warn",
                           datalist = datalist,
                           all_df_rules = all_df_rules,
                           expl_rules = expl_rules,
                           df_incl_rules_except_tte = df_incl_rules_except_tte)
    
    # First, check if any compliance errors. If so, display Modal Conditionally
    # (Note: this modal will show both warnings and Errors if warnings exist)
    if(nrow(err_tab$df) > 0){
      
      # update return_dl with list of compliant dfs
      rv$return_dl <- err_tab$df_list
      
      # render error & warning gt objects for modal
      output$err_gt <- render_gt({ err_tab$gt })
      output$wrn_gt <- if(nrow(wrn_tab$df) > 0) render_gt({ wrn_tab$gt })
      
      # Create modal pop-up UI
      showModal( modalDialog(
        title = div(style = "text-align:center; font-weight:bold;",
                    "Error: Loaded Data not in Expected Format"),
        
        # Show dismiss Button Conditionally based on R developer's wishes in footer
        footer =
          if(dismissErrBttn){
            tagList(
              div(style = "text-align:center; font-size: 14px;",
                  img(src="www/red_x.png", style="height:20px;"), "= indicates variable(s) that need attention"),
              modalButton("Dismiss")
            )
          } else {
            tagList(
              div(style = "text-align:center; font-size: 14px;",
                  img(src="www/red_x.png", style="height:20px;"), "= indicates variable(s) that need attention")
            )
          },
        
        # Content of the Modal Body
        tagList(
          br(),
          gt_output(ns("err_gt")), # gt table of error vars
          br(),br(),
          gt_output(ns("wrn_gt")), # gt table of warning vars
          br()
        )
      ))
    }
    else { # if no errors... take a slightly different approach:
      
      # Check if Warnings exist
      if(nrow(wrn_tab$df) > 0){
        
        # render gt output for modal
        output$wrn_gt <- render_gt({ wrn_tab$gt })
        
        # Only show modal for most recently uploaded data file, avoiding the
        # modal popping up for with every change in datalist() --> we only want
        # it to show for the file where the warning actually exists
        if(names(datalist())[length(names(datalist()))] %in% wrn_tab$df$df) {
          
          # Create modal pop-up UI
          showModal( modalDialog(
            title = div(style = "text-align:center; font-weight:bold;",
                        "Warning: Loaded Data not in Expected Format"),
            footer = tagList(
              div(style = "text-align:center; font-size: 14px;",
                  div(style = "text-align:center; font-size: 14px;",
                      img(src="www/red_x.png", style="height:20px;"), "= indicates variable(s) that need attention")
              ),
              modalButton("Dismiss")
            ),
            
            # Content of the modal body
            tagList(
              br(),
              gt_output(ns("wrn_gt")),
              br(),br()
              
            )
          ))
        }
      }
    } # end else
    return(rv$return_dl) # this get's updated if compliance error exists
  })
  
  return(return_datalist()) # module return
}
    
## To be copied in the UI - Done
# mod_dataComply_ui("dataComply_ui_1")
    
## To be copied in the server - Done
# callModule(mod_dataComply_server, "dataComply_ui_1")
 
