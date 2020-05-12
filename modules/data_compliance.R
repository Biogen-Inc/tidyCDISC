
######################################################################################
# a module that will interface with the data import module and either (I) display an
# error if needed variables don't exist and stop them from proceeding or (II) warn the
# user if if some columns are missing that are vital for the app to make sense, but 
# they can continue if they wish.
######################################################################################



############################################################################################
# User Interface
############################################################################################

dataComplyUI <- function(id, label = "Check if Data Complies with Rules") {
  
  ns <- NS(id)
  tagList()
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

dataComply <- function(input, output, session, datalist = reactive(NULL), showRules = T, dismissErrBttn = T) {
  
  ns <- session$ns
  rv <- reactiveValues(return_dl = datalist())
  
  # any time the reactive datalist() changes, run the code below which creates
  # a new datalist (if data compliance error) and updates gt outputs if needed
  # observeEvent(datalist(), {
  return_datalist <- eventReactive(datalist(), {
    
    # Run "the check" to see if any rules are violated
    err_tab <- gather_reqs(disp_type = "error",
                           datalist = datalist,
                           all_df_rules = alldf_rules,
                           expl_rules = hard_rules,
                           df_incl_rules = dfWith_rules)
    
    # Check for violations to "warnings" rules
    wrn_tab <- gather_reqs(disp_type = "warn",
                           datalist = datalist,
                           all_df_rules = alldf_rules,
                           expl_rules = hard_rules,
                           df_incl_rules = dfWith_rules)
    
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
              modalButton("Dismiss")
            )
          } else {
            div(style = "text-align:center; font-size: 14px;",
                html(paste(local_image(filename = "www/red_x.png", height = 15)
                           , "= indicates variable(s) that need attention")))
          },
         # Content of the Modal
         tagList(
            gt_output(ns("err_gt")),
            br(),br(),
            gt_output(ns("wrn_gt")),
            br()
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
                modalButton("Dismiss")
              ),
           # Content of the modal
           tagList(
             gt_output(ns("wrn_gt")),
             br(),br()
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
















