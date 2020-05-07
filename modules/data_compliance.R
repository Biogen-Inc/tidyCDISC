
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

dataComply <- function(input, output, session, datalist = reactive(NULL)) {
  
  ns <- session$ns

  # any time the reactive datalist() changes, run this code
  observeEvent(datalist(), {
    
    # Run "the check" to see if any rules are violated
    err_tab <- gather_reqs(disp_type = "error",
                           datalist = datalist,
                           expl_rules = hard_rules,
                           df_incl_rules = dfWith_rules)
    
    # Display Modal Conditionally, don't allow escape
    if(nrow(err_tab$df) > 0){
      output$err_gt <- render_gt({ err_tab$gt })
      showModal( modalDialog(footer = NULL, title = "ERROR!",
        tagList(
            gt_output(ns("err_gt")),
            br(),br()
          )
      ))
    }
    else { # if no errors...
      
      # Check for violations to "warnings" rules
      wrn_tab <- gather_reqs(disp_type = "warn",
                             datalist = datalist,
                             expl_rules = hard_rules,
                             df_incl_rules = dfWith_rules)
      
      # Display Modal Conditionally, allow escape
      if(nrow(wrn_tab$df) > 0){
        output$wrn_gt <- render_gt({ wrn_tab$gt })
        
        # Only show modal is most recently uploaded data has a warning
        if(names(datalist())[length(names(datalist()))] %in% wrn_tab$df$df) {
          showModal( modalDialog(title = "WARNING!",
             tagList(
               gt_output(ns("wrn_gt")),
               br(),br()
             )
          ))
        }
      }
    }
  })
}
















