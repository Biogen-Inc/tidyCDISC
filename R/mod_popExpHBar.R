#' popExpHBar Server Function
#' 
#' Generate a Horizontal Bar plot.
#' Child of parent module mod_popExp.R.
#'
#' @param input,output,session Internal parameters for {shiny}. 
#' @param df dataframe passed to mod_popExpScat.

#'   DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom shinyjs show hide
#' @importFrom plotly ggplotly layout
#' @importFrom shinyWidgets updatePrettyRadioButtons
#' @importFrom forcats fct_rev fct_infreq fct_reorder fct_explicit_na
#' 
#' @noRd
#' 
mod_popExpHBar_server <- function(input, output, session, df){
  ns <- session$ns
 
  # Horizonal Bar ("swimmer") plot
  widgets <- c("selPrmCode","groupbox","groupbyvar","selxvar","hbarOptions","selyvar")
  
  # show all the widgets using an anonymous function
  map(widgets, function(x) shinyjs::show(x))
  
  # dfsub <- NULL  # assign dfsub in function environment
  # makeReactiveBinding("dfsub")
  
  chr <- sort(names(df()[ , which(sapply(df(),is.character))])) # all chr
  fac <- sort(names(df()[ , which(sapply(df(),is.factor   ))])) # all factors
  num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num
  
  # groupbyvar is loaded with all the character/factor columns
  updateSelectInput(session = session, inputId = "groupbyvar", choices = c("",sort(c(chr,fac))), selected = "")
  
  # set checkbox to TRUE
  updateCheckboxInput(session = session, inputId = "groupbox", value = TRUE)
  
  updateSelectInput(session = session, inputId = "selxvar", choices = c("",chr),  selected=character(0))
  updateSelectInput(session = session, inputId = "selyvar", choices = c("",num),  selected=character(0))
  
  updatePrettyRadioButtons(session = session, inputId = "hbarOptions", selected = "1")
  
  # https://groups.google.com/forum/#!topic/shiny-discuss/4GEJZW0ZDUc
  # suggestion from Joe Cheng
  # create reactive values for selxvar and selyvar
  v <- reactiveValues(selxvar = character(0), selyvar = character(0))
  

  dfsub <- reactive({ 
    req(input$selPrmCode != "") 
    filter(df(), PARAMCD == input$selPrmCode) 
  })
  
  observeEvent(input$hbarOptions, {
    if (input$hbarOptions == "1") {
      shinyjs::hide("selyvar")
    } else {
      shinyjs::show("selyvar")
    }
  })
  
  observeEvent(input$selPrmCode, {

    req(input$selPrmCode != "")

    # invalidate selxvar and selyvar
    v$selxvar <- character(0)
    v$selyvar <- character(0)

  }, ignoreInit = TRUE) # observeEvent(input$selPrmCode

  observeEvent(input$selxvar, {
    v$selxvar <- input$selxvar
  })
  observeEvent(input$selyvar, {
    v$selyvar <- input$selyvar
  })

  output$PlotlyOut <- renderPlotly({
    
    req(input$selPrmCode != "") 
    req(!is.null(dfsub()))
    
    req(!is_empty(v$selxvar) && v$selxvar != "")
    
    # remove missing x-values from plot
    dfsel <- filter(dfsub(), !is.na(!!sym(input$selxvar)), !!sym(v$selxvar) != "")
    
    # save PARAm for ggtitle 
    PARAM <- dfsel$PARAM[1]
    
    labx <- sjlabelled::get_label(dfsel[[v$selxvar]], def.value = unique(v$selxvar))
    
    #
    # note that there are four combinations of plots here: input$groupbox {T|F} X input$hbarOptions {percent|Mean}
    #
    
    # groupbox == TRUE
    if(input$groupbox == TRUE) {

      req(!is_empty(input$groupbyvar) && input$groupbyvar != "")
      
      # remove missing z-values (groupby values) from plot
      dfsel <- filter(dfsub(), !is.na(!!sym(input$groupbyvar)), !!sym(input$groupbyvar) != "")

      # set def.value to use name if the variable has no label attribute
      labz <- sjlabelled::get_label(dfsub()[[input$groupbyvar]], def.value = unique(input$groupbyvar))
      
      # save PARAm for ggtitle 
      PARAM <- dfsel$PARAM[1]
      
      switch(input$hbarOptions,
             "1" = {

               ggtitle <- paste("Plot of Percent of Subjects versus",labx, "grouped by",labz,",for",PARAM) 
               
               # order by desc frequency count
               dfsel <- dfsel %>%
                 mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% forcats::fct_infreq() %>% forcats::fct_rev() ) 

               p <- ggplot(data = dfsel) +
                 suppressWarnings(geom_bar(mapping = aes(x = !!sym(v$selxvar), y = ..prop.., 
                                        group = !!sym(input$groupbyvar), fill = forcats::fct_rev(!!sym(input$groupbyvar)), 
                                        text = paste0(v$selxvar,": ",get(v$selxvar))), 
                          stat = "count", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) ) +
                 scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1), breaks = seq(0, 1, by=0.1)) +
                 scale_x_discrete(drop = FALSE) +
                 coord_flip() +
                 labs(x = "", y = "Percent of Subjects", fill = labz, title = ggtitle)
               
             },
             "2" = {     

               req(!is_empty(v$selyvar) && v$selyvar != "")
               laby <- sjlabelled::get_label(dfsel[[v$selyvar]], def.value = unique(v$selyvar))
               
               ggtitle <- paste("Plot of Mean",laby,"versus",labx,"grouped by",labz,"for",PARAM) 
              
               dfsel <- dfsel %>%
                 # expand to all x-var, group-var combinations
                 expand(PARAM, !!sym(input$groupbyvar), !!sym(v$selxvar) ) %>%
                 left_join(dfsel, dfsel, by = c("PARAM", input$groupbyvar, v$selxvar))
               
               # calculate mean value per x-var and groupvar
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(v$selxvar), !!sym(input$groupbyvar)) %>%
                 mutate(!!sym(v$selyvar) := round(mean(!!sym(v$selyvar), na.rm = TRUE), digits = 2)) %>%
                 distinct(PARAM, !!sym(v$selxvar), !!sym(input$groupbyvar), .keep_all = TRUE) %>%
                 ungroup() %>%
                 group_by(PARAM, !!sym(v$selxvar)) %>%
                 mutate(byymean = round(mean(!!sym(v$selyvar), na.rm = TRUE), digits = 2)) %>%
                 ungroup()

               # order by desc overall mean
               dfsel <- dfsel %>%
                 mutate(!!sym(v$selxvar) := fct_reorder(!!sym(v$selxvar), byymean))  
               
               p <- ggplot(data = dfsel) +
                 suppressWarnings(geom_bar(mapping = aes(x = !!sym(v$selxvar), y = !!sym(v$selyvar), 
                                        group = !!sym(input$groupbyvar), fill = forcats::fct_rev(!!sym(input$groupbyvar)), 
                                        text = paste0(v$selxvar,": ",get(v$selxvar),
                                                      "<br>",v$selyvar,": ",get(v$selyvar))), 
                          stat = "identity", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) ) +
                 scale_x_discrete(drop = FALSE) +
                 coord_flip() +
                 labs(x = "", y = laby, fill = labz, title = ggtitle)
               
             },
             stop("invalid hbarOptions button: ",input$hbarOptions)
      )
      
    } else {
      
      switch(input$hbarOptions,
             "1" = {

               ggtitle <- paste("Plot of Percent of Subjects versus",labx,"for",PARAM) 
               
               # order by desc frequency count
               dfsel <- dfsel %>%
                 mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% forcats::fct_infreq() %>% forcats::fct_rev() )

               p <- ggplot(data = dfsel) +
                 suppressWarnings(geom_bar(mapping = aes(x = !!sym(v$selxvar), y = ..prop.., group = 1, 
                                        text = paste0(v$selxvar,": ",get(v$selxvar))), 
                          stat = "count", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) ) +
                 scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1), breaks = seq(0, 1, by=0.1)) +
                 scale_x_discrete(drop = FALSE) +
                 coord_flip() +
                 labs(x = "", y = "Percent of Subjects", title = ggtitle)
               
             },
             "2" = {
               
               req(!is_empty(v$selyvar) && v$selyvar != "")
               laby <- sjlabelled::get_label(dfsel[[v$selyvar]], def.value = unique(v$selyvar))
               
               ggtitle <- paste("Plot of Mean",laby,"versus",labx,"for",PARAM) 
               
               # calculate mean value per x-var 
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(input$selxvar)) %>%
                 mutate(!!sym(v$selyvar) := round(mean(!!sym(v$selyvar), na.rm = TRUE), digits = 2)) %>%
                 distinct(PARAM, !!sym(input$selxvar), .keep_all = TRUE) %>%
                 ungroup()
               
               # order by desc mean
               dfsel <- dfsel %>%
                 mutate(!!sym(v$selxvar) := forcats::fct_reorder(!!sym(v$selxvar), !!sym(v$selyvar))) 
               
               p <- ggplot(data = dfsel) +
                 suppressWarnings(geom_bar(mapping = aes(x = !!sym(v$selxvar), y = !!sym(v$selyvar), 
                                        text = paste0(v$selxvar,": ",get(v$selxvar),
                                                      "<br>",v$selyvar,": ",get(v$selyvar))), 
                          stat = "identity", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) ) +
                 scale_x_discrete(drop = FALSE) +
                 coord_flip() +
                 labs(x = "", y = laby, title = ggtitle)
               
               
             },
             stop("invalid hbarOptions button: ",input$hbarOptions)
      )
    }
    
    p <- p +
      theme_light() 
    
    p1 <-  ggplotly(p, tooltip = "text")  %>%
    # ref https://plot.ly/r/reference/#layout-legend  {x,y} range from -2 to 3
    layout( legend = list(orientation = "h",  xanchor = "center", yanchor = "bottom", x=0.5, y=-0.35))
    
    if(input$groupbox == TRUE) {
      # Now, the workaround:
      # ------------------------------------------------------
      dfflt <- filter(dfsel,!is.na(!!sym(input$groupbyvar)))
      p1Names <- unique(dfflt[[input$groupbyvar]]) # we need to know the "true" legend values
      for (i in 1:length(p1$x$data)) { # this goes over all places where legend values are stored
        n1 <- p1$x$data[[i]]$name # and this is how the value is stored in plotly
        n2 <- " "
        for (j in 1:length(p1Names)) {
          if (grepl(x = n1, pattern = p1Names[j])) {n2 = p1Names[j]} # if the plotly legend name contains the original value, replace it with the original value
        }
        p1$x$data[[i]]$name <- n2 # now is the time for actual replacement
        if (n2 == " ") {p1$x$data[[i]]$showlegend = FALSE}  # sometimes plotly adds to the legend values that we don't want, this is how to get rid of them, too
      }
    }

    return(p1)
    
  })
  
  output$DataTable <- DT::renderDataTable({
    
    req(input$selPrmCode != "") 
    req(!is.null(dfsub))
    
    req(!is_empty(v$selxvar) && v$selxvar != "")
    
    # give missing x-values an explicit factor level
    dfsel <- dfsub() %>%
      mutate(!!sym(v$selxvar) := forcats::fct_explicit_na(!!sym(v$selxvar), na_level = "(Missing)"))
    
    dfsel <- dfsub()
    
    if(input$groupbox == TRUE) {
      
      req(!is_empty(input$groupbyvar) && input$groupbyvar != "")
      
      switch(input$hbarOptions,
             "1" = {
               # select variables
               dfsel <- dfsel %>%
                 select(PARAM, !!sym(v$selxvar), !!sym(input$groupbyvar))
               
               # expand to all x-var, group-var combinations
               dfsel <- dfsel %>%
                 # expand to all x-var, group-var combinations
                 expand(PARAM, !!sym(input$groupbyvar), !!sym(v$selxvar) ) %>%
                 left_join(dfsel, dfsel, by = c("PARAM", input$groupbyvar, v$selxvar))
               
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(input$groupbyvar), !!sym(v$selxvar) ) %>%
                 summarise(count = n()) %>%
                 mutate(prop = count/sum(count) ) %>%
                 ungroup() %>%
                 arrange(desc(count), !!sym(input$groupbyvar))

             },
             "2" = {     
               req(!is_empty(v$selyvar) && v$selyvar != "")
               
               # select variables
               dfsel <- dfsel %>%
                 select(PARAM, !!sym(v$selxvar), !!sym(v$selyvar), !!sym(input$groupbyvar))
               
               # expand to all x-var, group-var combinations
               dfsel <- dfsel %>%
                 # expand to all x-var, group-var combinations
                 expand(PARAM, !!sym(input$groupbyvar), !!sym(v$selxvar) ) %>%
                 left_join(dfsel, dfsel, by = c("PARAM", input$groupbyvar, v$selxvar))
               
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(v$selxvar), !!sym(input$groupbyvar)) %>%
                 mutate(!!sym(v$selyvar) := round(mean(!!sym(v$selyvar), na.rm = TRUE), digits = 2)) %>%
                 distinct(PARAM, !!sym(v$selxvar), !!sym(input$groupbyvar), .keep_all = TRUE) %>%
                 ungroup() %>%
                 group_by(PARAM, !!sym(v$selxvar)) %>%
                 mutate(byymean = round(mean(!!sym(v$selyvar), na.rm = TRUE), digits = 2)) %>%
                 ungroup() %>%
                 arrange(desc(byymean),!!sym(input$groupbyvar))

             },
             stop("invalid hbarOptions button: ",input$hbarOptions)
      )
      
    } else {
      
      switch(input$hbarOptions,
             "1" = {
               
               # select variables
               dfsel <- dfsel %>%
                 select(PARAM, !!sym(v$selxvar))
               
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(v$selxvar)) %>%
                 summarise(count = n()) %>%
                 mutate(prop = count/sum(count) ) %>%
                 ungroup() %>%
                 arrange(desc(count))
               
             },
             "2" = {
               
               req(!is_empty(v$selyvar) && v$selyvar != "")
               
               # select variables
               dfsel <- dfsel %>%
                 select(PARAM, !!sym(v$selxvar), !!sym(v$selyvar))
               
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(v$selxvar)) %>%
                 mutate(!!sym(v$selyvar) := round(mean(!!sym(v$selyvar), na.rm = TRUE), digits = 2)) %>%
                 distinct(PARAM, !!sym(v$selxvar), .keep_all = TRUE) %>%
                 ungroup() %>%
                 arrange(desc(!!sym(v$selyvar)))

             },
             stop("invalid hbarOptions button: ",input$hbarOptions)
      )
    }  
    

    tableout <- dfsel
    
    DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10))
  })
  
}
    
## To be copied in the UI -- erased
# mod_popExpHBar_ui("popExpHBar_ui_1")
    
## To be copied in the server -- done
# callModule(mod_popExpHBar_server, "popExpHBar_ui_1")
 
