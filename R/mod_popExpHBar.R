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
  
  # set checkbox to TRUE
  updateCheckboxInput(session = session, inputId = "groupbox", value = TRUE)
  
  updatePrettyRadioButtons(session = session, inputId = "hbarOptions", selected = "1")
  
  # https://groups.google.com/forum/#!topic/shiny-discuss/4GEJZW0ZDUc
  # suggestion from Joe Cheng
  # create reactive values for selxvar and selyvar
  rv <- reactiveValues(selxvar = character(0), selyvar = character(0))
  
  dfsub <- reactive({ 
    req(input$selPrmCode != "") 
    filter(df(), PARAMCD == input$selPrmCode) %>%
      # remove all NA columns
      mutate_if(is.character, ~replace(., . == "", NA)) %>%
      base::Filter(function(x) !all(is.na(x)), .)
  })

observeEvent(input$hbarOptions, {
    if (input$hbarOptions == "1") {
      shinyjs::hide("selyvar")
    } else {
      shinyjs::show("selyvar")
    }
  })
  
  observeEvent(input$selPrmCode, {

    req(!is.null(dfsub()))
    
    print("observeEvent for selPrmCode")
    
    chr <- sort(names(dfsub()[ , which(sapply(dfsub(),is.character))])) # all chr
    fac <- sort(names(dfsub()[ , which(sapply(dfsub(),is.factor   ))])) # all factors
    num <- sort(names(dfsub()[ , which(sapply(dfsub(),is.numeric  ))])) # all num
    
    # groupbyvar is loaded with all the character/factor columns
    updateSelectInput(session = session, inputId = "groupbyvar", choices = c("",sort(c(chr,fac))), selected = "")
    
    updateSelectInput(session = session, inputId = "selxvar", choices = c("",sort(c(chr,fac))), selected = "")
    
    updateSelectInput(session = session, inputId = "selyvar", choices = c("",num),  selected = "")

    # invalidate selxvar and selyvar 
    rv$selxvar <- character(0)
    rv$selyvar <- character(0)
    
  }, ignoreInit = TRUE) # observeEvent(input$selPrmCode

  observeEvent(input$selxvar, {
    print(paste("in observeEvent for selxvar:",input$selxvar))
    rv$selxvar <- input$selxvar
  })
  observeEvent(input$selyvar, {
    print(paste("in observeEvent for selyvar:",input$selyvar))
    rv$selyvar <- input$selyvar
  })

  output$PlotlyOut <- renderPlotly({
    
    req(!is.null(dfsub()))
    
    print("in renderPlotly")
    req(!is_empty(rv$selxvar) && rv$selxvar != "")
    print(paste("selxvar is",rv$selxvar))
    
    # remove missing x-values from plot
    dfsel <- filter(dfsub(), !is.na(!!sym(rv$selxvar)), !!sym(rv$selxvar) != "")
    
    # save PARAm for ggtitle 
    PARAM <- dfsel$PARAM[1]
    
    labx <- sjlabelled::get_label(dfsel[[rv$selxvar]], def.value = unique(rv$selxvar))
    
    #
    # note that there are four combinations of plots here: input$groupbox {T|F} X input$hbarOptions {percent|Mean}
    #
    
    # groupbox == TRUE
    if(input$groupbox == TRUE) {

      req(!is_empty(input$groupbyvar) && input$groupbyvar != "")
      
      # remove missing z-values (groupby values) from plot
      dfsel <- filter(dfsel, !is.na(!!sym(input$groupbyvar)), !!sym(input$groupbyvar) != "", !!sym(input$groupbyvar) != "Missing")

      # set def.value to use name if the variable has no label attribute
      labz <- sjlabelled::get_label(dfsub()[[input$groupbyvar]], def.value = unique(input$groupbyvar))
      
      # save PARAm for ggtitle 
      PARAM <- dfsel$PARAM[1]
      
      switch(input$hbarOptions,
             "1" = {

               ggtitle <- paste("Plot of Percent of Subjects versus",labx, "grouped by",labz,",for",PARAM) 
               
               # order by desc frequency count
               dfsel <- dfsel %>%
                 mutate(!!sym(rv$selxvar) := !!sym(rv$selxvar) %>% forcats::fct_infreq() %>% forcats::fct_rev() ) 

               p <- ggplot(data = dfsel) +
                 suppressWarnings(geom_bar(mapping = aes(x = !!sym(rv$selxvar), y = ..prop.., 
                                        group = !!sym(input$groupbyvar), fill = get(input$groupbyvar),  
                                        text = paste0(rv$selxvar,": ",get(rv$selxvar))), 
                          stat = "count", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) ) +
                 scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1), breaks = seq(0, 1, by=0.1)) +
                 scale_x_discrete(drop = FALSE) +
                 coord_flip() +
                 labs(x = "", y = "Percent of Subjects", fill = labz, title = ggtitle)
               
             },
             "2" = {     

               req(!is_empty(rv$selyvar) && rv$selyvar != "")
               print(paste("selxvar is",rv$selxvar))
               
               labx <- sjlabelled::get_label(dfsel[[rv$selxvar]], def.value = unique(rv$selxvar))
               laby <- sjlabelled::get_label(dfsel[[rv$selyvar]], def.value = unique(rv$selyvar))
               
               ggtitle <- paste("Plot of Mean",laby,"versus",labx,"grouped by",labz,"for",PARAM) 
              
               # calculate mean value per x-var and groupvar
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(rv$selxvar), !!sym(input$groupbyvar)) %>%
                 mutate(!!sym(rv$selyvar) := round(mean(!!sym(rv$selyvar), na.rm = TRUE), digits = 2)) %>%
                 distinct(PARAM, !!sym(rv$selxvar), !!sym(input$groupbyvar), .keep_all = TRUE) %>%
                 ungroup() %>%
                 group_by(PARAM, !!sym(rv$selxvar)) %>%
                 mutate(byymean = round(mean(!!sym(rv$selyvar), na.rm = TRUE), digits = 2)) %>%
                 ungroup()

               # order by desc overall mean
               dfsel <- dfsel %>%
                 mutate(!!sym(rv$selxvar) := fct_reorder(!!sym(rv$selxvar), byymean))  
               
               p <- ggplot(data = dfsel) +
                 suppressWarnings(geom_bar(mapping = aes(x = !!sym(rv$selxvar), y = !!sym(rv$selyvar), 
                                        group = !!sym(input$groupbyvar), fill = get(input$groupbyvar), 
                                        text = paste0(rv$selxvar,": ",get(rv$selxvar),
                                                      "<br>",rv$selyvar,": ",get(rv$selyvar))), 
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
                 mutate(!!sym(rv$selxvar) := !!sym(rv$selxvar) %>% forcats::fct_infreq() %>% forcats::fct_rev() )

               p <- ggplot(data = dfsel) +
                 suppressWarnings(geom_bar(mapping = aes(x = !!sym(rv$selxvar), y = ..prop.., group = 1, 
                                        text = paste0(rv$selxvar,": ",get(rv$selxvar))), 
                          stat = "count", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) ) +
                 scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1), breaks = seq(0, 1, by=0.1)) +
                 scale_x_discrete(drop = FALSE) +
                 coord_flip() +
                 labs(x = "", y = "Percent of Subjects", title = ggtitle)
               
             },
             "2" = {
               
               req(!is_empty(rv$selyvar) && rv$selyvar != "")
               labx <- sjlabelled::get_label(dfsel[[rv$selxvar]], def.value = unique(rv$selxvar))
               laby <- sjlabelled::get_label(dfsel[[rv$selyvar]], def.value = unique(rv$selyvar))
               
               ggtitle <- paste("Plot of Mean",laby,"versus",labx,"for",PARAM) 
               
               # calculate mean value per x-var 
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(rv$selxvar)) %>%
                 mutate(!!sym(rv$selyvar) := round(mean(!!sym(rv$selyvar), na.rm = TRUE), digits = 2)) %>%
                 distinct(PARAM, !!sym(rv$selxvar), .keep_all = TRUE) %>%
                 ungroup()
               
               # order by desc mean
               dfsel <- dfsel %>%
                 mutate(!!sym(rv$selxvar) := forcats::fct_reorder(!!sym(rv$selxvar), !!sym(rv$selyvar))) 
               
               p <- ggplot(data = dfsel) +
                 suppressWarnings(geom_bar(mapping = aes(x = !!sym(rv$selxvar), y = !!sym(rv$selyvar), 
                                        text = paste0(rv$selxvar,": ",get(rv$selxvar),
                                                      "<br>",rv$selyvar,": ",get(rv$selyvar))), 
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
    
    req(!is.null(dfsub()))
    
    req(!is_empty(rv$selxvar) && rv$selxvar != "")

    # give missing x-values an explicit factor level
    dfsel <- dfsub() %>%
      mutate(!!sym(rv$selxvar) := forcats::fct_explicit_na(!!sym(rv$selxvar), na_level = "Missing"))
    
    dftbl <- reactive({
      
    if(input$groupbox == TRUE) {
      
      req(!is_empty(input$groupbyvar) && input$groupbyvar != "")
      
      switch(input$hbarOptions,
             "1" = {
               # select variables
               dfsel <- dfsel %>%
                 select(PARAM, !!sym(rv$selxvar), !!sym(input$groupbyvar))
               
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(input$groupbyvar), !!sym(rv$selxvar) ) %>%
                 summarise(count = n()) %>%
                 mutate(prop = count/sum(count) ) %>%
                 ungroup() %>%
                 arrange(desc(count), !!sym(input$groupbyvar))

             },
             "2" = {     
               req(!is_empty(rv$selyvar) && rv$selyvar != "")
               
               # select variables
               dfsel <- dfsel %>%
                 select(PARAM, !!sym(rv$selxvar), !!sym(rv$selyvar), !!sym(input$groupbyvar))
               
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(rv$selxvar), !!sym(input$groupbyvar)) %>%
                 mutate(!!sym(rv$selyvar) := round(mean(!!sym(rv$selyvar), na.rm = TRUE), digits = 2)) %>%
                 distinct(PARAM, !!sym(rv$selxvar), !!sym(input$groupbyvar), .keep_all = TRUE) %>%
                 ungroup() %>%
                 group_by(PARAM, !!sym(rv$selxvar)) %>%
                 mutate(byymean = round(mean(!!sym(rv$selyvar), na.rm = TRUE), digits = 2)) %>%
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
                 select(PARAM, !!sym(rv$selxvar))
               
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(rv$selxvar)) %>%
                 summarise(count = n()) %>%
                 mutate(prop = count/sum(count) ) %>%
                 ungroup() %>%
                 arrange(desc(count))
               
             },
             "2" = {
               
               req(!is_empty(rv$selyvar) && rv$selyvar != "")
               
               # select variables
               dfsel <- dfsel %>%
                 select(PARAM, !!sym(rv$selxvar), !!sym(rv$selyvar))
               
               dfsel <- dfsel %>%
                 group_by(PARAM, !!sym(rv$selxvar)) %>%
                 mutate(!!sym(rv$selyvar) := round(mean(!!sym(rv$selyvar), na.rm = TRUE), digits = 2)) %>%
                 distinct(PARAM, !!sym(rv$selxvar), .keep_all = TRUE) %>%
                 ungroup() %>%
                 arrange(desc(!!sym(rv$selyvar)))

             },
             stop("invalid hbarOptions button: ",input$hbarOptions)
      )
    }  
    
    })
    
    DT::datatable(dftbl(), options = list(dom = 'ftp', pageLength = 15, lengthMenu = c(5, 10, 15, 20)))
  })
  
}
    
## To be copied in the UI -- erased
# mod_popExpHBar_ui("popExpHBar_ui_1")
    
## To be copied in the server -- done
# callModule(mod_popExpHBar_server, "popExpHBar_ui_1")
 
