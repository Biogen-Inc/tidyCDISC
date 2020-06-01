PopuExpl7Hbar <- function(input, output, session, df){
  
  ns <- session$ns
  
# Horizonal Bar ("swimmer") plot
widgets <- c("selPrmCode","groupbox","groupbyvar","selxvar","selyvar","hbarOptions")

# show all the widgets using an anonymous function
map(widgets, function(x) shinyjs::show(x))

dfsub <- NULL  # assign dfsub in function environment
makeReactiveBinding("dfsub")

chr <- sort(names(df()[ , which(sapply(df(),is.character))])) # all chr
fac <- sort(names(df()[ , which(sapply(df(),is.factor   ))])) # all factors
num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num

# groupbyvar is loaded with all the character/factor columns
updateSelectInput(session = session, inputId = "groupbyvar", choices = c("",sort(c(chr,fac))), selected = "")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "groupbox", value = TRUE)

updateSelectInput(session = session, inputId = "selxvar", choices = c("",chr),  selected=character(0))
updateSelectInput(session = session, inputId = "selyvar", choices = c("",num),  selected=character(0))

updatePrettyRadioButtons(session = session, inputId = "hbarOptions", selected = "2")

# https://groups.google.com/forum/#!topic/shiny-discuss/4GEJZW0ZDUc
# suggestion from Joe Cheng
# create reactive values for selxvar and selyvar
v <- reactiveValues(selxvar = character(0), selyvar = character(0))

observeEvent(input$selPrmCode, {
  
  req(input$selPrmCode != "") 
  
  # invalidate selxvar and selyvar
  v$selxvar <- character(0)
  v$selyvar <- character(0)
  
  # subset data based on Parameter Code selection
  dfsub <<- filter(df(),PARAMCD == input$selPrmCode) # superassignment operator

  # update select inputs
  chr <- sort(names(df()[ , which(sapply(dfsub,is.character))])) # all chr
  fac <- sort(names(df()[ , which(sapply(dfsub,is.factor   ))])) # all factors
  num <- sort(names(df()[ , which(sapply(dfsub,is.numeric  ))])) # all num
  updateSelectInput(session = session, inputId = "selxvar", choices = c("",chr),  selected=character(0))
  updateSelectInput(session = session, inputId = "selyvar", choices = c("",num),  selected=character(0))
  updateSelectInput(session = session, inputId = "groupbyvar", choices = c("",sort(c(chr,fac))), selected=character(0))

}, ignoreInit = TRUE) # observeEvent(input$selPrmCode

observeEvent(input$selxvar, {
  v$selxvar <- input$selxvar
})
observeEvent(input$selyvar, {
  v$selyvar <- input$selyvar
})

output$PlotlyOut <- renderPlotly({
  
  req(input$selPrmCode != "") 
  req(!is.null(dfsub))
  
  req(!is_empty(v$selxvar) && v$selxvar != "")
  req(!is_empty(v$selyvar) && v$selyvar != "")
  
  xvar <- sym(v$selxvar)
  yvar <- sym(v$selyvar)
  
  labx <- sjlabelled::get_label(dfsub[[v$selxvar]], def.value = unique(v$selxvar))
  laby <- sjlabelled::get_label(dfsub[[v$selyvar]], def.value = unique(v$selyvar))

  # remove missing x-values from plot
  dfsub <- filter(dfsub, !is.na(!!sym(input$selxvar))) 
  
  if(input$groupbox == TRUE) {
    
    req(!is_empty(input$groupbyvar) && input$groupbyvar != "")

    # remove missing groups from plot
    dfsub <- filter(dfsub, !is.na(!!sym(input$groupbyvar))) 
    
    # set def.value to use name if the variable has no label attribute
    labz <- sjlabelled::get_label(dfsub[[input$groupbyvar]], def.value = unique(input$groupbyvar))

    # save PARAm for ggtitle 
    PARAM <- dfsub$PARAM[1]
    
    dfsel <- dfsub %>%
      select(PARAM, !!sym(v$selxvar), !!sym(v$selyvar), !!sym(input$groupbyvar))
    
    switch(input$hbarOptions,
           "1" = {

      # order by x-var          
      # dfsub <- dfsub %>%
      #   mutate(!!sym(v$selxvar) := fct_rev(factor(!!sym(v$selxvar))))
      
      # order by desc frequency count
      dfsel <- dfsel %>%
        mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% fct_infreq() %>% fct_rev() ) 
      
      ggtitle <- paste("Plot of",labx,"versus Percent of Subjects, grouped by",labz,",for",PARAM) 
      
      p <- ggplot(data = dfsel) +
        geom_bar(mapping = aes(x = !!sym(v$selxvar), y = ..prop.., 
                               group = !!sym(input$groupbyvar), fill = fct_rev(!!sym(input$groupbyvar)), 
                               text = paste0(v$selxvar,": ",get(v$selxvar),
                                      "<br>",v$selyvar,": ",get(v$selyvar))), 
                 stat = "count", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) +
        scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1), breaks = seq(0, 1, by=0.1)) +
        scale_x_discrete(drop = FALSE) +
        coord_flip() +
        labs(x = "", y = "Percent of Subjects", fill = labz, title = ggtitle)
 
           },
    "2" = {     

      # calculate mean value per x-var and groupvar
      dfsub <- dfsub %>%
        group_by(PARAM, !!sym(v$selxvar), !!sym(input$groupbyvar)) %>%
        mutate(!!sym(v$selyvar) := round(mean(!!sym(v$selyvar), na.rm = TRUE), digits = 2)) %>%
        distinct(PARAM, !!sym(v$selxvar), !!sym(input$groupbyvar), .keep_all = TRUE) %>%
        ungroup()

      # order by x-var 
      # dfsub <- dfsub %>%
      #   mutate(!!sym(v$selxvar) := fct_rev(factor(!!sym(v$selxvar))))
      
      # order by desc frequency count
      dfsub <- dfsub %>%
        mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% fct_infreq() %>% fct_rev() )
      
      ggtitle <- paste("Plot of",labx,"versus mean",laby,"grouped by",labz,"for",PARAM) 
      
      p <- ggplot(data = dfsub) +
        geom_bar(mapping = aes(x = !!sym(v$selxvar), y = !!sym(v$selyvar), 
                               group = !!sym(input$groupbyvar), fill = fct_rev(!!sym(input$groupbyvar)), 
                               text = paste0(v$selxvar,": ",get(v$selxvar),
                                             "<br>",v$selyvar,": ",get(v$selyvar))), 
                 stat = "identity", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) +
        scale_x_discrete(drop = FALSE) +
        coord_flip() +
        labs(x = "", y = laby, fill = labz, title = ggtitle)

    },
    stop("invalid hbarOptions button: ",input$hbarOptions)
    )
    
  } else {
    
    # save PARAm for ggtitle 
    PARAM <- dfsub$PARAM[1]
    
    dfsel <- dfsub %>%
      select(PARAM, !!sym(v$selxvar), !!sym(v$selyvar))
    
    switch(input$hbarOptions,
           "1" = {
      
      # order by desc frequency count
      dfsub <- dfsub %>%
        mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% fct_infreq() %>% fct_rev() )
      
      ggtitle <- paste("Plot of",labx,"versus Percent of Subjects for",PARAM) 
      
      p <- ggplot(data = dfsub) +
        geom_bar(mapping = aes(x = !!sym(v$selxvar), y = ..prop.., group = 1, 
                               text = paste0(v$selxvar,": ",get(v$selxvar),
                                             "<br>",v$selyvar,": ",get(v$selyvar))), 
                 stat = "count", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) +
        scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1), breaks = seq(0, 1, by=0.1)) +
        scale_x_discrete(drop = FALSE) +
        coord_flip() +
        labs(x = "", y = "Percent of Subjects", title = ggtitle)
      
    },
    "2" = {


      # calculate mean value per x-var 
      dfsub <- dfsub %>%
        group_by(PARAM, !!sym(input$selxvar)) %>%
        mutate(!!sym(input$selyvar) := round(mean(!!sym(input$selyvar), na.rm = TRUE), digits = 2)) %>%
        distinct(PARAM, !!sym(input$selxvar), .keep_all = TRUE) %>%
        ungroup()
      
    # order by desc frequency count
    dfsub <- dfsub %>%
      mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% fct_infreq() %>% fct_rev() )
    
    ggtitle <- paste("Plot of",labx,"versus mean",laby,"for",PARAM) 
    
    p <- ggplot(data = dfsub) +
      geom_bar(mapping = aes(x = !!sym(v$selxvar), y = !!sym(v$selyvar), 
                             text = paste0(v$selxvar,": ",get(v$selxvar),
                                           "<br>",v$selyvar,": ",get(v$selyvar))), 
               stat = "identity", width=0.7, alpha = 0.5, position = "dodge",  na.rm = TRUE) +
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
    dfflt <- filter(dfsub,!is.na(!!sym(input$groupbyvar)))
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
  req(!is_empty(v$selyvar) && v$selyvar != "")
  
  xvar <- sym(v$selxvar)
  yvar <- sym(v$selyvar)
  
  labx <- sjlabelled::get_label(dfsub[[v$selxvar]], def.value = unique(v$selxvar))
  laby <- sjlabelled::get_label(dfsub[[v$selyvar]], def.value = unique(v$selyvar))
  
  # give missing x-values an explicit factor level
  dfsub <- dfsub %>%
    mutate(!!sym(input$selxvar) := fct_explicit_na(!!sym(input$selxvar), na_level = "(Missing)"))
  
  if(input$groupbox == TRUE) {
    
    req(!is_empty(input$groupbyvar) && input$groupbyvar != "")
    
    # set def.value to use name if the variable has no label attribute
    labz <- sjlabelled::get_label(dfsub[[input$groupbyvar]], def.value = unique(input$groupbyvar))
    
    # save PARAm for ggtitle 
    PARAM <- dfsub$PARAM[1]
    
    dfsel <- dfsub %>%
      select(PARAM, !!sym(v$selxvar), !!sym(v$selyvar), !!sym(input$groupbyvar))
    
    switch(input$hbarOptions,
           "1" = {

             
             dfsel <- dfsel %>%
               group_by(PARAM, !!sym(input$groupbyvar), !!sym(v$selxvar) ) %>%
               summarise(count = n()) %>%
               mutate(prop = count/sum(count) ) %>%
               ungroup()
             
             dfsel <- dfsel %>%
               # expand to all x-var, group-var combinations
               expand(!!sym(input$groupbyvar), !!sym(v$selxvar) ) %>%
               left_join(dfsel) 
             
             # order by desc frequency count
             dfsel <- dfsel %>%
               mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% fct_infreq() %>% fct_rev() ) 
             

           },
           "2" = {     
             
             
             dfsel <- dfsel %>%
               group_by(PARAM, !!sym(input$selxvar), !!sym(input$groupbyvar)) %>%
               mutate(!!sym(input$selyvar) := round(mean(!!sym(input$selyvar), na.rm = TRUE), digits = 2)) %>%
               distinct(PARAM, !!sym(input$selxvar), !!sym(input$groupbyvar), .keep_all = TRUE) %>%
               ungroup()
             
             dfsel <- dfsel %>%
               # expand to all x-var, group-var combinations
               expand(!!sym(v$selxvar), !!sym(input$groupbyvar)) %>%
               left_join(dfsel) 
             
             # order by desc frequency count
             dfsel <- dfsel %>%
               mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% fct_infreq() %>% fct_rev() ) 

           },
           stop("invalid hbarOptions button: ",input$hbarOptions)
    )
    
  } else {
    
    # save PARAm for ggtitle 
    PARAM <- dfsub$PARAM[1]
    
    dfsel <- dfsub %>%
      select(PARAM, !!sym(input$selxvar), !!sym(input$selyvar))
    
    switch(input$hbarOptions,
           "1" = {
             
             dfsel <- dfsel %>%
               group_by(PARAM, !!sym(input$selxvar)) %>%
               summarise(count = n()) %>%
               mutate(prop = count/sum(count) ) %>%
               ungroup()
             
             dfsel <- dfsub %>%
               # expand to all x-var combinations
               expand(!!sym(v$selxvar)) %>%
               left_join(dfsel) 
             
             # order by desc frequency count
             dfsel <- dfsel %>%
               mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% fct_infreq() %>% fct_rev() ) 
           },
           "2" = {
             
             dfsel <- dfsel %>%
               group_by(PARAM, !!sym(input$selxvar)) %>%
               mutate(!!sym(input$selyvar) := round(mean(!!sym(input$selyvar), na.rm = TRUE), digits = 2)) %>%
               distinct(PARAM, !!sym(input$selxvar), .keep_all = TRUE) %>%
               ungroup()
             
             dfsel <- dfsub %>%
               # expand to all x-var combinations
               expand(!!sym(v$selxvar)) %>%
               left_join(dfsel) 
             
             # order by desc frequency count
             dfsel <- dfsel %>%
               mutate(!!sym(v$selxvar) := !!sym(v$selxvar) %>% fct_infreq() %>% fct_rev() ) 
             
           },
           stop("invalid hbarOptions button: ",input$hbarOptions)
    )
  }  

  tableout <- dfsel
  
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10))
})

}