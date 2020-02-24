## Only run this example in interactive R sessions
if (interactive()) {
  
  ui <- navbarPage("Navbar page", id = "tabs",
                   tabPanel("Home",
                            actionButton("hideTab", "Hide 'Foo' tab"),
                            actionButton("showTab", "Show 'Foo' tab"),
                            actionButton("hideMenu", "Hide 'More' navbarMenu"),
                            actionButton("showMenu", "Show 'More' navbarMenu")
                   ),
                   tabPanel("Foo", "This is the foo tab"),
                   tabPanel("Bar", "This is the bar tab"),
                   navbarMenu("More",
                              tabPanel("Table", "Table page"),
                              tabPanel("About", "About page"),
                              "------",
                              "Even more!",
                              tabPanel("Email", "Email page")
                   )
  )
  
  server <- function(input, output, session) {
    observeEvent(input$hideTab, {
      hideTab(inputId = "tabs", target = "Foo")
    })
    
    observeEvent(input$showTab, {
      showTab(inputId = "tabs", target = "Foo")
    })
    
    observeEvent(input$hideMenu, {
      hideTab(inputId = "tabs", target = "More")
    })
    
    observeEvent(input$showMenu, {
      showTab(inputId = "tabs", target = "More")
    })
  }
  
  shinyApp(ui, server)
}