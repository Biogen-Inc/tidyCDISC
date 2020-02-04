data_for_blocks <- list("ONE" = c("A", "B", "C"),
                        "TWO" = c("D", "E", "F"),
                        "THREE" = c("G", "H", "I"))


rowBlock <- function(name) {
  tags$li(
    class = "block", id = name,
    div(tippy(div(name), name))
  )
}

# each dataframe should be its own list
# and titled with the name of the df
rowPallete <- function(data) {
  Map(function(x, y) 
    div(h5(x), style="max-height:300px;overflow-y:scroll", 
        tags$ul(class = 'all_blocks', 
                lapply(y, rowBlock))),
    names(data),
    data)
}

rowArea <- function(bins) {
  column(1, offset = 0, style='padding:0px;',
         rowPallete(bins)
  )
}

library(shiny)

ui <- fluidPage(
   rowArea(data_for_blocks)
)

server <- function(input, output) {
}
shinyApp(ui = ui, server = server)

