library(shiny)
library(tippy)

# list of dataframes
datalist <- list(data.frame(A = c("col_1", "col_2", "col_3"), B = c("val_1", "val_2", "val_3")),
                 data.frame(X = c("col_4", "col_5", "col_6"), Y = c("val_4", "val_5", "val_6")),
                 data.frame(A = c("col_7", "col_8", "col_9"), B = c("val_7", "val_8", "val_9")))

# named list
names(datalist) <- c("Group 1", "Group 2", "Group 3")


# Create each singluar list element - a col and a val
rowBlock <- function(name, label) {
  tags$li(
    class = "block", id = name,
    div(tippy(div(name), label))
  )
}

# THIS DOES NOT WORK
# combine the rowBlocks
# with an h5 title 
rowPalette <- function(data) {
  Map(function(x, y) 
    div(h5(x), style="max-height:300px;overflow-y:scroll", 
        tags$ul(class = 'all_blocks', 
                lapply(y, rowBlock))),
    names(data),
    data)
}


ui <- 
  tagList(
    rowPalette(datalist),
    div(
      div(h1("Group 1"),
          tags$li(tippy("col_1", "val_1")),
          tags$li(tippy("col_2", "val_2")),
          tags$li(tippy("col_3", "val_3"))),
      
      div(h1("Group 2"),
          tags$li(tippy("col_4", "val_4")),
          tags$li(tippy("col_5", "val_5")),
          tags$li(tippy("col_6", "val_6"))),
      
      div(h1("Group 3"),
          tags$li(tippy("col_7", "val_7")),
          tags$li(tippy("col_8", "val_8")),
          tags$li(tippy("col_9", "val_9")))
    )
  )
  
server <- function(input, output) {
}

shinyApp(ui = ui, server = server)