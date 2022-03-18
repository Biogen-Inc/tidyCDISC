
#' Create a single draggable block for each column
#' @param name the column name
#' @param classname the dataset of the column
#' 
#' @importFrom tippy tippy
#' @importFrom purrr map
#' @import shiny
#' 
#' @family tableGen Functions
#' @noRd
rowBlock <- function(name, classname) {
  apply(name,
        1,
        function(x){
          tags$li(
            class = paste("block ", classname), id = paste(x[1]),
            tippy(paste(x[1]), tooltip = div(paste(x[2]), style = "max-width:60px;"))
          )
          
          
        }) %>%
    purrr::map(., ~ .x)
}


#' Create an accordion dropdown for each file 
#' @param data the file uploaded
#' 
#' @import shiny
#' @importFrom purrr map
#' @importFrom purrr map2
#' 
#' @family tableGen Functions
#' @noRd
rowPallete <- function(data) {
  purrr::map2(names(data), data,
       ~div(class="accordion-container",
            div(class="accordion",
                h6(.x, style="display:inline-block;"),
                tags$button(icon("chevron-down w3-tiny", verify_fa = FALSE, class="rotate"))),
            div(class="accordion-panel",
                div(
                  tags$ul(rowBlock(.y,.x), class = 'all_blocks', class = .x)
                )
            )
       )
  ) %>% 
    purrr::map(., tagList)
}

#' The entire row area of accordion blocks
#' generated within the table generator server
#' @param bins is the number of accordions to create
#' @param col is the column with to apply
#' @import shiny
#' @family tableGen Functions
#' @noRd
rowArea <- function(bins, col) {
  column(col, offset = 0, style='padding:0px;',
         rowPallete(bins)
  )
}

#' The entire drop area of the blocks,
#' both statistical and columns
#' @param name the name of the drop area
#' @param id the id of the drop area
#' @param ulid the id of the list item 
#' @param class the class of the drop area 
#' so that blocks can be dropped here but not others
#' @param styles the aesthetics to apply
#' @param col the width of the drop area
#' @import shiny
#' @family tableGen Functions
#' @noRd
dropArea <- function(name, id, ulid, class, styles, col) {
  column(col, offset = 0, style=styles,
         h4(name),
         id = id,
         tags$ul(
           id = ulid,
           class = class
         ))
}