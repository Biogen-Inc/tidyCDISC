#' An HTML dropdown for the commonly used tables
#' Add more tables here!
#' @importFrom shiny HTML
recipe <- HTML('
               <select id="RECIPE">
               <option  id="none">NONE</option>
               <option  id="demography">DEMOGRAPHY</option>
               </select>')


#' Create a single draggable block for each column
#' @param name the column name
#' @param classname the dataset of the column
#' 
#' @importFrom tippy tippy
#' @importFrom purrr map
#' @import shiny
#' 
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
rowPallete <- function(data) {
  purrr::map2(names(data), data,
       ~div(class="accordion-container",
            div(class="accordion",
                h6(.x, style="display:inline-block;"),
                tags$button(icon("chevron-down w3-tiny", class="rotate"))),
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
rowArea <- function(bins, col) {
  column(col, offset = 0, style='padding:0px;',
         rowPallete(bins)
  )
}

#' The entire drop area of the blocks,
#' both statistical and columns
#' @param name the name of the drop area
#' @param id the id of the drop area
#' @param class the class of the drop area 
#' so that blocks can be dropped here but not others
#' @param styles the aesthetics to apply
#' @param col the width of the drop area
#' 
#' @import shiny
dropArea <- function(name, id, ulid, class, styles, col) {
  column(col, offset = 0, style=styles,
         h4(name),
         id = id,
         tags$ul(
           id = ulid,
           class = class
         ))
}


#' Create custom S3 classes for each column block
#' based on the dataframe they came from
#' @param x the name of each block
#' @param df the name of the data file
#' which will determine the class
#' 
#' @import shiny
#' @importFrom dplyr case_when
custom_class <- function(x, df) {
  class(x) <- 
    case_when(
      df == "ADSL" ~ c(class(x), "ADSL"),
      df == "ADAE" | df == "ADCM" | df == "ADMH" ~ c(class(x), "OCCDS"), # also admh, conmed
      TRUE ~ c(class(x), "BDS") # contains paramcd
    )
  return(x)
}


#' Using the drag and drop blocks
#' and the shiny inputs,
#' create a dataframe of the columns 
#' and statistics to perform and add custom classes
#' to each of the column names based on their 
#' datafile of origin
#' 
#' @import shiny
#' @importFrom purrr map_chr
#' @importFrom stringr str_trim
#' 
convertTGOutput <- function(agg, blocks) {
  
  agg <- unlist(agg, recursive = FALSE)
  blocks <- unlist(blocks, recursive = FALSE)
  
  # why does it work if I assign it outside the tibble???
  test <- purrr::map_chr(agg, "val", .default = NA_character_) %>% unname()
  
  if (length(agg) > length(blocks)) {
    stop("Need addional variable block")
  } else if (length(agg) < length(blocks)) {
    stop("Need additional statistics block")
  } else {
    
    tibble(
      agg = purrr::map_chr(agg, "txt") %>% unname() %>% str_trim(),
      block = purrr::map_chr(blocks, "txt") %>% unname() %>% str_trim(),
      dataset = purrr::map_chr(blocks, "df") %>% unname() %>% str_trim(),
      # why is this NA in the tibble, but not NA pri
      # dropdown = map_chr(agg, "val", .default = NA_character_) %>% unname()
      dropdown = test,
      S3 = map2(block, dataset, ~ custom_class(.x, .y)),
      gt_group =
        case_when(
          dropdown == "NONE" ~ glue("{agg} of {block}"),
          is.na(dropdown) ~ glue("{agg} of {block}"),
          TRUE ~ glue("{agg} of {block} at {dropdown}")
        )
    )
    
  }
}