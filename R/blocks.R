recipe <- HTML('
               <select id="RECIPE">
               <option  id="none">NONE</option>
               <option  id="demography">DEMOGRAPHY</option>
               </select>')

rowBlock <- function(name) {
  apply(name,
        1,
        function(x){
          tags$li(
            class = "block", id = paste(x[1]),
            tippy(paste(x[1]), tooltip = div(paste(x[2]),
                                             style = "max-width:60px;"))
          )
          
          
        }) %>%
    map(., ~ .x)
}

rowPallete <- function(data) {
  map2(names(data),
       data,
       ~div(h5(.x), style="max-height:300px;overflow-y:scroll",
            tags$ul(rowBlock(.y), class = 'all_blocks'))) %>% 
    map(.,
        tagList)
}

rowArea <- function(bins, col) {
  column(col, offset = 0, style='padding:0px;',
         rowPallete(bins)
  )
}

dropArea <- function(name, id, ulid, class, styles, col) {
  column(col, offset = 0, style=styles,
         h4(name),
         id = id,
         tags$ul(
           id = ulid,
           class = class
         ))
}