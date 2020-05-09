recipe <- HTML('
               <select id="RECIPE">
               <option  id="none">NONE</option>
               <option  id="demography">DEMOGRAPHY</option>
               </select>')



rowBlock <- function(name, classname) {
  apply(name,
        1,
        function(x){
          tags$li(
            class = paste("block ", classname), id = paste(x[1]),
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
            tags$ul(rowBlock(.y,.x), class = 'all_blocks', class = .x))) %>% 
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


custom_class <- function(x, df) {
  class(x) <- 
    case_when(
      df == "ADSL" ~ c(class(x), "ADSL"),
      df == "ADAE" ~ c(class(x), "OCCDS"), # also admh, conmed
      df == "ADVS" ~ c(class(x), "BDS"), # contains paramcd
      TRUE ~ c(class(x), "custom")
    )
  return(x)
}

convertTGOutput <- function(agg, blocks) {
  
  agg <- unlist(agg, recursive = FALSE)
  blocks <- unlist(blocks, recursive = FALSE)
  
  # why does it work if I assign it outside the tibble???
  test <- map_chr(agg, "val", .default = NA_character_) %>% unname()
  
  if (length(agg) > length(blocks)) {
    stop("Need addional variable block")
  } else if (length(agg) < length(blocks)) {
    stop("Need additional statistics block")
  } else {
    
  tibble(
    agg = map_chr(agg, "txt") %>% unname() %>% str_trim(),
    block = map_chr(blocks, "txt") %>% unname() %>% str_trim(),
    dataset = map_chr(blocks, "df") %>% unname() %>% str_trim(),
    # why is this NA in the tibble, but not NA pri
    # dropdown = map_chr(agg, "val", .default = NA_character_) %>% unname()
    dropdown = test,
    S3 = map2(block, dataset, ~ custom_class(.x, .y))
  )
    
  }
}
