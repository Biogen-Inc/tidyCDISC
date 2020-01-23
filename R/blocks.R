recipe <- HTML('<select id="RECIPE"><option id="none">NONE</option><option id="mean">MEAN</option></select>')

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
    div(h5(x), 
        tags$ul(
          style='padding:0px;padding-right: 5px;max-height: 250px;overflow-y:scroll;text-align:center;"',
          class = 'all_blocks', lapply(colnames(y), rowBlock))),
    names(data),
    data)
}

rowArea <- function(bins) {
  column(1, offset = 0, style='padding:0px;',
         rowPallete(bins)
  )
}

dropArea <- function(name, id, ulid, class, styles) {
  column(5, offset = 0, style=styles,
         h4(name),
         id = id,
         tags$ul(
           id = ulid,
           class = class
         ))
}