#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#' @param recipes_json Path to JSON file with metadata 
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' 
#' @return No return value, called to run the application.
run_app <- function(
    ...,
    recipes_json = NULL
) {
  if (is.null(recipes_json)) recipes_json <- app_sys("recipes.json")
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(recipes_json = recipes_json, ...)
  )
}
