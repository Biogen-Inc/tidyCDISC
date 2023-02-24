load_recipes <- function(recipe_file) {
  recipes <- jsonlite::read_json(recipe_file)
  
  for (i in seq_along(recipes)) {
    class(recipes[[i]]$blocks) <- c(class(recipes[[i]]$blocks), recipes[[i]]$recipe_inclusion)
    for (j in seq_along(recipes[[i]]$blocks)) {
      class(recipes[[i]]$blocks[[j]]) <- c(class(recipes[[i]]$blocks), recipes[[i]]$blocks[[j]]$stat_options_fn)
    }
  }

  recipes
}

recipe_inclusion <- function(blocks, datalist, ...) {
  UseMethod("recipe_inclusion", blocks)
}

recipe_inclusion.default <- function(blocks, datalist, ...) {
  data_incl <- purrr::map_lgl(blocks, ~ .x$data %in% names(datalist))
  if (!all(data_incl)) 
    return(rep(FALSE, length(blocks)))
  
  param_col_incl <- purrr::map_lgl(blocks, ~ .x$variable %in% datalist[[.x$data]][["PARAMCD"]] || .x$variable %in% names(datalist[[.x$data]]))
  if (!all(param_col_incl)) 
    return(rep(FALSE, length(blocks)))
  
  param_col_incl
}

recipe_inclusion.stan_labs <- function(blocks, datalist, ...) {
  data_incl <- purrr::map_lgl(blocks, ~ "ADLB" %in% substr(names(datalist), 1, 4))
  if (!all(data_incl)) 
    return(rep(FALSE, length(blocks)))
  
  param_incl <- purrr::map_lgl(blocks, ~ .x$variable %in% unlist(purrr::map(datalist["ADLB" == substr(names(datalist), 1, 4)], ~ unique(.x[["PARAMCD"]])), use.names = FALSE))
  
  param_incl
}
