#' Find the proper function to apply to 
#' each statistical and column block pairing
#' and use the metadata associated with 
#' each column block for the function's arguments
#' @param agg the statistic to apply given the block name
#' @param column the column to apply that statistic too,
#' and class of the column dictated by the data frame it came from
#' @param week the week if needed for calculation
#' @param group whether to perform a group_by and if so by which column
#' @param data the dataset to perform all functions on
#' 
#' @return the table corresponding to the proper function
#' to perform given the supplied column.
#' This is used within a map to apply to all blocks
#' inside the table generator module.
#' 
#' @family tableGen Functions
#' @export
IDEA_methods <- function(agg, column, week, group, data) {
  if (agg == "MEAN") {
    IDEA_mean(column, week, group, data)
  } else if (agg == "FREQ") {
    IDEA_freq(column, group, data)
  } else if (agg == "ANOVA") {
    IDEA_anova(column, week, group, data)
  } else if (agg == "Y_FREQ"){
    IDEA_y(column, group, data)
  } else if (agg == "NON_MISSING"){
    IDEA_non_missing(column, group, data)
  } else {
    IDEA_chg(column, week, group, data)
  }
}

#' Create custom S3 classes for each column block
#' based on the dataframe they came from
#' @param x the name of each block
#' @param df the name of the data file
#' which will determine the class
#' 
#' @import shiny
#' @importFrom dplyr case_when
#' 
#' @family tableGen Functions
#' 
custom_class <- function(x, df) {
  class(x) <- 
    case_when(
      df == "ADSL" ~ c(class(x), "ADSL"),
      df == "ADAE" ~ c(class(x), "ADAE"),
      df == "ADCM" | df == "ADMH" ~ c(class(x), "OCCDS"), # also admh, conmed
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
#' @param agg the aggregate statistic block 
#' to apply to the column
#' @param blocks the block corresponding 
#' to the column name to apply statistic on
#' 
#' @family tableGen Functions
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
        )#, # will need to feed these datasets through to this function? Or create a new funct?
            # that get's called "blocks_and_functions" that adds the labels?
      # label = purrr::map(blockData$block, function(x) attr(data_to_use(dataset)[[x]], 'label')) %>% 
      #   unname() %>% str_trim()
    )
    
  }
}