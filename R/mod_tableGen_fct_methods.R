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
#' @param totals the totals data frame that contains denominator N's use when
#'   calculating column percentages
#' 
#' @return the table corresponding to the proper function
#' to perform given the supplied column.
#' This is used within a map to apply to all blocks
#' inside the table generator module.
#' 
#' @family tableGen Functions
#' @export
#' @keywords tabGen_repro
#' 
#' @examples 
#' data(example_dat1, package = "tidyCDISC")
#' 
#' # Create non-missing table section
#' app_methods("NON_MISSING", 
#'             structure("USUBJID", class = c("character", "ADSL")), NA, 
#'             "TRT01P", example_dat1$AE, example_dat1$totals)
#'             
#' # Create ANOVA table section
#' app_methods("ANOVA", 
#'             structure("TEMP", class = c("character", "BDS")), "Week 2", 
#'             "TRT01P", example_dat1$BDS, example_dat1$totals)
#' 
#' # Create change table section
#' app_methods("CHG", 
#'             structure("WEIGHT", class = c("character", "BDS")), "Week 12", 
#'             "TRT01P", example_dat1$BDS, example_dat1$totals)
#' 
#' # Create mean table section
#' app_methods("MEAN", 
#'             structure("PULSE", class = c("character", "BDS")), "Baseline", 
#'             "TRT01P", example_dat1$BDS, example_dat1$totals)
app_methods <- function(agg, column, week, group, data, totals) {
  # informative error in case the selected variable doesn't exist in data
  # if no data in the source, do not run the pmap, just show this msg:
  if(nrow(data) == 0){
    stop("No subjects remain in data source.")
  }
  if (!(paste(column) %in% colnames(data)) &
      !(paste(column) %in% unique(data$PARAMCD))
      ){
    stop(glue::glue("{column} variable doesn't exist in data, please remove or replace that variable from drop zone."))
  }
  
  
  if (agg == "MEAN") {
    app_mean(column, week, group, data)
  } else if (agg == "FREQ") {
    app_freq(column, group, data, totals)
  } else if (agg == "ANOVA") {
    app_anova(column, week, group, data)
  } else if (agg == "NESTED_FREQ_DSC"){
    app_nested_freq(column, week, group, data, totals, sort = "desc_tot")
  } else if (agg == "NESTED_FREQ_ABC"){
    app_nested_freq(column, week, group, data, totals, sort = "alpha")
  } else if (agg == "Y_FREQ"){
    app_y(column, group, data, totals)
  } else if (agg == "MAX_FREQ"){
    app_max_freq(column, group, data, totals)
  } else if (agg == "NON_MISSING"){
    app_non_missing(column, group, data, totals)
  } else {
    app_chg(column, week, group, data)
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
#' @noRd
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
#' @param aggs the aggregate statistic block 
#' to apply to the column
#' @param blocks the block corresponding 
#' to the column name to apply statistic on
#' 
#' @family tableGen Functions
#' @noRd
#' 
convertTGOutput <- function(aggs, blocks) {
  
  aggs <- unlist(aggs, recursive = FALSE)
  blocks <- unlist(blocks, recursive = FALSE)

  if (length(aggs) > length(blocks)) {
    stop("Need addional variable block")
  } else if (length(aggs) < length(blocks)) {
    stop("Need additional statistics block")
  } else {
    
    purrr::map2_df(aggs, blocks, function(aggs, blocks) {
      if (!is.null(aggs$val) && aggs$val == "ALL") {
        purrr::map_df(aggs$lst, function(dropdown) {
          tidyr::tibble(
            agg = aggs$txt %>% unname() %>% str_trim(),
            block = blocks$txt %>% unname() %>% str_trim(),
            dataset = blocks$df %>% unname() %>% str_trim(),
            dropdown = dropdown %>% unname() %>% str_trim(),
            S3 = map2(block, dataset, ~ custom_class(.x, .y)),
            gt_group =
              case_when(
                dropdown == "NONE" ~ glue("{agg} of {block}"),
                is.na(dropdown) ~ glue("{agg} of {block}"),
                tolower(substr(dropdown, 1, 4)) %in% c("week","base","scree","end ") ~ glue("{agg} of {block} at {dropdown}"),
                TRUE ~ glue("{agg} of {block} and {dropdown}") # "and" instead of "at"
              )
          )
        })
      } else {
        tidyr::tibble(
          agg = aggs$txt %>% unname() %>% str_trim(),
          block = blocks$txt %>% unname() %>% str_trim(),
          dataset = blocks$df %>% unname() %>% str_trim(),
          dropdown = ifelse(is.null(aggs$val), NA_character_, aggs$val %>% unname() %>% str_trim()),
          S3 = map2(block, dataset, ~ custom_class(.x, .y)),
          gt_group =
            case_when(
              dropdown == "NONE" ~ glue("{agg} of {block}"),
              is.na(dropdown) ~ glue("{agg} of {block}"),
              tolower(substr(dropdown, 1, 4)) %in% c("week","base","scree","end ") ~ glue("{agg} of {block} at {dropdown}"),
              TRUE ~ glue("{agg} of {block} and {dropdown}") # "and" instead of "at"
            )
        )
      }


    })
    
  }
}
