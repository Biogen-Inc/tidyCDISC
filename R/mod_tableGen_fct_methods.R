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
#' @param filter a string denoting the additional filter to apply to the dataset
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
#' if(interactive()){
#'   data(example_dat1, package = "tidyCDISC")
#' 
#'   # Create non-missing table section
#'   app_methods("NON_MISSING", 
#'              structure("USUBJID", class = c("character", "ADSL")), NA, 
#'              "TRT01P", example_dat1$AE, example_dat1$totals)
#'             
#'   # Create ANOVA table section
#'   app_methods("ANOVA", 
#'             structure("TEMP", class = c("character", "BDS")), "Week 2", 
#'             "TRT01P", example_dat1$BDS, example_dat1$totals)
#' 
#'   # Create change table section
#'   app_methods("CHG", 
#'             structure("WEIGHT", class = c("character", "BDS")), "Week 12", 
#'             "TRT01P", example_dat1$BDS, example_dat1$totals)
#' 
#'   # Create mean table section
#'   app_methods("MEAN", 
#'             structure("PULSE", class = c("character", "BDS")), "Baseline", 
#'             "TRT01P", example_dat1$BDS, example_dat1$totals)
#' }
app_methods <- function(agg, column, week, group, data, totals, filter = NA) {
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
  
  if (!is.na(filter)) {
    data <- dplyr::filter(data, !!rlang::parse_expr(filter))
    
    if (week != "NONE" && nrow(dplyr::filter(data, AVISIT == week, PARAMCD == column)) == 0) {
      cat("\033[0;31mBlock output suppressed for `PARAMCD == '", column, "' & ", filter, " & AVISIT == '", week, "'` because dataset was empty.\033[0m\n", sep = "")
      return(NULL)
    }
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

#' Process the drag and drops blocks
#' 
#' @param aggs the aggregate statistic block 
#' to apply to the column
#' @param blocks the block corresponding 
#' to the column name to apply statistic on
#' 
#' @family tableGen Functions
#' @noRd
#' @importFrom purrr map
process_droppables <- function(aggs, blocks) {
  
  aggs <- unlist(aggs, recursive = FALSE)
  blocks <- unlist(blocks, recursive = FALSE)
  process_dropdown <- function(droppable) {
    droppable$dropdown <-
      if (is.null(droppable$val)) {
        NA_character_
      } else if (droppable$val == "ALL") {
        droppable$lst
      } else {
        droppable$val
      } %>%
      unname() %>% str_trim()
    if (is.null(droppable$grp))
        droppable$grp <- NA_character_
    droppable
  }
  
  if (length(aggs) == 0 && length(blocks) == 0) {
    return(list(aggs = aggs, blocks = blocks))
  } else if (length(aggs) > length(blocks)) {
    stop("Need addional variable block")
  } else if (length(aggs) < length(blocks)) {
    stop("Need additional statistics block")
  } else {
    aggs <- purrr::map(aggs, process_dropdown)
    blocks <- purrr::map(blocks, process_dropdown)
    aggs_out <- blocks_out <- list()
    for (i in seq_along(aggs)) {
      for (j in seq_along(aggs[[i]]$dropdown)) {
        for (k in seq_along(blocks[[i]]$dropdown)) {
          len <- length(aggs_out) + 1
          aggs_out[[len]] <- aggs[[i]]
          aggs_out[[len]]$val <- aggs[[i]]$dropdown[[j]]
          aggs_out[[len]]$lst <- NULL
          aggs_out[[len]]$dropdown <- NULL
          blocks_out[[len]] <- blocks[[i]]
          blocks_out[[len]]$val <- blocks[[i]]$dropdown[[k]]
          blocks_out[[len]]$lst <- NULL
          blocks_out[[len]]$dropdown <- NULL
        }
      }
    }
    return(list(aggs = aggs_out, blocks = blocks_out))
  }
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
  
  if (length(aggs) == 0 & length(blocks) == 0) {
    tidyr::tibble(
      agg = character(),
      block = character(),
      dataset = character(),
      dropdown = character(),
      filter = character(),
      S3 = character(),
      gt_group = character()
    )
  } else {
    purrr::map2_df(aggs, blocks, function(aggs, blocks) {
      tidyr::tibble(
        agg = aggs$txt %>% unname() %>% str_trim(),
        block = blocks$txt %>% unname() %>% str_trim(),
        dataset = blocks$df %>% unname() %>% str_trim(),
        dropdown = aggs$val,
        filter = if (is.na(blocks$grp)) {NA_character_} 
        else if (blocks$val == "N/A") {glue::glue("is.na({blocks$grp %>% unname() %>% str_trim()})")} 
        else {glue::glue("{blocks$grp %>% unname() %>% str_trim()} == '{blocks$val}'")},
        S3 = map2(block, dataset, ~ custom_class(.x, .y)),
        gt_group = glue("{agg} of {block}{if (is.na(dropdown) || dropdown == 'NONE') '' else if (tolower(substr(dropdown, 1, 4)) %in% c('week','base','scree','end ')) paste(' at', dropdown) else paste(' and', dropdown)}{if (is.na(blocks$grp) || blocks$val == 'N/A' || blocks$val == dropdown) '' else paste('/', blocks$val)}")
      )
    })
  }
}

create_avisit <- function(datalist, bds_data) {
  if (!any(purrr::map_lgl(datalist, ~"AVISIT" %in% colnames(.x))))
    stop("The column AVISIT must be present in the data list")

    avisit_words <-  
      purrr::map(bds_data, function(x) x %>% dplyr::select(AVISIT)) %>%
        dplyr::bind_rows() %>%
        dplyr::distinct(AVISIT) %>%
        dplyr::pull()

  avisit_fctr  <- 
      purrr::map(bds_data, function(x) x %>% dplyr::select(AVISITN)) %>%
        dplyr::bind_rows() %>%
        dplyr::distinct(AVISITN) %>%
        dplyr::pull()

    awd <- tidyr::tibble(AVISIT = avisit_words, AVISITN = avisit_fctr)
    avisit_words <-
      awd %>%
      dplyr::mutate(AVISIT = factor(AVISIT,
                                    levels = awd[order(awd$AVISITN), "AVISIT"][[1]] %>% unique() )) %>%
      dplyr::pull(AVISIT) %>%
      unique() %>%
      sort()
    
  avisit_words[avisit_words != ""]
}

create_all_cols <- function(datalist) {
  if("ADAE" %in% names(datalist)){
    all_cols <- unique(c(
      colnames(datalist$ADSL)[sapply(datalist$ADSL, class) %in% c('character', 'factor')],
      colnames(datalist$ADAE)[sapply(datalist$ADAE, class) %in% c('character', 'factor')]
    ))
  } else {
    all_cols <- unique(c(
      colnames(datalist$ADSL)[sapply(datalist$ADSL, class) %in% c('character', 'factor')]
    ))
  }
  all_cols
}
