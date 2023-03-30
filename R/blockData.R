#' import R6
table_blocks <-
  R6::R6Class("table_blocks",
              list(
                #' @field datalist A list of ADaM-ish datasets used to generate the table
                datalist = NULL,
                #' @field all_rows A list of parameters/fields from the datalist with descriptions
                all_rows = NULL,
                #' @field blocks A data frame containing the block data
                blocks = dplyr::tibble(
                  agg = character(),
                  block = character(),
                  dataset = character(),
                  dropdown = character(),
                  S3 = list(),
                  gt_group = glue::glue(),
                  label = character(),
                  label_source = character()
                ),
                #' @description 
                #' Create a new block data object
                #' @param datalist A list of ADaM-ish datasets used to generate the table
                initialize = function(datalist) {
                  self$datalist <- datalist
                  
                  init <- sapply(datalist, function(x) "PARAMCD" %in% colnames(x) & !("CNSR" %in% colnames(x)))
                  BDS <- datalist[init]

                  ADSL <- datalist$ADSL
                  metadata <- data.frame(col_names = colnames(ADSL))
                  metadata$code <- NA

                  for (i in 1:nrow(metadata)) {
                    if("label" %in% names(attributes(ADSL[[metadata$col_names[i]]]))){
                      metadata$code[i] <- attr(ADSL[[metadata$col_names[i]]], "label")
                    }
                  }

                  new_list <- lapply(BDS, function(x) x %>% select(PARAMCD, PARAM) %>% distinct())

                  new_list[[length(new_list) + 1 ]] <- metadata
                  names(new_list)[length(new_list)] <- "ADSL"

                  # only display ADAE column blocks if an ADAE is uploaded!
                  if ("ADAE" %in% names(datalist)) {
                    # this also doesn't need to depend on pre-filters
                    ADAE <- datalist$ADAE
                    # Display variable blocks that are only unique to ADAE
                    ADAE_blocks <- data.frame(
                      col_names = dplyr::setdiff(colnames(ADAE), metadata$col_names)
                    )
                    ADAE_blocks$code <- NA

                    for (i in 1:nrow(ADAE_blocks)) {
                      if("label" %in% names(attributes(ADAE[[ADAE_blocks$col_names[i]]]))){
                        ADAE_blocks$code[i] <- attr(ADAE[[ADAE_blocks$col_names[i]]], "label")
                      }
                    }
                    new_list[[length(new_list) + 1 ]] <- ADAE_blocks
                    names(new_list)[length(new_list)] <- "ADAE"
                  }

                  self$all_rows <- new_list
                  
                  avisit_words <-
                    if(any(purrr::map_lgl(datalist, ~"AVISIT" %in% colnames(.x)))){
                      purrr::map(BDS, function(x) x %>% dplyr::select(AVISIT)) %>%
                        dplyr::bind_rows() %>%
                        dplyr::distinct(AVISIT) %>%
                        dplyr::pull()
                    } else {
                      NULL
                    }
                  
                  avisit_fctr <-
                    if(any(purrr::map_lgl(datalist, ~"AVISIT" %in% colnames(.x)))){
                      purrr::map(BDS, function(x) x %>% dplyr::select(AVISITN)) %>%
                        dplyr::bind_rows() %>%
                        dplyr::distinct(AVISITN) %>%
                        dplyr::pull()
                    } else {
                      1:2
                    }
                  
                  private$my_weeks <- 
                    if (is.null(avisit_words)) {
                      NULL
                    } else {
                      awd <- tidyr::tibble(AVISIT = avisit_words, AVISITN = avisit_fctr)
                      avisit_words <-
                        awd %>%
                        dplyr::mutate(AVISIT = factor(AVISIT,
                                                      levels = awd[order(awd$AVISITN), "AVISIT"][[1]] %>% unique() )) %>%
                        dplyr::pull(AVISIT) %>%
                        unique()
                      avisit_words[avisit_words != ""] %>%
                        as.vector()
                    }
                  
                  private$all_cols <-
                    if("ADAE" %in% names(datalist)){
                      unique(c(
                        colnames(datalist$ADSL)[sapply(datalist$ADSL, class) %in% c('character', 'factor')],
                        colnames(datalist$ADAE)[sapply(datalist$ADAE, class) %in% c('character', 'factor')]
                      ))
                    } else { # just adsl cols
                      unique(c(
                        colnames(datalist$ADSL)[sapply(datalist$ADSL, class) %in% c('character', 'factor')]
                      ))
                    }
                  
                  private$my_avals <- 
                    if (!any(purrr::map_lgl(datalist, ~ "ATPT" %in% colnames(.x)))) {
                      list()
                    } else {
                      atpt_datasets <- purrr::map_lgl(datalist, ~ "ATPT" %in% colnames(.x))
                      
                      avals <- 
                        purrr::map(datalist[atpt_datasets], ~ .x %>%
                                     dplyr::select(PARAMCD, dplyr::any_of(c("ATPT"))) %>%
                                     dplyr::filter(dplyr::if_any(-PARAMCD, ~ !is.na(.x) & .x != "")) %>%
                                     dplyr::pull(PARAMCD) %>%
                                     get_levels()
                        )
                      
                      purrr::imap(avals, ~ purrr::map(.x, function(i, j =.y) {
                        datalist[[j]] %>% 
                          dplyr::filter(PARAMCD == i) %>%
                          dplyr::select(dplyr::any_of(c("ATPT", "ATPTN"))) %>%
                          varN_fctr_reorder() %>%
                          dplyr::select(dplyr::any_of(c("ATPT"))) %>%
                          purrr::map(~ .x %>%
                                       addNA(ifany = TRUE) %>%
                                       purrr::possibly(relevel, otherwise = .)(NA_character_) %>%
                                       get_levels() %>%
                                       tidyr::replace_na("N/A") %>%
                                       {if (length(.) > 1) c("ALL", .) else .} %>%
                                       as.list())
                      }) %>%
                        purrr::set_names(.x)
                      )
                    }
                  
                },
                #' @description 
                #' Print the data frame containing the blocks
                print = function() {
                  print(self$blocks)
                  invisible(self)
                },
                #' @description 
                #' Add block to the block data object
                #' @param variable The parameter or field the statistic is based on
                #' @param stat The statistic to be calculated
                #' @param dropdown A subgroup on which the statistic is calculated (usually an AVISIT)
                #' @param df The dataset the parameter or field is from
                add_block = function(variable, stat, dropdown, tpnt, df) {
                  blocks <- list()
                  aggs <- list()
                  get_var <- function(x) {
                    if (missing(x)) {
                      block_txt <- readline("INPUT: ")
                    } else {
                      block_txt <- x
                    }
                    if (block_txt == "1") {
                      purrr::iwalk(tmp$all_rows, ~ {cat(.y, ":\n  ", sep = ""); cat(.x[[1]], sep = ", "); cat("\n")})
                      get_var()
                    } else if (block_txt == "2") {
                      cat(names(self$all_rows), sep = ", ")
                      get_var()
                    } else if (block_txt %in% names(self$all_rows)) {
                      cat(self$all_rows[[block_txt]][[1]], sep = ","); cat("\n")
                      get_var()
                    } else {
                      if (!any(purrr::map_lgl(self$all_rows, ~ block_txt %in% .x[[1]]))) {
                        cat("Param/field not found. Please type 1 to see all available options.\n")
                        get_var()
                      }
                      return(block_txt)
                    }
                  }
                  get_filter <- function(x, atpt_lst) {
                    opt_lst <- c("NONE", purrr::imap(atpt_lst, ~ glue::glue("{.y} - {.x}")) %>% unlist())
                    
                    if (missing(x)) {
                      filter_txt <- readline("INPUT: ")
                    } else {
                      filter_txt <- x
                    }
                    if (filter_txt == "A") {
                      cat("Please type the name or the number corresponding to the desired time point.\n")
                      cat(paste0(seq_along(opt_lst), ": ", opt_lst), sep = "\n"); cat("\n")
                      get_filter(atpt_lst = atpt_lst)
                    } else if (filter_txt %in% seq_along(opt_lst)) {
                      if (filter_txt == "1")
                        return("NONE")
                      str_parse <- stringr::str_match(opt_lst[as.numeric(filter_txt)], "(^.*?) - (.*$)")
                      filter_return <- str_parse[3]
                      names(filter_return) <- str_parse[2]
                      return(filter_return)
                    } else if (filter_txt %in% opt_lst) {
                      if (filter_txt == "NONE")
                        return("NONE")
                      str_parse <- stringr::str_match(filter_txt, "(^.*?) - (.*$)")
                      filter_return <- str_parse[3]
                      names(filter_return) <- str_parse[2]
                      return(filter_return)
                    } else if (filter_txt %in% unlist(atpt_lst)) {
                      if (sum(filter_txt == unlist(atpt_lst)) > 1) {
                        cat('Time point is not unique. Please type "A" to see all available options.\n')
                        get_filter(atpt_lst = atpt_lst)
                      } else {
                        x <- as.character(match(filter_txt, unlist(atpt_lst)) + 1)
                        get_filter(x, atpt_lst = atpt_lst)
                      }
                    } else {
                      cat('Time point not valid. Please type "A" to see all available options.\n')
                      get_filter(atpt_lst = atpt_lst)
                    }
                  }
                  get_stat <- function(x) {
                    if (missing(x)) {
                      agg_txt <- readline("INPUT: ")
                    } else {
                      agg_txt <- x
                    }
                    if (agg_txt == "A") {
                      cat("Please type statistic or the number corresponding to desired stat.\n")
                      cat(paste0(seq_along(private$stats), ": ", private$stats), sep = "\n"); cat("\n")
                      get_stat()
                    } else if (agg_txt %in% seq_along(private$stats)) {
                      return(private$stats[as.numeric(agg_txt)])
                    } else if (agg_txt %in% private$stats) {
                      return(agg_txt)
                    } else {
                      cat('Statistic not valid. Please type "A" to see all available options.\n')
                      get_stat()
                    }
                  }
                  get_dropdown <- function(x, opts = c("weeks", "cols")) {
                    opts <- match.arg(opts)
                    
                    opt_lst <-
                      if (opts == "weeks") {
                        c("NONE", "ALL", private$my_weeks)
                      } else {
                        c("NONE", private$all_cols)
                      }
                    
                    if (missing(x)) {
                      agg_val <- readline("INPUT: ")
                    } else {
                      agg_val <- x
                    }
                    if (is.null(agg_val) || is.na(agg_val)) {
                      return(NULL)
                    } else if (agg_val == "A") {
                      cat("Please type the week or the number corresponding to the desired option.\n")
                      cat(paste0(seq_along(opt_lst), ": ", opt_lst), sep = "\n"); cat("\n")
                      get_dropdown(opts=opts)
                    } else if (agg_val %in% seq_along(opt_lst)) {
                      return(opt_lst[as.numeric(agg_val)])
                    } else if (agg_val %in% opt_lst) {
                      return(agg_val)
                    } else {
                      cat('Option not valid. Please type "A" to see all available.\n')
                      get_dropdown(opts=opts)
                    }
                  }
                  get_df <- function(x, possible_dfs) {
                    if (!missing(x) && !(x %in% possible_dfs || x %in% seq_along(possible_dfs))) {
                      cat("The selected variable is not in the supplied dataset.\n")
                    } else if (!missing(x) && x %in% possible_dfs) {
                      return(x)
                    } else if (!missing(x) && x %in% seq_along(possible_dfs)) {
                      return(possible_dfs[as.numeric(x)])
                    } else if (missing(x) & length(possible_dfs) == 1) {
                      return(possible_dfs)
                    }
                    
                    cat("Please type the dataset or the number corresponding to the desired option.\n")
                    cat(paste0(seq_along(possible_dfs), ": ", possible_dfs), sep = "\n"); cat("\n")
                    df_val <- readline("Input: ")
                    get_df(df_val, possible_dfs)
                  }
                  
                  if (missing(variable))
                    cat('Please provide a PARAMCD or field.', 
                        'To see all options, type 1. To see all datasets, type 2. To see options for a particular dataset, type its name (e.g. "ADAE").\n', sep = "\n")
                  blocks$txt <- get_var(variable)
                  possible_dfs <- names(self$all_rows)[purrr::map_lgl(self$all_rows, ~ blocks$txt %in% .x[[1]])]
                  blocks$df <- get_df(df, possible_dfs)
                  
                  if (blocks$df %in% names(private$my_avals) && blocks$txt %in% names(private$my_avals[[blocks$df]])) {
                    if (missing(tpnt))
                      cat('Pleae provide a time point.',
                          'To see all options, type "A".\n', sep = "\n")
                    atpt_lst <- private$my_avals[[blocks$df]][[blocks$txt]]
                    filter_return <- get_filter(tpnt, atpt_lst = atpt_lst)
                    blocks$grp <- names(filter_return)
                    blocks$val <- as.character(filter_return)
                    if (blocks$val == "ALL")
                      blocks$lst <- atpt_lst[[blocks$grp]][-1]
                  }
                  
                  if (missing(stat))
                    cat('Please provide an aggregator.',
                        'To see all options, type "A".\n', sep = "\n")
                  aggs$txt <- get_stat(stat)
                  
                  if (aggs$txt %in% c("ANOVA", "CHG", "MEAN") & !is.null(private$my_weeks)) {
                    if (missing(dropdown))
                      cat('Please provide an AVISIT.',
                          'To see all options, type "A".\n', sep = "\n")
                    aggs$val <- get_dropdown(dropdown, "weeks")
                    if (!is.null(aggs$val) && aggs$val == "ALL")
                      aggs$lst <- as.list(private$my_weeks)
                  } else if (aggs$txt %in% c("NESTED_FREQ_DSC", "NESTED_FREQ_ABC")) {
                    if (missing(dropdown))
                      cat('Please provide a field.',
                          'To see all options, type "A",\n', sep = "\n")
                    aggs$val <- get_dropdown(dropdown, "cols")
                  }
                  
                  process_drops <- process_droppables(list(list(aggs)), list(list(blocks)))
                  private$block_drop <- c(private$block_drop, process_drops$blocks)
                  private$agg_drop <- c(private$agg_drop, process_drops$aggs)
                  
                  private$create_TG(private$agg_drop, private$block_drop)
                  
                  self
                },
                #' @description 
                #' Remove block from the block data object
                #' @param x vector specifying elements to remove from block data object
                remove_block = function(x) {
                  
                  private$block_drop <- private$block_drop[-x]
                  private$agg_drop <- private$agg_drop[-x]
                  
                  private$create_TG(private$agg_drop, private$block_drop)
                  
                  self
                }
              ),
              list(
                stats = c("ANOVA", "CHG", "MEAN", "FREQ", "Y_FREQ", "MAX_FREQ", "NON_MISSING", "NESTED_FREQ_DSC", "NESTED_FREQ_ABC"),
                my_weeks = NULL,
                all_cols = NULL,
                my_avals = NULL,
                block_drop = list(),
                agg_drop = list(),
                create_TG = function(aggs, blocks) {
                  blockData <- convertTGOutput(aggs, blocks)
                  
                  blockData$label <-
                    "N/A"
                  
                  blockData$label_source <-
                    "N/A"
                  
                  self$blocks <- blockData
                  invisible(self)
                }
              )
  )

#' Create Block Data Object
#' 
#' @param datalist A list of ADaM-ish datasets used to generate the table
#' 
#' @return A block data object
#' 
#' @export
#' 
#' @examples 
#' 
#' datalist <- list(ADSL = tidyCDISC::adsl, ADVS = tidyCDISC::advs, 
#'                  ADAE = tidyCDISC::adae, ADLBC = tidyCDISC::adlbc)
#' bd <- createBlockdata(datalist)
#' bd
createBlockdata <- function(datalist) {
  table_blocks$new(datalist = datalist)
}

#' Add Block to Block Data Object
#' 
#' @param bd A block data object
#' @param variable The parameter or field the statistic is based on
#' @param stat The statistic to be calculated
#' @param dropdown A subgroup on which the statistic is calculated (usually an AVISIT)
#' @param df The dataset the parameter or field is from
#' 
#' @return The \code{bd} block data object with additional block
#' 
#' @export
#' 
#' @examples 
#' 
#' datalist <- list(ADSL = tidyCDISC::adsl, ADVS = tidyCDISC::advs, 
#'                  ADAE = tidyCDISC::adae, ADLBC = tidyCDISC::adlbc)
#' bd <- createBlockdata(datalist)
#' 
#' if (interactive()) {
#'   addBlock(bd)
#'   bd
#' }
#' 
#' addBlock(bd, "DIABP", "MEAN", "ALL")
#' bd
addBlock <- function(bd, variable, stat, dropdown, tpnt, df) {
  bd$add_block(variable = variable, stat = stat, dropdown = dropdown, tpnt = tpnt, df = df)
}

#' Remove Block(s) from Block Data Object
#' 
#' @param bd A block data object
#' @param x vector specifying elements to remove from block data object
#' 
#' @return The \code{bd} block data object with additional block
#' 
#' @export
removeBlock <- function(bd, x) {
  bd$remove_block(x = x)
}
