#' GT Column Names
#' 
#' @param nm name
#' 
#' create the labels for each column using the total function
#' so the columns are now NAME N= X
#' @export
#' @keywords tabGen_repro
#' 
# get column names with N
col_for_list_expr <- function(nm) {
  nm = md(glue::glue("**{row_names_n}** <br> N={total}"))
}


#' My GG Color Hue
#'
#' Grab specific colors from the ggplot2 default color palette of length n
#'
#' @param n An output reactive dataframe from IDEAFilter
#'
#' @return character vector of hex colors of length n
#'
#' @family indvExp Functions
#' @noRd
#'   
my_gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n +1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#' Standard Error Calculation
#' 
#' Calculates the square root of variance divided by n
#'
#' @param x A numeric vector
#' @param na.rm logical, should NA's be removed? Defaults to FALSE
#'
#' @return numeric, representing the standard error
#'
#' @family tableGen Functions
#' @noRd
#'  
std_err <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

#' Translate the position of into a integer needed for ggplot2
#' @param dir the strings to capitalize
#' @noRd
#' 
translate_pos <- function(dir){
  if(dir %in% c("left","bottom")) -1
  else if(dir %in% c("right","top")) 1
  else 0 # middle
}

#' Capitalize the first letter of a string
#' @param y the strings to capitalize
#' @noRd
#' 
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


#' transpose dataframes so they can all 
#' be used with rbind to generate
#' the gt tables
#' 
#' @param df the dataframe to transpose
#' @param num the number of rows to return
#' @noRd
#' 
transpose_df <- function(df, num) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    dplyr::mutate(rownames = rownames(.), .before = 1) %>%
    tidyr::as_tibble(.)
  return(t_df[-num,])
}

#' Identify Names of Columns
#' 
#' transform the gt rownames from generics to the column name and the total N of
#' each column
#'
#' @param data the data to create columns with
#' @param group whether to group the data to calculate Ns
#'  
#' @export
#' @keywords tabGen_repro
#' 
common_rownames <- function(data, group) { 
  if (is.null(group) ) { #| group == "NONE"
    vars <- c("Variable", "Total")
  } else {
    if(is.factor(data[[group]])){
      # droplevels() get's rid of levels that no longer exist in the data post filtering
      lvls <- levels(data[[group]]) # removed droplevels() to retain all trt grps
    } else {
      lvls <- sort(unique(data[[group]]))
    }
    vars <- c("Variable", lvls, "Total")
    vars[vars == ""] <- "Missing"
  }
  return(vars)
}

# #' Figure out which reactive data.frame to use
# #'
# #' @param d a string, naming a data.frame
# #' @param ae an AE reactive data frame
# #' @param alt an alternative reactive data frame
# #' @noRd
# #'
# data_to_use_str <- function(d, ae, alt) {
#   if (dat == "ADAE") { ae }
#   else alt
# }

# #' Figure out which reactive data.frame to use
# #'
# #' @param x a string, naming a data.frame. Either
# #' @noRd
# "data_to_use_str"
# # function included in mod_tableGen... not here.
# # data_to_use_str <- function(x) {
# #   if (x == "ADAE") ae_data() else all_data()
# # }



#' Convert actions performed on from an IDEAFilter output dataframe into text
#'
#' Function accepts a filtered data object from IDEAFilter (shiny_data_filter
#' module) and translates the information to be more readable to the lay person
#' (a non- R programmer) but still assumes some knowledge of filtering
#' expressions. IE, the app is for an audience who is familar with programming
#' (SAS programmers).
#'
#' @param filtered_data An output reactive dataframe from IDEAFilter
#' @param filter_header A header to label the output string

#'   DO NOT REMOVE.
#' @import dplyr
#' @importFrom dplyr %>%
#' @importFrom purrr map2
#' @importFrom stringr str_locate_all
#' @importFrom utils capture.output
#' @importFrom tidyr as_tibble
#' @importFrom shiny HTML
#' 
#' @return An HTML string
#' @noRd
#' 
filters_in_english <- function(filtered_data, filter_header = "Filters Applied:"){
  
  # grab the output
  orig_code <- paste(utils::capture.output(attr(filtered_data, "code")),collapse = "")
  # orig_code <- 'processed_data %>% filter(ABIFN1 %in% c(NA, "NEGATIVE")) %>% filter(ABIFN1 %in% c(NA, "POSITIVE"))'
  # convert double quotes to single quotes
  code_text <- gsub('\"',"\'",orig_code)
  
  # find the character position for the end of the string
  len <- nchar(code_text)
  
  # find the start of the variable expressions using position of "filter"
  f_loc <- str_locate_all(code_text,"filter\\(")
  filter_loc <- tidyr::as_tibble(f_loc[[1]])
  var_st <- filter_loc$end + 1
  
  # find the end of variable expression susing position of "%>%"
  p_loc <- str_locate_all(code_text,"\\%\\>\\%") # have to use this
  pipe_loc <- tidyr::as_tibble(p_loc[[1]])
  num_pipes <- nrow(pipe_loc)
  var_end <- c(pipe_loc$start[ifelse(num_pipes == 1, 1, 2):num_pipes] - 3, len - 1) # ifelse(num_pipes == 1, 1, 2)
  
  # use map2, to apply multiple arguments to the substr function, returing a list
  filter_vectors <- map2(.x = var_st, .y = var_end, function(x,y) substr(code_text,x,y))
  my_msgs <- filter_vectors[!(is.na(filter_vectors) | filter_vectors == "")] # get rid of NA msgs
  
  # clean up messages to read more naturally
  disp_msg <- gsub("\\%in\\%","IN",
                   gsub("c\\(","\\(",
                        gsub("\\(NA","\\(Missing",
                             gsub(".na",".Missing",
                                  gsub("   "," ", # 3 spaces
                                       gsub("  "," ", # 2 spaces
                                            gsub("\\|","OR",
                                                 gsub("\\&","AND",
                                                      my_msgs
                                                 ))))))))
  
  # format as html in a specific format, with indentation
  return(HTML(paste0("<b>",filter_header,"</b><br/>&nbsp;&nbsp;&nbsp;&nbsp;"
                     ,paste(disp_msg, collapse = "<br/>&nbsp;&nbsp;&nbsp;&nbsp;"))))
}

#' getLevels function
#'
#' Return levels of a factor/vector
#'
#' @param x a vector
#'   
#' @return x vector 
#' 
#' @export
#' @keywords helpers
#' 
getLevels <- function(x) {if(is.factor(x)) levels(x) else sort(unique(x), na.last = T) } 



#' %quote%
#' @param x test if null
#' @param y return if x is null
#' @return either y or a string
#' @noRd
#'
`%quote%` <- function(x,y) {
  if (is.null(x)) {
    y
  } else {
    paste0("\'",x,"\'") #sQuote(x) # old
  }
}



#' Re-order Factor Levels by VARN
#' 
#' Function to that looks for VARN counterparts to any character or factor VAR
#' variables in any dataframe and re-orders there factor levels, taking the lead
#' from VARN's numeric guide.
#' 
#' @param data a dataframe, including one enriched with SAS labels attributes
#' 
#' @importFrom sjlabelled get_label set_label
#' @importFrom data.table setDT 
#' @importFrom purrr walk2 
#' 
#' @export
#' @keywords helpers
#' 
varN_fctr_reorder2 <- function(data) {
  # rm(data)
  # data <- all_data
  # Now to refactor levels in VARN order, if they exist:
  # save the variable labels into savelbls vector
  savelbls <- sjlabelled::get_label(data)
  
  # identify all char - numeric variables pairs that need factor re-ordering
  cols <- colnames(data)
  non_num_cols <- c(subset_colclasses(data, is.factor),
                    subset_colclasses(data, is.character))
  varn <- paste0(non_num_cols,"N")[paste0(non_num_cols,"N") %in% cols]
  varc <- substr(varn,1,nchar(varn) - 1)
  
  if(!rlang::is_empty(varn)){
    for(i in 1:length(varn)){
      this_varn <- as.character(varn[i])
      this_varc <- varc[i]
      this_varn_sym <- rlang::sym(this_varn)
      this_varc_sym <-rlang::sym(this_varc)
      pref_ord <- data %>% select(one_of(this_varc, this_varn)) %>% distinct() %>% arrange(!!this_varn_sym)
      data <-
        data %>% mutate(!!this_varc_sym := factor(!!this_varc_sym, levels = unique(pref_ord[[this_varc]])))
      # return(data)
    }
  }

  # copy SAS labels back into data
  data <- sjlabelled::set_label(data, label = savelbls)
  return(data)
}


