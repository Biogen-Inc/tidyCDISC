#' GT Column Names
#' 
#' @param dat data
#' @param nm name
#' @param x columns
#' 
#' create the labels for each column using the total function
#' so the columns are now NAME N= X
#' @export
# get column names with N
col_for_list_expr <- function(nm, x) {
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
#'  
std_err <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

translate_pos <- function(dir){
  if(dir %in% c("left","bottom")) -1
  else if(dir %in% c("right","top")) 1
  else 0 # middle
}

#' Capitalize the first letter of a string
#' @param y the strings to capitalize
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


#' transpose dataframes so they can all 
#' be used with rbind to generate
#' the gt tables
#' @param df the dataframe to transpose
#' @param num the number of rows to return
transpose_df <- function(df, num) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.)
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
#' @importFrom forcats fct_count
#' 
#' @export
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
#' @importFrom tibble as_tibble
#' @importFrom shiny HTML
#' 
#' @return An HTML string
#' 
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
  filter_loc <- as_tibble(f_loc[[1]])
  var_st <- filter_loc$end + 1
  
  # find the end of variable expression susing position of "%>%"
  p_loc <- str_locate_all(code_text,"\\%\\>\\%") # have to use this
  pipe_loc <- as_tibble(p_loc[[1]])
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
#' 
getLevels <- function(x) {if(is.factor(x)) levels(x) else sort(unique(x), na.last = T) } 



#' %quote%
#' @param x test if null
#' @param y return if x is null
#' @return either y or a string
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
#' 
varN_fctr_reorder <- function(data) {
  
  # Now to refactor levels in VARN order, if they exist:
  # save the variable labels into savelbls vector
  savelbls <- sjlabelled::get_label(data)
  
  # identify all char - numeric variables pairs that need factor re-ordering
  cols <- colnames(data)
  non_num_cols <- c(subset_colclasses(data, is.factor),
                    subset_colclasses(data, is.character))
  varn <- paste0(non_num_cols,"N")[paste0(non_num_cols,"N") %in% cols]
  varc <- substr(varn,1,nchar(varn) - 1)
  
  data.table::setDT(data)
  purrr::walk2(varc, varn, ~ refact(data, .x, .y))
  # copy SAS labels back into data
  data <- sjlabelled::set_label(data, label = savelbls)
  return(data)
}

