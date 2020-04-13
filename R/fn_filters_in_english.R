

# Function accepts an filtered data object from IDEAFilter (shiny_data_filter module) and translates the information
# to be more readable to the lay person (a non- R programmer) but still assumes some knowledge of filtering expressions
# IE, the app is for an audience who is familar with programming (SAS programmers).
filters_in_english <- function(filtered_data){
  
  # grab the output
  orig_code <- paste(capture.output(attr(filtered_data, "code")),collapse = "")
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
  return(HTML(paste0("<b>Filters Applied:</b><br/>&nbsp;&nbsp;&nbsp;&nbsp;"
                     ,paste(disp_msg, collapse = "<br/>&nbsp;&nbsp;&nbsp;&nbsp;"))))
}
