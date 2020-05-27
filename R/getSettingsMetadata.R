#' Get metadata about chart settings
#'
#' Retrieve specified metadata about chart settings from the data/settingsMetadata.Rda file.
#'
#' @param charts optional vector of chart names used to filter the metadata. Exact matches only (case-insensitive). All rows returned by default.
#' @param text_keys optional vector of keys used to filter the metadata. Partial matches for any of the strings are returned (case-insensitive). All rows returned by default.
#' @param filter_expr optional filter expression used to subset the data.
#' @param add_standards should data standard info stored in standardsMetadata be included
#' @param cols optional vector of columns to return from the metadata. All columns returned by default.
#' @param metadata metadata data frame to be queried
#'
#' @return dataframe with the requested metadata or single metadata value
#'
#' @examples
#' safetyGraphics:::getSettingsMetadata()
#' # Returns a full copy of settingsMetadata.Rda
#'
#' safetyGraphics:::getSettingsMetadata(text_keys=c("id_col"))
#' # returns a dataframe with a single row with metadata for the id_col setting
#'
#' safetyGraphics:::getSettingsMetadata(text_keys=c("id_col"), cols=c("label"))
#' # returns the character value for the specified row.
#'
#' @importFrom stringr str_subset
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export

getSettingsMetadata<-function(charts=NULL, text_keys=NULL, cols=NULL, filter_expr=NULL, add_standards=TRUE, metadata = safetyGraphics::settingsMetadata){
  md <- metadata %>% mutate(text_key=as.character(.data$text_key))
  
  if(add_standards){
    ms<-safetyGraphics::standardsMetadata %>% mutate(text_key=as.character(.data$text_key))
    md<-md%>%left_join(ms, by="text_key")
  }
  
  all_columns <- names(md)
  
  #filter the metadata based on the charts option (if any)
  if(!is.null(charts)){ #Don't do anything if charts isn't specified
    stopifnot(typeof(charts) == "character")
    
    # get list of all chart flags in the data
    chart_columns <- tolower(str_subset(all_columns, "^chart_"));
    
    # get a list of chart flags matching the request
    charts<-tolower(charts)
    matched_chart_columns <- intersect(chart_columns, paste0("chart_",charts))
    #filter based
    if(length(matched_chart_columns)==0){
      return(NULL)
    }else{
      # see if any of the matched chart flags are TRUE
      md<-md%>%filter_at(vars(matched_chart_columns),any_vars(.))
    }
  }
  
  #filter the metadata based on the text_keys option (if any)
  if(!is.null(text_keys)){
    stopifnot(typeof(text_keys) == "character")
    md<-md%>%filter(tolower(.data$text_key) %in% tolower(text_keys))
  }
  
  #filter the metadata based on a the filter expression
  filter_expr <- enexpr(filter_expr)
  if(!is.null(filter_expr)){
    stopifnot(typeof(filter_expr) %in% c("language","symbol"))
    md<-md %>% filter(!!filter_expr)
  } 
  
  
  #subset the metadata columns returned based on the metadata_columns option (if any)
  if(!is.null(cols)){
    stopifnot(typeof(cols) =="character")
    valid_cols <- intersect(cols, names(md))
    md<-md%>%select(valid_cols)
  }    
  
  
  #coerce factors to character
  if(dim(md)[2]>0){
    i <- sapply(md, is.factor)
    md[i] <- lapply(md[i], as.character)
  }
  
  #return the requested metadata
  if(dim(md)[1]==0 | dim(md)[2]==0){ #return null if no rows or no columns are selected
    return(NULL)
  }else if(dim(md)[2]==1){ #return a vector if there is only a single columns specified
    return(md[[1]])
  }else{ #otherwise return the whole data frame
    return(md)
  }
}