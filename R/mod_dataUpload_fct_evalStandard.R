#' Evaluate a data set against a data standard
#'
#' Determines whether the required data elements in a data standard are found in a given data frame
#'
#' @param data data.frame to evaluate
#' @param standard standard to evaluate
#' @param includeFields should field level data be evaluated? 
#' @param domain data domain. "labs" only for now. 
#' 
#' @return a list describing to what degree the data set matches the data standard. The "match" property describes compliance with the standard as "full", "partial" or "none". The "checks" property is a list of the data elements expected for the standard and whether they are "valid" in the given data set. "total_checks", "valid_checks" and "invalid_checks" provide counts of the specified checks. "match_percent" is calculated as valid_checks/total_checks.
#'  
#' @import dplyr
#' @importFrom purrr map 
#' @importFrom rlang .data
#' 
#' @keywords internal


evaluateStandard <- function(data, standard, includeFields=TRUE, domain="labs"){
  
  stopifnot(
    is.data.frame(data),
    is.character(standard),
    is.logical(includeFields),
    is.character(domain)
  )
  
  standard<-tolower(standard)
  
  compare_summary<-list()
  compare_summary[["standard"]]<-standard
  
  # Get metadata for settings using the specified standard and see if required data elements are found
  standardChecks <- getSettingsMetadata(cols=c("text_key", "column_mapping", "field_mapping", "field_column_key", "setting_required","standard_val",standard)) %>%
    rename("standard_val"=standard) %>%
    filter(.data$column_mapping == TRUE | .data$field_mapping ==TRUE) %>%
    filter(.data$setting_required==TRUE) %>%
    mutate(type = ifelse(.data$column_mapping, "column", "field")) %>% 
    rowwise %>%
    mutate(field_column_name = ifelse(.data$field_mapping, getSettingsMetadata(cols=standard, text_keys=.data$field_column_key),"")) %>%
    mutate(
      valid = ifelse(.data$column_mapping,
                     hasColumn(data=data, columnName=.data$standard_val),
                     hasField(data=data, columnName=.data$field_column_name, fieldValue=.data$standard_val)
      )) %>%
    select(.data$text_key, .data$standard_val, .data$type, .data$valid)
  
  # filter out the field level checks if includeChecks is false
  if(!includeFields){
    standardChecks <- standardChecks %>% filter(.data$type != "field")
  }
  
  compare_summary[["checks"]] <- standardChecks 
  
  # count valid/invalid data elements
  compare_summary[["total_count"]] <- standardChecks %>% nrow()
  compare_summary[["valid_count"]] <- standardChecks %>% filter(.data$valid) %>% nrow()
  compare_summary[["invalid_count"]] <- standardChecks %>% filter(!.data$valid) %>% nrow()
  compare_summary[["match_percent"]] <- compare_summary[["valid_count"]] / compare_summary[["total_count"]]
  
  if (compare_summary[["invalid_count"]]==0) {
    compare_summary[["match"]] <- "full"
  } else if(compare_summary[["valid_count"]]>0) {
    compare_summary[["match"]] <- "partial"
  } else {
    compare_summary[["match"]] <- "none"
  }
  
  return(compare_summary)
}