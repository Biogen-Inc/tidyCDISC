#' Detect the data standard used for a data set
#'
#' This function attempts to detect the clinical data standard used in a given R data frame.
#'
#' This function compares the columns in the provided \code{"data"} with the required columns for a given data standard/domain combination. The function is designed to work with the SDTM and ADaM CDISC(<https://www.cdisc.org/>) standards for clinical trial data by default. Additional standards can be added by modifying the \code{"standardMetadata"} data set included as part of this package. Currently, "labs" is the only domain supported.
#'
#' @param data A data frame in which to detect the data standard
#' @param includeFields specifies whether to check the data set for field level data in addition to columns.  Default: \code{TRUE}.
#' @param domain The data domain for the data set provided.  Default: \code{"labs"}.
#' @return A list containing the matching \code{"standard"} from \code{"standardMetadata"} and a list of  \code{"details"} describing each standard considered.
#' @export

detectStandard <- function(data, includeFields=TRUE, domain="labs"){
  stopifnot(
    domain=="labs",
    typeof(domain)=="character"
  )
  
  
  # Create placeholder list, with Standard = none.
  available_standards <- safetyGraphics::standardsMetadata %>% select(-.data$text_key) %>% names
  standard_list <- list()
  standard_list[["details"]] = list()
  standard_list[["standard"]] = "none"
  standard_list[["standard_percent"]] = 0
  
  for(standard in available_standards){
    # evaluate the current standard and save the result
    standard_list[["details"]][[standard]]<-evaluateStandard(data,standard=standard, includeFields=includeFields, domain=domain)  
    
    # if the current standard is a better match, use it as the overall standard
    # if there is a tie, don't change the standard - this means the column order in standardMetadata breaks ties!
    current_percent <- standard_list[["details"]][[standard]][["match_percent"]]
    overall_percent <- standard_list[["standard_percent"]]
    if(current_percent > overall_percent){
      standard_list[["standard"]] <- standard  
      standard_list[["standard_percent"]] <- current_percent
    }
  }
  
  # Determine the final standard
  
  # TODO: write a general algorithm to do this ...
  # if(standard_list[["details"]][["sdtm"]][["match"]] == "Full"){
  #   standard_list[["standard"]]<- "sdtm"
  # } else if(standard_list[["details"]][["adam"]][["match"]] == "Full"){
  #   standard_list[["standard"]]<- "adam"
  # } else if(standard_list[["details"]][["sdtm"]][["match"]] == "Partial" |
  #          standard_list[["details"]][["adam"]][["match"]] == "Partial"){
  # standard_list[["standard"]] <- ifelse(
  #   length(standard_list[["details"]][["adam"]][["valid_count"]]) >
  #     length(standard_list[["details"]][["sdtm"]][["valid_count"]]),
  #     "adam" , "sdtm" #SDTM if they are equal
  #   )
  # 
  # } else {
  #   standard_list[["standard"]]<-"None"
  # }
  
  return(standard_list)
}