dd <- list()
dd$data <- list(
  ADSL = zap_formats(read_sas("/Users/mgans/OneDrive - Biogen/Desktop/TableGenerator_Data/adsl.sas7bdat")),
  ADVS = zap_formats(read_sas("/Users/mgans/OneDrive - Biogen/Desktop/TableGenerator_Data/advs.sas7bdat")),
  ADEX = zap_formats(read_sas("/Users/mgans/OneDrive - Biogen/Desktop/TableGenerator_Data/adex.sas7bdat")))


block_names <- function(df_to_pivot) {
  unique(df_to_pivot[["PARAMCD"]])
}

pivot_paramcd <- function(df_to_pivot) {
  # First are we pivoting AVAL or both AVAL and CHG
  ifelse(
    "CHG" %in% colnames(df_to_pivot),
    values <<- c("CHG", "AVAL"),
    values <<- "AVAL"
  )
  
  # Use the values to pivot by AVAL or AVAL and CHG
  df_to_pivot %>% 
    select(PARAMCD, USUBJID, !!values) %>%
    group_by(PARAMCD) %>%
    mutate(rn = row_number()) %>%
    ungroup %>%
    pivot_wider(names_from = PARAMCD, values_from = !!values) %>%
    select(-rn)
}

# Loop over the dataframe list and keep non-parmcd files the same
# Then replace the dataframes containing paramcd
# This is ugly and takes a long time 

data_for_blocks <- list()
for (i in 1:length(dd$data)) {
  ifelse((length(grep("PARAMCD", names(dd$data[[i]]))) == 0) ,
         data_for_blocks[[i]] <- names(dd$data[[i]]),
         data_for_blocks[[i]] <- block_names(dd$data[[i]]))
}

names(data_for_blocks) <- names(dd$data)


###
###

dd <- list()
dd$data <- list(
  ADSL = zap_formats(read_sas("/Users/mgans/OneDrive - Biogen/Desktop/TableGenerator_Data/adsl.sas7bdat")),
  ADVS = zap_formats(read_sas("/Users/mgans/OneDrive - Biogen/Desktop/TableGenerator_Data/advs.sas7bdat")),
  ADEX = zap_formats(read_sas("/Users/mgans/OneDrive - Biogen/Desktop/TableGenerator_Data/adex.sas7bdat")))


pivot_paramcd <- function(df_to_pivot) {
  # First are we pivoting AVAL or both AVAL and CHG
  ifelse(
    "CHG" %in% colnames(df_to_pivot),
    values <<- c("CHG", "AVAL"),
    values <<- "AVAL"
  )
  
  # Use the values to pivot by AVAL or AVAL and CHG
  df_to_pivot %>% 
    select(PARAMCD, USUBJID, !!values) %>%
    group_by(PARAMCD) %>%
    mutate(rn = row_number()) %>%
    ungroup %>%
    pivot_wider(names_from = PARAMCD, values_from = !!values) %>%
    select(-rn)
}

# Loop over the dataframe list and keep non-parmcd files the same
# Then replace the dataframes containing paramcd
# This is ugly and takes a long time 

data_for_blocks <- list()
for (i in 1:length(dd$data)) {
  ifelse((length(grep("PARAMCD", names(dd$data[[i]]))) == 0) ,
         data_for_blocks[[i]] <- dd$data[[i]],
         data_for_blocks[[i]] <- pivot_paramcd(dd$data[[i]]))
}
names(data_for_blocks) <- names(dd$data)
