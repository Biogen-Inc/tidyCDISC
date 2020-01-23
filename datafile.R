library(haven)

# The dataUpload module outputs a list of dataframes
# The first list component is 'Example Data' I'LL EVENTUALLY DELETE THIS!
# After that, each component is given the same name as the uploaded file

dd <- list()
dd$data <- list(`Example Data` =  "GARBAGE", #"safetyGraphics::adlbc",
             ADSL = zap_formats(read_sas("../../TableGenerator_Data/adsl.sas7bdat")),
             ADVS = zap_formats(read_sas("../../TableGenerator_Data/advs.sas7bdat")),
             ADEX = zap_formats(read_sas("../../TableGenerator_Data/adex.sas7bdat")))

# Your first step will be to remove the example data from your list
# We'll delete this line of code once I clean the UI to not included the Example
dd$data[1] <- NULL

lapply(names(dd$data), function(x) assign(x, dd$data[[x]], envir = .GlobalEnv))

filenames <- sort(names(dd$data))
filenames <- filenames[filenames != "ADSL"]

lapply(names(dd$data), function(x) assign(x, dd$data[[x]], envir = .GlobalEnv)) 
