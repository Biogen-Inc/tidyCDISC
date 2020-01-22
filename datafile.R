library(haven)

# The dataUpload module outputs a list of dataframes
# The first list component is 'Example Data' I'LL EVENTUALLY DELETE THIS!
# After that, each component is given the same name as the uploaded file

dd <- list()
dd$data <- list(`Example Data` =  "GARBAGE", #"safetyGraphics::adlbc",
             adsl.sas7bdat = zap_formats(read_sas("adsl.sas7bdat")),
             advs.sas7bdat = zap_formats(read_sas("advs.sas7bdat")),
             adex.sas7bdat = zap_formats(read_sas("adex.sas7bdat")))

# Your first step will be to remove the example data from your list
# We'll delete this line of code once I clean the UI to not included the Example
dd$data[1] <- NULL