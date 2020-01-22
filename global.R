source('R/detectStandard.R')
source('R/evaluateStandard.R')
source('R/getSettingsMetadata.R')
source('R/hasColumn.R')
source('R/hasField.R')

source('modules/dataUpload.R')
source('modules/dataUploadUI.R')

source('modules/blocks.R')
source('modules/chart.R')
source('modules/chartUI.R')



preload_data_list <- list(data = list("Example data" = safetyGraphics::adlbc),
                            current = 1,
                            standard = list(list("standard" = "adam", "details" = list("adam"=list("match"="full")))),
                            display = list(HTML("<p>Example data - <em style='color:green; font-size:12px;'>ADaM</em></p>")))