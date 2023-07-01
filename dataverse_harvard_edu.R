library(dataverse)
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

dataverse_search("ecological inference")[c("name", "type", "description")]
