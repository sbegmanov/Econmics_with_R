library(tidyverse)
library(ggplot2)
library(gtrendsR)

query <- c("electric vehicles", "electric cars", "hybrid electric vehicles", "electric two-wheelers", "electric scooters")
###Problem### - > length(keyword) <= 5 is not TRUE

ec <- gtrends(query, geo = "UZ", gprop = 'youtube')


plot(ec)


coq <- c("CO2")
gtrends(coq, geo = "UZ")
