library(tidyverse)
library(readxl)

df <- read_xlsx("Data_UZB_HQ.xlsx")
View(df)
str(df)
colnames(df)
attach(df)

ggplot(df, aes(x = ??????????))