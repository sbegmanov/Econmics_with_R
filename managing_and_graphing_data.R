### data in r or a package
data("anscombe")
ans <- anscombe
str(ans)
head(ans)

library(wooldridge)
data("injury")
str(injury)
names(injury)

### data in a csv file
library(readr)
ifri_car_liv <- read_csv("data/ifri_car_liv.csv")
spec(ifri_car_liv)

ifri <- ifri_car_liv
str(ifri)

### data in a stata file
library(haven)
ifri_car_liv_stata <- read_dta("data/ifri_car_liv.dta")
str(ifri_car_liv_stata)

### data from World Development Indicators
library(WDI)
WDIsearch("gdp.*capita.*PPP")
WDIsearch("CO2.*capita")

# download the data
wdi_data <- WDI(indicator = c("NY.GDP.PCAP.PP.KD",
                              "EN.ATM.CO2E.PC"),
                start = 2010,
                end = 2010,
                extra = TRUE)

names(wdi_data)
write_csv(wdi_data, "data/wdi_data.csv")

WDIsearch("5.1.1_MOZ.TOTA.AID.CAN")
WDIsearch("CO2.*capita")
