library(gtrendsR)
library(tidyverse)
library(ggforce)
library(ggrepel)

gtrends(keyword = c("remittance"), time = "today 12-m", geo = "UZ")
trends <- gtrends(keyword = c("credit", "demand"), time = "today 12-m", geo = "UZ")

credit2022 <- trends$interest_over_time

credit2022 %>%
  top_n(5, hits) %>%
  arrange(desc(hits))

credit2022 %>% 
  ggplot(aes(x = date, y = hits,group=keyword, color = keyword))  +
  theme_bw() +
  labs(title = "Google Web searches for 'Credit Demand' in Uzbekistan", caption = "Obs: 3/22 was the day with the most searches", x = NULL, y = "Interest") +
  ggforce::facet_zoom(xlim = c(as.POSIXct(as.Date("2021-01-31")),as.POSIXct(as.Date("2021-01-23")))) +
  geom_smooth(span=0.1,se=FALSE) + geom_vline(xintercept = as.POSIXct(as.Date("2022-01-23")),color = "red", lwd = 0.5,linetype="dashed")+
  theme(legend.position = "none") +
  geom_point(color="black") +
  geom_label_repel(data = subset(credit2022, hits == 100), aes(label = as.character(date)), size = 5, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"))
