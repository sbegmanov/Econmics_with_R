data <- read.csv("data/HS Codes for Petrochemicals_1.xlsx", encoding = "UTF-8")

data_2007 <- data[which(data$year == 2007), ]
data_2007 <- data_2007[order(data_2007$continent, data_2007$country), ]
slope <- 2.666051223553066e-05
data_2007$size <- sqrt(data_2007$pop * slope)
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

fig <- plot_ly(data_2007, x = ~gdpPercap, y = ~lifeExp, color = ~continent,
               size = ~size, colors = colors,
               type = 'scatter', mode = 'markers', sizes = c(min(data_2007$size),
                                                             max(data_2007$size)),
               marker = list(symbol = 'circle', sizemode = 'diameter',
                             line = list(width = 2, color = '#FFFFFF')),
               text = ~paste('Country:', country,
                             '<br>Life Expectancy:', lifeExp,
                             '<br>GDP:', gdpPercap,
                             '<br>Pop.:', pop))
fig <- fig %>%
  layout(title = 'Life Expectancy v. Per Capita GDP, 2007',
         xaxis = list(title = 'GDP per capita (2000 dollars)',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(2.003297660701705, 5.191505530708712),
                      type = 'log', zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Life Expectancy (years)',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(36.12621671352166, 91.72921793264332),
                      zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')

print(fig)

