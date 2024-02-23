library(plotly)

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")
fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School,
               type = 'scatter', mode = 'markers',
               marker = list(size = ~Gap, opacity = 0.5,
                             color = 'rgb(255, 65, 54)'))

fig <- fig %>%
  layout(title = "Gender Gap in Earnings per University",
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

print(fig)

# setting multiple colors
data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

colors <- c('rgba(204,204,204,1)', 'rgba(222,45,38,0.8)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
            'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
            'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
            'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
            'rgba(204,204,204,1)')
# Note: The colors will be assigned to each observations based on the order of the observations in the data frame.

fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School,
               type = 'scatter', mode = 'markers',
               marker = list(size = ~Gap, opacity = 0.5,
                             color = colors))

fig <- fig %>%
  layout(title = "Gender Gap in Earnings per University",
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))
print(fig)

# mapping a color variable(continuous)
data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School,
               type = 'scatter', mode = 'markers',
               color = ~Gap, colors = 'Reds',
               marker = list(size = ~Gap, opacity = 0.5))

fig <- fig %>%
  layout(title = "Gender Gap in Earnings per University",
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))
print(fig)

# mapping a color variable(categorical)
data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

data$State <- as.factor(c('Massachusetts', 'California', 'Massachusetts', 'Pennsylvania', 
                          'New Jersey', 'Illinois', 'Washington DC',
                          'Massachusetts', 'Connecticut', 'New York', 
                          'North Carolina', 'New Hampshire', 'New York', 'Indiana',
                          'New York', 'Michigan', 'Rhode Island', 'California', 
                          'Georgia', 'California', 'California'))

fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School,
               type = 'scatter', mode = 'markers',
               size = ~Gap, color = ~State, colors = 'Paired',
               marker = list(opacity = 0.5, sizemode = 'diameter'))
fig <- fig %>%
  layout(title = "Gender Gap in Earnings per University",
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)
print(fig)

# Scaling the size of bubble charts
data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

data$State <- as.factor(c('Massachusetts', 'California', 'Massachusetts', 'Pennsylvania', 
                          'New Jersey', 'Illinois', 'Washington DC',
                          'Massachusetts', 'Connecticut', 'New York', 
                          'North Carolina', 'New Hampshire', 'New York', 'Indiana',
                          'New York', 'Michigan', 'Rhode Island', 'California', 'Georgia', 
                          'California', 'California'))

fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School,
               type = 'scatter', mode = 'markers',
               size = ~Gap, color = ~State, colors = 'Paired',
               # choosing the range of the bubbles' size:
               sizes = c(10, 50),
               marker = list(opacity = 0.5, sizemode = 'diameter'))
fig <- fig %>%
  layout(title = "Gender Gap in Earnings per University",
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)
print(fig)

# Scaling using Sizeref
data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

data$State <- as.factor(c('Massachusetts', 'California', 'Massachusetts', 'Pennsylvania', 
                          'New Jersey', 'Illinois', 'Washington DC',
                          'Massachusetts', 'Connecticut', 'New York', 
                          'North Carolina', 'New Hampshire', 'New York', 'Indiana',
                          'New York', 'Michigan', 'Rhode Island', 'California', 
                          'Georgia', 'California', 'California'))
# Use the ideal sizeref value
desired_maximum_marker_size <- 40
your_list_of_size_values <- data['Gap']
sizeref <- 2.0 * max(your_list_of_size_values) / (desired_maximum_marker_size**2)

fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School, type = 'scatter', 
               mode = 'markers', color = ~State, colors = 'Paired',
               sizes = c(10, 50),
               marker = list(size = your_list_of_size_values, 
                             opacity = 0.5, sizemode = 'area', sizeref = sizeref))

fig <- fig %>% 
  layout(title = 'Gender Gap in Earnings per University',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE),
                      showlegend = FALSE)
print(fig)

# Scaling V2
data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

data$State <- as.factor(c('Massachusetts', 'California', 'Massachusetts', 'Pennsylvania', 
                          'New Jersey', 'Illinois', 'Washington DC',
                          'Massachusetts', 'Connecticut', 'New York', 'North Carolina', 
                          'New Hampshire', 'New York', 'Indiana',
                          'New York', 'Michigan', 'Rhode Island', 'California', 
                          'Georgia', 'California', 'California'))


fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School, type = 'scatter', 
               mode = 'markers', size = ~Gap, color = ~State, colors = 'Paired',
               #Choosing the range of the bubbles' sizes:
               sizes = c(10, 50),
               marker = list(opacity = 0.5, sizemode = 'diameter'))

fig <- fig %>% 
  layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)

print(fig)


# Hover Text with Bubble Charts

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

data$State <- as.factor(c('Massachusetts', 'California', 'Massachusetts', 'Pennsylvania',
                          'New Jersey', 'Illinois', 'Washington DC',
                          'Massachusetts', 'Connecticut', 'New York', 'North Carolina',
                          'New Hampshire', 'New York', 'Indiana',
                          'New York', 'Michigan', 'Rhode Island', 'California',
                          'Georgia', 'California', 'California'))

fig <- plot_ly(data, x = ~Women, y = ~Men, type = 'scatter', mode = 'markers',
               size = ~Gap, color = ~State, colors = 'Paired',
               sizes = c(10, 50),
               marker = list(opacity = 0.5, sizemode = 'diameter'),
               hoverinfo = 'text',
               text = ~paste('School:', School, '<br>Gender Gap:', Gap))

fig <- fig %>%
  layout(title = 'Gender Gap in Earnings per University',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE),
                      showlegend = FALSE)

print(fig)


# Styled Bubble Chart

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv")
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



















