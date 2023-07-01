library(plotly)

Primates <- c('Potar monkey', 'Gorilla', 'Human', 'Rhesus monkey', 'Chimp')
Bodywt <- c(10.0, 207.0, 62.0, 6.8, 52.2)
Brainwt <- c(115, 406, 1320, 179, 440)

data <- data.frame(Primates, Bodywt, Brainwt)

fig <- plot_ly(data, x = ~Bodywt, y = ~Brainwt, type = "scatter",
               mode = 'text', text = ~Primates, textposition = 'middle right',
               textfont = list(color = '#000000', size = 16))
fig <- fig %>%
  layout(title = 'Primates Brain and Body Weight',
         xaxis = list(title = 'Body Weight(kg)',
                      zeroline = TRUE,
                      range = c(0, 25)),
         yaxis = list(title = 'Brain Weight(g)',
                      range = c(0, 1400)))
print(fig)
