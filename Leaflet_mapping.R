install.packages("leaflet")
library(leaflet)
library(sp)

#http://leaflet-extras.github.io/leaflet-providers/preview/index.html
# for provision of maps
m <- leaflet() %>%
  addProviderTiles("OpenStreetMap.CH")
print(m)

m <- leaflet() %>%
  addTiles()
print(m)


data <- read.csv("MOCK_DATA.csv")
data <- data[complete.cases(data), ]
attach(data)

# spatial data
data$Longitude <- as.numeric(data$Longitude)
data$Latitude <- as.numeric(data$Latitude)
data.SP <- SpatialPointsDataFrame(data[,c(6,7)], data[, -c(6, 7)])


m <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = data, lng = ~ Longitude, lat = ~ Latitude,
             popup = ~ Image)
print(m)

library(leaflet)
dataf <- data.frame(c(43.11940, 43.11940),
                   c(-79.24658, -79.244658),
                   c("HQ 1", "HQ 2"),
                   c(4736583, 3204853),
                   c('<iframe width = "300" height = "169" src = "https://www.youtube.com/watch?v=mTTuUGisxDk&list=PLmFi_ou2WwcEyPq7Y9DvzFRLlp9-XvFDb&index=4">Video</iframe>',
                     '<iframe width = "300" height = "169" src = "https://www.youtube.com/watch?v=mTTuUGisxDk&list=PLmFi_ou2WwcEyPq7Y9DvzFRLlp9-XvFDb&index=4">Video</iframe>'))
names(dataf) <- c("latz", "lngz", "name", "score", "video")
print(dataf)

m <- leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  addMarkers(data = dataf, lat = ~ latz, lng = ~ lngz, 
             popup = ~paste("<h3 style = 'color: red'>Hello</h3>", "<b>Name:</b>",name, 
                            "<b>Score:</b>",score,video, sep = " "))

print(m)

# from A to B
myDF = read.csv("MOCK_DATA.csv")

L1 = myDF[(myDF$Group == "a"),]
L2 = myDF[(myDF$Group == "b"),]

m = leaflet(data = myDF) %>%
  addTiles() %>%
  addPolygons(data = L1, lat = ~ Latitude, lng = ~ Longitude, color = "red") %>%
  addPolygons(data = L2, lat = ~ Latitude, lng = ~ Longitude, color = "blue") %>%
  addPopups(lat = ~ Latitude, lng = ~ Longitude, popup = ~Match.Quality)
print(m)  












