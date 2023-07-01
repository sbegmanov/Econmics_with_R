download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
system("unzip world_shape_file.zip")

library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0("data/world_shape_file") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3", verbose=FALSE
)

par(mar = c(0, 0, 0, 0))
plot(my_spdf, col = "#f2f2f2", bg="skyblue", lwd=0.25, border=0 )

library(broom)
spdf_fortified <- tidy(my_spdf, region = "NAME")

library(ggplot2)
ggplot() +
  geom_polygon(data = spdf_fortified, aes(x = long, y = lat, group = group),
               fill="#69b3a2", color="white") +
  theme_void()
