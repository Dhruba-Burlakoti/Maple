#####################################################
library(sp)
library(rgdal)

# Create Spatial Point dataframe:
spdat <- dat
coordinates(spdat)= ~LON + LAT


# Define Map Projection:
proj4string(spdat) <- CRS("+init=epsg:4326")

# Map the data:
library(tmap)
library(sf)
library(raster)

# Convert sp to sf:
sfdat <- st_as_sf(spdat)

tm_basemap("Stamen.Watercolor") +
  tm_shape(sfdat) +
  tm_bubbles(col = "black", shape = 1, size = .1) +
  tmap_mode("view")

tm_basemap("Esri.WorldTopoMap") +
  tm_shape(sfdat) +
  tm_bubbles(col = "black", shape = 1, size = .1) +
  tmap_mode("view")

###############################################################
# Scale Color Brewer:

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- ggplot(dsamp, aes(carat, price)) +
    geom_point(aes(colour = clarity)))
d
d + scale_colour_brewer()
#Change scale label
d + scale_color_brewer("Diamond \nclarity")
# Select brewer palette to use:
?scales::brewer_pal
d + scale_color_brewer(palette = "Dark2")


# To look for all available palette
display.brewer.all()
scale_colour_brewer(
  ...,
  type = "seq",
  palette = 1,
  direction = 1,
  aesthetics = "colour"
)



dat_sf$Insect
summary(dat_sf$Insect)
summary(as.factor(dat_sf$Insect))

summary(dat_sf$Animal_fact)

is.facet(dat_sf$Insect)
is.factor(dat_sf$Insect)
is.numeric(dat_sf$Insect)


Ins <- cut(dat_sf$Insect, br = c(0, 10, 20, 50, 100, 2040 ))
summary(Ins)
length(Ins)

table(Ins)
plot(Ins)
is.factor(Ins)
rm(Ins_eq)
#######################################################
library(tidyverse)
rm(list = ls())

mi_counties <- map_data("county", "michigan") %>% 
  select(lon = long, lat, group, id = subregion)
head(mi_counties)


ggplot2::map_data()

ggplot(mi_counties, aes(lon, lat)) + 
  geom_point(size = .25, show.legend = TRUE) +
  coord_quickmap()

ggplot(mi_counties, aes(lon, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey50") + 
  coord_quickmap()
library(ozmaps)

baseborderNep <- st_as_sf(ne_states(country="Nepal"))
ggplot() +
  geom_sf(data = baseborderNep)










