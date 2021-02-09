# Script for creating a map for ERA that shows the Elk Valley and associated weather/snow/hydrometric stations

#
library(bcmaps)
library(ggplot2)
library(tidyhydat)
library("sp")
library(dplyr)
library(weathercan)

# Create a map of the Elk Valley with 

bc <- bc_bound()
bc <- st_as_sf(bc)
st_crs(bc) <- 4326

bc_cropped <- st_crop(bc, 
                      xmin = -110, xmax = -115,
                      ymin = 48, ymax = 54)


bb_ca <- c(
  xmin = -122.9,
  ymin =  49.1,
  xmax = -122.5,
  ymax = 49.3
)

ont_crop <- st_crop(bc, xmin = -122.9, xmax = -122.5, ymin = 49.1, ymax = 49.3)


ca_sf <-  bc %>%
  st_geometry() %>%
  st_crop(bb_ca)

cities <- bc_cities() %>%
  dplyr::filter(NAME %in% c('Sparwood', 'Fernie', 'Elkford', 'Cranbrook'))
bec <- bec()

# Watercourses
bcw <- watercourses_5M()

# Get tidyhydat locations
wsc <- hy_stations(prov_terr_state_loc = "BC") %>%
  dplyr::filter(HYD_STATUS == "ACTIVE")

wsc <- st_as_sf(wsc, coords = c("LONGITUDE", "LATITUDE")) #%>%
 # dplyr::filter(!is.na(coordinates))
st_crs(wsc) <- 4326

# climate stations
#weather <- weather_dl()

# Get the snow locations

# To plot - cities, DEM, rivers, watershed deliniation, WSC sites, snow sites, bec zones?

ggplot() +
  geom_sf(data = ca_sf, fill = NA) +
  geom_sf(data = bcw, col = "lightblue") +
  geom_sf(data = cities) +
  geom_sf(data = wsc, col = "blue") +
  theme_bw()
