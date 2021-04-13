# Script for creating a map for ERA that shows the Elk Valley and associated weather/snow/hydrometric stations
rm(list=ls())

#
library(bcmaps)
library(ggplot2)
library(tidyhydat)
library("sp")
library(dplyr)
library(weathercan)
library(shadowtext)
library(bcdata)
#install.packages("rgdal")
library(rgdal)
library("ggspatial")

# Create a map of the Elk Valley with 
x_max <- 1870000
x_min <- 1730000

y_min <- 478000.3
y_max <- 690000

bc_cropped <- st_crop(st_geometry(transform_bc_albers(bc_neighbours())),
                      xmin = x_min, xmax = x_max,
                      ymin = y_min, ymax = y_max)
st_crs(bc_cropped)

cities <- bc_cities() %>%
  dplyr::filter(NAME %in% c('Sparwood', 'Fernie', 'Elkford')) %>%
  dplyr::mutate(
    COORDS = purrr::map(geometry, st_coordinates),
    COORDS_X = purrr::map_dbl(COORDS, 1),
    COORDS_Y = purrr::map_dbl(COORDS, 2)
  )

cities$nudge_x <- 0
cities$nudge_y <- 5000

# BEC zones
bec_trim <- st_crop(bec(), 
               xmin = x_min, xmax = x_max,
               ymin = y_min, ymax = y_max)

# Watercourses
bcw <- st_crop(st_geometry(transform_bc_albers(watercourses_5M())),
               xmin = x_min, xmax = x_max,
               ymin = y_min, ymax = y_max)

# Get tidyhydat locations
wsc <- hy_stations(prov_terr_state_loc = "BC") %>%
  dplyr::filter(HYD_STATUS == "ACTIVE")

wsc <- st_as_sf(wsc, coords = c("LONGITUDE", "LATITUDE"))
st_crs(wsc) <- 4326

# Trim
wsc_trim <- st_crop(st_geometry(transform_bc_albers(wsc 
                                                    %>% dplyr::filter(HYD_STATUS == "ACTIVE"))),
        xmin = x_min, xmax = x_max,
        ymin = y_min, ymax = y_max)

# WSC drainages from BC maps
#wsc_d <- st_crs(st_geometry(transform_bc_albers(wsc_drainages()[wsc_drainages()$SUB_SUB_DRAINAGE_AREA_NAME == "Elk",])))
wsc_d <- wsc_drainages()[wsc_drainages()$SUB_SUB_DRAINAGE_AREA_NAME == "Elk",]

# climate stations

weather_sf <- st_as_sf(stations %>% dplyr::filter(!is.na(lon)), 
                       coords = c("lon", "lat")) %>%
  dplyr::filter(!is.na(geometry)) %>%
  st_set_crs(4326) %>% 
  transform_bc_albers() %>%
  st_geometry()

weather <- st_crop(weather_sf, 
        xmin = x_min, xmax = x_max,
        ymin = y_min, ymax = y_max)

# get only the weather stations within the elk valley
# Watershed delination
fr_albers <- st_transform(wsc_d, crs = 3005)


ec_albers <- st_transform(st_as_sf(weather), crs = st_crs(fr_albers))

weather_elk <- st_filter(ec_albers, fr_albers) 

# Get the snow locations
snow <- bcsnowdata::snow_auto_location() %>%
  dplyr::filter(LOCATION_ID %in% c("2C09Q"))
snow <- st_geometry(snow)

# Temp sites from ERA
fn = read.csv("/Users/user/Dropbox/ERA_scripts/ERA/data/temp_sites_loc.csv")
temp <- fn %>%
  st_as_sf(coords = c("Longitude", "Latitude"),
         crs= 4326,
         agr= "constant")


# get the dem
#aoi <- census_subdivision()[census_subdivision()$CENSUS_SUBDIVISION_NAME %in% 
#                              c("Elkford", "Sparwood", "Fernie"), ]
#aoi_raster <- cded_raster(aoi)

# To plot - cities, DEM, rivers, watershed deliniation, WSC sites, snow sites, bec zones?

ggplot() +
  geom_sf(data = bc_cropped, fill = NA) +
  #geom_sf(data = bec_trim,
  #        aes(fill = ZONE, col = ZONE)) +
  geom_sf(data = bcw, col = "lightblue") +
  geom_sf(data = wsc_d, 
          #aes(color="Elk River Watershed"),
          colour = "black", 
          size = 1,
          fill = NA) +
  geom_sf(data = wsc_trim, 
          #colour = "blue", 
          aes(colour = "WSC Sites", 
              shape = "WSC Sites"),
          #shape = 9, 
          size = 3,
          show.legend = "point") +
  geom_sf(data = weather_elk,
          aes(colour = "Climate Stations", 
              shape = "Climate Stations"),
          #shape = 9, 
          size = 3,
          show.legend = "point") +
  geom_sf(data = snow, 
          #colour = "red",
          aes(colour = "Snow Weather Stations", 
              shape = "Snow Weather Stations"),
          #shape = 10, 
          size = 3,
          show.legend = "point") +
  geom_sf(data = temp, 
          aes(colour = "ERA Temperature Monitoring Sites", 
              shape = "ERA Temperature Monitoring Sites"),
          #colour = "black", 
          #shape = 12, 
          size = 3,
          fill = NA,
          show.legend = "point") +
  geom_sf(data = cities,
          aes(color = "Cities", 
              shape = "Cities"),
          size = 2,
          #shape = 24,
          #fill = "orange", 
          #colour = "black",
          show.legend = "point")  +
  shadowtext::geom_shadowtext(data = cities,
                              mapping = aes(
                                x = COORDS_X,
                                y = COORDS_Y,
                                label = NAME
                              ),
                              nudge_x = cities$nudge_x,
                              nudge_y = cities$nudge_y,
                              size = 4,
                              colour = "black",
                              bg.colour = "white") +
  theme_bw() +
  xlab("") +
  ylab("") +
  ggtitle("Monitoring Sites in the Elk Valley") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_colour_manual(name = "",
                      values = c("Cities" = "#000000", 
                                 "WSC Sites" = "#E69F00",
                                 "Climate Stations" = "#56B4E9",
                                 "Snow Weather Stations" = "#009E73", 
                                 "ERA Temperature Monitoring Sites" = "#D55E00")) +   
  scale_shape_manual(name = "",
                     labels = c("Cities", 
                                 "WSC Sites", 
                                 "Climate Stations", 
                                 "Snow Weather Stations", 
                                 "ERA Temperature Monitoring Sites"),
                     values = c(17, 12, 10, 9, 18))


