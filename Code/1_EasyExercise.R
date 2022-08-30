#Geocomputation with R
#Exercises
#Tutorial

#Libraries
library(sf)
library(rgeoboundaries)
library(terra)
library(tmap)
library(supercells)
library(dplyr)

#--E1. Programmatically download borders of Germany----
ger = gb_adm0('Germany', type = "SSCGS")
plot(ger)

col = gb_adm0('Colombia', type = "SSCGS")
plot(col)


#--E2. Read the median (P50) global snow cover monthly values for December 2019 ----
#from  https://zenodo.org/record/6011200/
P50_2019_url = "/vsicurl/https://zenodo.org/record/6011200/files/clm_snow.cover_esa.modis_p50_1km_s0..0cm_2019.12_epsg4326_v1.tif?download=1"
snow_P50_url = rast(P50_2019_url)
snow_P50_url
plot(snow_P50_url)


#Other way
#y2019     = "https://zenodo.org/record/6011200/files/clm_snow.cover_esa.modis_p50_1km_s0..0cm_2019.12_epsg4326_v1.tif?download=1"
#y2019rast = rast(paste0("/vsicurl/", y2019))
#plot(y2019rast)


#--E3. Limit the snow cover data extent to the area of Germany ----
snow_germ   = crop(snow_P50_url,ger)
plot(snow_germ)
snow_germ_f = mask(snow_germ,ger)
plot(snow_germ_f)


#--E4. Transform the Germany borders and the Germany snow cover data to a local projection--
#snowcover
crs(snow_germ_f)
snow_germ_f_pro = project(snow_germ_f, "epsg:4839")
plot(snow_germ_f_pro)

#st_geometry(snow_germ_f_pro)

#borders
ger_4839 = st_transform(ger, "epsg:4839")
plot(ger_4839)
#str(ger_4839)
plot(st_geometry(ger_4839))


#--E5. Create a map of the median (P50) Germany snow cover monthly values for December 2019 using the {tmap} package----
tm_shape(snow_germ_f_pro) +
  tm_raster(#style = "cont", palette = "-RdYlGn",
            title = "Snow") +
  tm_shape(ger_4839) + 
  tm_borders()+
  tm_layout(main.title = "Germany",
            bg.color = "lightgrey")



#Nice one
tm1 = tm_shape(snow_germ_f_pro) +
  tm_graticules() +
  tm_raster(title = "Snow depth",
            palette = "viridis",
            style = "cont") +
  tm_shape(ger_4839) +
  tm_borders(lwd = 3,col="red") +
  tm_scale_bar(position = c("left", "bottom"),
               breaks = c(0, 20, 40)) +
  tm_compass(position = c("right", "top")) +
  tm_layout(main.title = "Snow for Germany")
tm1


#-- E6. Save the map created in the previous exercise to a .png file 
tmap_save(tm1, "my_map.png", height = 745, width = 1000, dpi = 150)


#-- E7. Use the {supercells} package to create superpixels
set.seed(2021-08-19)
y2019rastsc = supercells(snow_germ_f_pro, k = 10000, compactness = 1)
y2019rastsck = kmeans(sf::st_drop_geometry(y2019rastsc[4]), centers = 6)
y2019rastsc$k = y2019rastsck$cluster

tm_shape(snow_germ_f_pro) +
  tm_raster(style = "cont", palette = "viridis") +
  tm_shape(y2019rastsc) +
  tm_polygons(col = "k", style = "cat")






