library(sf)
library(tidyverse)
library(terra)
library(mapview)
library(tmap)
library(raster)

# download .tif files from https://landscan.ornl.gov/
pop_dat <- list.files("population/", full.names = TRUE)

# create domain to crop with
euro_domain <- st_bbox(c(xmin = -12, xmax = 60, ymin = 25, ymax = 72), crs = st_crs(4326))

#check it out
mapview(euro_domain)

# loop throough each tif
r_all <- list()
for (f in pop_dat){
  
  r <- raster(f)
  r <- crop(r,euro_domain)
  
  r_all[[f]] <- r
  print(f)
}

# brick them
r_b <- brick(r_all)

# create scale
pop_bks <- c(0,1,2,4,8,20,40,80, 100,200,500, 1000, 2000,4000, 8000, 20000, 40000, 100000)
#pop_bks <- c(0, 200, 800, 2000, 50000, 100000)

## define palette for population density
pop_pal <- c("#6e9085", "#c7cdb7", "#b8a69a", "#d2b198", "#f69379", "#af2a3b")
pop_pal <- cols4all::c4a("kovesi.rainbow_bgyrm_35_85_c71", n = 20)[4:20]
r_b2 <- subset(r_b,1:3)
yrz <- seq(2000,2024)
## create plot using tmap
tm_ws <- tm_shape(r_b) +
  tm_raster(col.scale = tm_scale_intervals(values = pop_pal, breaks = pop_bks), col.legend = tm_legend(title = "persons per km\u00B2"))+
  tm_legend(position = c(0.85,0.988), frame = FALSE)+
  tm_layout(bg.color = "lightskyblue1", frame = FALSE,title.position = c(0,0.98),panel.show = FALSE)+
  tm_title(text = yrz, size = 4)+
  tm_facets(nrow = 1, ncol = 1)+
  tm_credits("Source: Landscan Global, Oak Ridge National Laboratory. https://doi.org/10.48690/1532445")

tmap_animation(tm_ws, filename = paste0("europe.gif"), width =2000, height = 2000, dpi = 200, delay = 40)

