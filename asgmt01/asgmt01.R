## APG3040C-APG4013C July-Sept 2023
## arkriger

##---------
#-- student number: [write your student number here]
#-- name:           [write your name.surname (exactly like that {name.surname}) here]
#----------

##-- create a folder on your C:/asgmt01/ and place the relevant files there. 
##-- work through the script block-by-block. 
##-- i.e.: highlight #1. 
##         Click Run. read the output. Answer the questions and follow the instructions; if any. 
##--       then highlight #2. etc.

# ---you might need to install these very useful packages
#1
install.packages(c("sf", "tidyverse", "terra", "ggplot2", "patchwork"))

#2
# ---load-packages--------------------------------------------------
library(sf)
library(tidyverse)
library(ggplot2)
library(terra)
library(patchwork)

# ---setting a working directory is always helpful------------------
setwd("C:/asgmt01/")

## ---WE'LL START WITH VECTORS----------------------------------------

#3
# ---read the dataset-------------------------------------------------
veg <- st_read("./data/Vegetation_Indigenous.shp")

# ---Q1: What is the CRS of the vector dataset?-----------------------
# --[delete this line and write your answer here]

#4
# ---We can have a more detailed look at the CRS---------------------
st_crs(veg)

# ---Q2: On which Longitude is the dataset centered?-----------------
#        (hint: its "Longitude of natural origin")
# --[delete this line and write your answer here]

#5
# ---look at the data------------------------------------------------
head(veg)

# ---Q3: What class of data is veg?----------------------------------
#        (hint: you'll use the class() function)
# --[delete this line and write your answer here]

#6
# ---lets create a basic plot----------------------------------------
plot(veg)
# --Export the plot as an image and name it Rpolt01. Hand these in---

## ---we're going to CROP this dataset and then SUBSET the data.frame-- 
##    crop with COORDINATES and trim the data.frame with a COMMAND

#7
# ---Make a vector with desired coordinates in metres---------------- 
ext <- c(-66642.18, -3809853.29, -44412.18, -3750723.29) 

#8
# --Give the vector names--------------------------------------------
names(ext) <- c("xmin", "ymin", "xmax", "ymax") 
ext

# ---Q4: Why would we want to name the vectors?----------------------
# --[delete this line and write your answer here]

#9
# ---We crop (physically cut) our dataset.---------------------------
#    Note that we're overwriting "veg".
veg <- st_crop(veg, ext)

#10
# ---lets create a fancy plot with ggplot of our CROP----------------
ggplot() + geom_sf(data=veg, aes(fill = `National_`))
# --Export the plot as an image and name it Rpolt02. Hand these in

#11
# ---we've CROPPED (cut with coordinates) the data now we subset the data.frame with the data we want. 
#    It's very similar to vector with coordinates but with column headings 

split_veg <- c("Peninsula Granite Fynbos - North", 
               "Peninsula Granite Fynbos - South", 
               "Cape Flats Dune Strandveld - West Coast", 
               "Cape Flats Dune Strandveld - False Bay")

#Use base R indexing to select attributes from the already cropped dataset
vegsub <- veg[which(veg$National_ %in% split_veg),]

#Plot
ggplot() + geom_sf(data=vegsub, aes(fill = `National_`))
# --Export the plot as an image and name it Rpolt03. Hand these in


## ---NOW WE'LL CONTINUE WITH RASTERS--------------------------------


#12
# ---read and look at the dataset------------------------------------ 
dem  = rast("./data/CoCT_10m.tif")
dem

# ---Q5: What is the CRS of the raster dataset?----------------------
# --[delete this line and write your answer here]

# ---Q6: What is the spatial resolution of the raster dataset?-------
# --[delete this line and write your answer here]

#13
# ---We can have a more detailed look at the CRS---------------------
crs(dem)
# ---or
st_crs(dem)

# ---Q7: On which Longitude is the dataset centered?----------------- 
#        (hint: its "Longitude of natural origin")
# --[delete this line and write your answer here]

#14
# ---We'll perform some basic raster cropping based on coordinates---
#    Its very similar to the sf ~~ st_crop() from #9 above
dem <- crop(dem, ext(c(-66642.18, -44412.18, -3809853.29, -3750723.29)))
dem

#15 
# ---Although this might sound almost unbelievable; there are times when working
#    with lower resolution data is better. At those times we often resample to a lower resolution. 
#dem_30 <- warp(dem_trim, cellsize = 30, use_gdal=TRUE, no_data_value=-9999)
dem30 <- aggregate(dem, fact = 3)#, fun = mean)
dem30 <- resample(dem, dem30, method='bilinear')
dem30
#plot(dem30, col = terrain.colors(50))

# ---Q8: What is the spatial resolution of the elevation raster now?---
# --[delete this line and write your answer here]

#16
# ---we'll use 'terra' for quickly visualizing slope, aspect and a hillshade

# ---create the slope--------------------------------------------------
slope <- terrain(dem30, "slope", unit = "radians")

#17
# ---estimate the aspect or orientation-------------------------------
aspect <- terrain(dem30, "aspect", unit = "radians")

#18
# ---calculate the hillshade effect with 45-degree of elevation--------------
hillshade <- shade(slope, aspect, angle = 45, direction = 270, normalize= TRUE)

#19
#-- plot all together-------------------------------------------------- 
# Plot hillshade
p1 <- hillshade %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = hillshade)) +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  theme(legend.position = "none") +
  labs(x = 'x' , y = 'y')

# Plot slope
p2 <- slope %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = slope)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(legend.position = "none") +
  labs(x = 'x' , y = 'y')

# Plot aspect
p3 <- aspect %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = aspect)) +
  scale_fill_gradient(low = "brown", high = "green") +
  theme(legend.position = "none") +
  labs(x = 'x' , y = 'y')

# Combine plots
plots <- hillshade_plot + slope_plot + aspect_plot + plot_layout(ncol = 3, guides = "collect")

# Plot with shared y-axis
plots_shared_y <- plots + ylab('y')

# Print plot
print(plots_shared_y)
# --Export the plot as an image and name it Rpolt04. Hand these in

# ---Q9: While a hillshade gives a very clear picture of the topography
#    Why would we want to know the slope and aspect? Who would use these datasets?
# --[delete this line and write your answer here]

