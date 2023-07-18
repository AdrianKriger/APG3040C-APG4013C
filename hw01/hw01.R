## APG3040C-APG4013C July-Sept 2023
## arkriger

##---------
#-- student number: [write your student number here]
#-- name:           [write your name.surname (exactly like that {name.surname}) here]
#----------

##-- create a folder on your C:/data/ and place the relevant files there. 
##-- work through the script block-by-block. 
##-- i.e.: highlight #1. 
##         Click Run. read the output. Answer the questions and follow the instructions; if any. 
##--       then highlight #2. etc.

#-- you might need to install these very useful packages
#1
install.packages(c("sf", "tidyverse", "terra", "ggplot2"))

#2
library(sf)
library(tidyverse)
library(ggplot2)
library(terra)
#library(patchwork)


##-- WE'LL START WITH VECTORS

#3
#-- read the dataset
veg <- st_read("c:/data/Vegetation_Indigenous.shp")

#-- Q1: What is the CRS of the vector dataset?
#-- [delete this line and write your answer here]

#4
#-- We can have a more detailed look at the CRS
st_crs(veg)

#-- Q2: On which Longitude is the dataset centered? (hint: its "Longitude of natural origin")
#-- [delete this line and write your answer here]

#5
#-- look at the data
head(veg)

#-- Q3: What class of data is veg? (hint: you'll use the class() function)
##- delete this line and write your answer here]

#6
#-- lets create a basic plot
plot(veg)
#-- Export the plot as an image and name it Rpolt01. Hand these in

##-- we're going to CROP this dataset and then SUBSET the dataframe 
##-- crop with COORDINATES and trim the dataframe with a COMMAND

#7
#-- Make a vector with desired coordinates in metres 
ext <- c(-66642.18, -3809853.29, -44412.18, -3750723.29) 

#8
#Give the vector names
names(ext) <- c("xmin", "ymin", "xmax", "ymax") 
ext

#- Q4: Why would we want to name the vectors?
# [delete this line and write your answer here]

#9
#-- We crop (physically cut) our dataset. Note that we're overwriting "veg".
veg <- st_crop(veg, ext)

#10
#-- lets create a fancy plot with ggplot (part of tidyverse) of our CROP.
ggplot() + geom_sf(data=veg, aes(fill = `National_`))
#-- Export the plot as an image and name it Rpolt02. Hand these in

#11
#-- we've CROPPED (cut with coordinates) the data now we subset the data.frame with the data we want. 
#-- It's very similar to vector with coordinates
split_veg <- c("Peninsula Granite Fynbos - North", 
               "Peninsula Granite Fynbos - South", 
               "Cape Flats Dune Strandveld - West Coast", 
               "Cape Flats Dune Strandveld - False Bay")

#Use base R indexing to select attributes from the already cropped dataset
vegsub <- veg[which(veg$National_ %in% split_veg),]

#Plot
ggplot() + geom_sf(data=vegsub, aes(fill = `National_`))
#-- Export the plot as an image and name it Rpolt03. Hand these in


##-- NOW WE'LL CONTINUE WITH RASTERS


#12
#-- read and look at the dataset 
dem  = rast("c:/data/CoCT_10m.tif")
#dem  = read_stars("c:/2023July-Sept/APG3040C_APG4013C-AdvancedSpatialAnalysis/assignments/hw01/data/CoCT_10m.tif")
dem

#-- Q5: What is the CRS of the raster dataset?
#-- [delete this line and write your answer here]

#-- Q6: What is the spatial resolution of the raster dataset?
#-- [delete this line and write your answer here]

#13
#-- We can have a more detailed look at the CRS
crs(dem)

#-- Q7: On which Longitude is the dataset centered? (hint: its "Longitude of natural origin")
#-- [delete this line and write your answer here]

#14
#-- We'll perform some basic raster cropping based on coordinates. Its very similar to the sf ~~ st_crop() from #9 above
dem <- crop(dem, ext(c(-66642.18, -44412.18, -3809853.29, -3750723.29)))
dem

#15 
#-- Although this might sound almost unbelievable; there are times when working
#-- with lower resolution data is better. At those times we often resample to a lower resolution. 
#dem_30 <- warp(dem_trim, cellsize = 30, use_gdal=TRUE, no_data_value=-9999)
dem30 <- aggregate(dem, fact = 3)#, fun = mean)
dem30 <- resample(dem, dem30, method='bilinear')
dem30
#plot(dem30, col = terrain.colors(50))

#-- Q7: What is the spatial resolution of the elevation raster now?
#-- [delete this line and write your answer here]

#16
#-- Create the slope
slope <- terrain(dem30, "slope", unit = "radians")

#17
#-- estimate the aspect or orientation
aspect <- terrain(dem30, "aspect", unit = "radians")

#18
#-- calculate the hillshade effect with 45º of elevation
hillshade <- shade(slope, aspect, angle = 45, direction = 270, normalize= TRUE)

#19
#-- plot all together 
par(mfrow=c(1,3))
plot(slope, legend=FALSE)
plot(aspect, legend=FALSE, yaxt = "n")
plot(hillshade, col = grey(c(0:100)/100), legend = F, maxcell = Inf, smooth = FALSE,  yaxt = "n")
#-- Export the plot as an image and name it Rpolt04. Hand these in


## ------- ### NOT DONE FROM HERE


pts_matrix = matrix(c(-253145, -6229725,
                      -3266860, -3241958),
                    ncol = 2, byrow = TRUE)
line = st_sfc(st_linestring(pts_matrix), crs = 2180)
line = st_line_sample(line, density = 1)
line = st_cast(line, "POINT")















dem30df <- as.data.frame(dem30, xy = TRUE)
names(dem30df)[3] <- "10m_BA "

# map
ggplot() +
  geom_raster(data = dem30df,
              aes(x, y, fill = alt)) +
  # geom_sf(data = suiz_lakes,
  #         fill = "#c6dbef", 
  #         colour = NA) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m") +
  #coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")












# calculate the hillshade effect with 45º of elevation
#hillshade <- shade(slope, aspect, angle = 45, direction = 315, normalize= TRUE)

# final hillshade 
plot(hillshade, col = grey(1:100/100))

hillshade %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = hillshade)) + #note that the hillshade column name in this case is "hillshade"
  scale_fill_gradient(low = "grey10", high = "grey90")


# calculate the hillshade effect with 45º of elevation
hillshade <- shade(slope, aspect, 
                     angle = 45, 
                     direction = 300,
                     normalize= TRUE)

# final hillshade 
plot(hillshade, col = grey(1:100/100))









# Plot the DEMs
p1 <- ggplot(dem %>% as.data.frame(xy = TRUE)) +
  geom_raster(aes(x = x, y = y, fill = `10m_BA`)) +
  scale_fill_gradientn(colors = terrain.colors(100))
p2 <- ggplot(dem30 %>% as.data.frame(xy = TRUE)) +
  geom_raster(aes(x = x, y = y, fill = `10m_BA`)) +
  scale_fill_gradientn(colors = terrain.colors(100))

# Combine the plots side by side
combined_plot <- p1 + p2 + plot_layout(ncol = 2, guides = "collect")

# Display the combined plot
combined_plot



# Compute slope
slope <- terrain(dem, "slope", unit = "radians")

# Compute aspect
aspect <- terrain(dem, "aspect", unit = "radians")

# Compute hillshade
hillshade <- shade(slope, aspect)

plot(hillshade)

hillshade %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = hillshade)) + #note that the hillshade column name in this case is "hillshade"
  scale_fill_gradient(low = "grey10", high = "grey90")


# Create the slope, aspect, and hillshade
slope <- terrain(dem30, slope, unit="degrees", neighbors=8)
aspect <- terrain(dem30, aspect, unit="degrees", neighbors=8)
hillshade <- hillShade(slope, aspect, 30)

# Plot the slope, aspect, and hillshade side by side
p1 <- ggplot(slope %>% as.data.frame(xy = TRUE)) +
  geom_raster(aes(x = x, y = y, fill = slope)) +
  scale_fill_gradientn(colors = terrain.colors(10))
p2 <- ggplot(aspect %>% as.data.frame(xy = TRUE)) +
  geom_raster(aes(x = x, y = y, fill = aspect)) +
  scale_fill_gradientn(colors = terrain.colors(10))
# p3 <- ggplot(hillshade %>% as.data.frame(xy = TRUE)) +
#   geom_raster(aes(x = x, y = y, fill = hillshade)) +
#   scale_fill_gradientn(colors = terrain.colors(10))
p3 <- ggplot(hillshade %>% as.data.frame(xy = TRUE) +
  geom_raster(aes(x = x, y = y, fill = hillshade)) + #note that the hillshade column name in this case is "hillshade"
  scale_fill_gradientn(low = "grey10", high = "grey90"))

# Combine the plots side by side
combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 3, guides = "collect")

# Display the combined plot
combined_plot






#12
#-- read and look at the dataset 
#dem  = read_stars("c:/data/CoCT_10m.tif")
dem  = read_stars("c:/2023July-Sept/APG3040C_APG4013C-AdvancedSpatialAnalysis/assignments/hw01/data/CoCT_10m.tif")
dem

#-- Q5: What is the CRS of the raster dataset?
#-- [delete this line and write your answer here]

#-- Q6: What is the spatial resolution of the raster dataset?
#-- [delete this line and write your answer here]

#4
#-- We can have a more detailed look at the CRS
st_crs(dem)

#-- Q7: On which Longitude is the dataset centered? (hint: its "Longitude of natural origin")
#-- [delete this line and write your answer here]

#5
#-- We'll perform some basic raster cropping based on coordinates. 
bbox <- st_bbox(c(xmin = -66642.18, xmax = -44412.18, ymin = -3809853.29, ymax = -3750723.29), crs = st_crs(dem))
#-- Its very similar to the sf ~~ st_crop() from #9 above
dem_trim <- st_crop(dem, bbox)
dem_trim

# Resample to 30m
#dem_30 <- resample(dem_trim, 30)
dem_30 <- st_warp(dem_trim, cellsize = 30, use_gdal=TRUE, no_data_value=-9999)
dem_30


# Create the slope, aspect, and hillshade
slope <- terrain(dem30, "slope", unit="degrees", neighbors=8)
aspect <- terrain(dem30, "aspect", unit="degrees", neighbors=8)
hillshade <- shade(slope, aspect, angle=45, direction = 315, normalize=TRUE)

# Plot the slope, aspect, and hillshade side by side
p1 <- ggplot(slope %>% as.data.frame(xy = TRUE)) +
  geom_raster(aes(x = x, y = y, fill = slope)) +
  scale_fill_gradientn(colors = terrain.colors(10))
p2 <- ggplot(aspect %>% as.data.frame(xy = TRUE)) +
   geom_raster(aes(x = x, y = y, fill = aspect)) +
   scale_fill_gradientn(colors = terrain.colors(10))
# P3 <- ggplot(hillshade %>% as.data.frame(xy = TRUE)) +
#    geom_raster(aes(x = x, y = y, fill = hillshade)) +
#    scale_fill_gradientn(colors = terrain.colors(10))

P3 <- ggplot(hillshade %>% as.data.frame(xy = TRUE)) +
  geom_raster(aes(x = x, y = y, fill = hillshade)) +
  scale_fill_gradientn(colors = grey(1:100/100))

# Combine the plots side by side
combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 3, guides = "collect")

# Display the combined plot
combined_plot

# convert the hillshade to xyz
hilldf_single <- as.data.frame(dem30, xy = TRUE)

dem30df <- as.data.frame(dem30, xy = TRUE)

# map 
ggplot() +
  geom_raster(data = hilldf_single,
              aes(x, y, fill = hillshade),
              show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = dem30df,
              aes(x, y, fill = alt),
              alpha = .7) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  # geom_sf(data = suiz_lakes,
  #         fill = "#c6dbef", colour = NA) +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")



## For better handling we set here the names
names(dem) <- "alt"





#16
#-- Create the slope
slope <- terrain(dem30, "slope", unit = "radians")
plot(slope, legend=FALSE)

#17
#-- estimate the aspect or orientation
aspect <- terrain(dem30, "aspect", unit = "radians")
plot(aspect, legend=FALSE)

#18
#-- calculate the hillshade effect with 45º of elevation
hillshade <- shade(slope, aspect, angle = 45, direction = 270, normalize= TRUE)
#plot(hillshade, col = grey(c(0:100)/100), legend = F, maxcell = Inf, smooth = FALSE)

#19
#-- plot all together 
par(mfrow=c(1,3))
plot(slope, legend=FALSE)
plot(aspect, legend=FALSE, yaxt = "n")
plot(hillshade, col = grey(c(0:100)/100), legend = F, maxcell = Inf, smooth = FALSE,  yaxt = "n")

