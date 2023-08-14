# Advanced Spatial Data Analysis ~ Interpolation Technics
# arkriger - august 2023

install.packages("sf")
install.packages("gstat")
install.packages("stars")

library(sf)                # 'simple features' representations of spatial objects
library(gstat)             # geostatistics
library(stars) 

options(show.signif.stars=FALSE)

file = 'cptFlatsAquifer_watertable4.txt'

#-- import
cfaq <- read.csv(file, header = 1, sep = ',', dec = '.') #sep = '\t',

#-- look
head(cfaq ,3)

str(cfaq)

#- set as a spatial feature with xy coords an existing projection
cfaq.sf <- st_as_sf(cfaq, coords=c("long", "lat"), crs = 4326) #wgs84

st_crs(cfaq.sf)

#- transform to local crs
cfaq.sf <- st_transform(cfaq.sf, crs = 32734) #utm 34s
cfaq.sf$X <- st_coordinates(cfaq.sf)[, "X"]
cfaq.sf$Y <- st_coordinates(cfaq.sf)[, "Y"]
str(st_coordinates(cfaq.sf))

#- summarize the coordinates
summary(st_coordinates(cfaq.sf), digits = 3)

#- bounding box
st_bbox(cfaq.sf)

plot(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1],
     #plot(st_coordinates(cfaq.sf)[,1] ~ st_coordinates(cfaq.sf)[,2],
     pch=20, cex=0.4, col="blue", asp=1,
     xlab="Lo 19 E", ylab="Lo 19 N")
grid()
text(st_coordinates(cfaq.sf)[,1], st_coordinates(cfaq.sf)[,2],
     #st_coordinates(cfaq.sf)[,2], st_coordinates(cfaq.sf)[,1],
     #round(cfaq$waterLevel), adj=c(0.5,0.5))
     cfaq$name, adj=c(0.5,0.5))
#text(cfaqN, round(aq$z), adj=c(0.5,0.5))
title("Elevation of aquifer, m")

plot(#st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1],
  st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1],
  cex=0.8*cfaq$water_level/max(cfaq$waterLevel),
  col="blue", bg="red", pch=21, asp=1,
  xlab="Lo19 E", ylab="Lo19 N")
grid()
title("Elevation of aquifer, m")

plot(#st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1],
  st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1],
  pch=21,
  xlab="UTM19_E", ylab="UTM19_N",
  bg=sp::bpy.colors(length(cfaq$waterLevel))[rank(cfaq$waterLevel)],
  cex=0.8)#*cfaq$waterLevel/max(cfaq$waterLevel), asp=1)
grid()
title("Elevation of aquifer, m")


# Voronoi tesselation
pnts <- st_union(cfaq.sf)
#pnts
voronoi_grid <- st_voronoi(pnts)

#-- plot
plot(voronoi_grid, col = NA)
points(st_coordinates(cfaq.sf)[,1], st_coordinates(cfaq.sf)[,2],
       pch=21, 
       bg=sp::bpy.colors(length(cfaq$waterLevel))[rank(cfaq$waterLevel)],
       cex=0.4*cfaq$waterLevel/max(cfaq$waterLevel))
grid()
title("Elevation of aquifer, m.a.s.l.")

#- create grid
cfaq.bb <- st_bbox(cfaq.sf)
cfaq.bb[c("xmin","ymin")] <-   floor(cfaq.bb[c("xmin","ymin")]/1000)*1000
cfaq.bb[c("xmax","ymax")] <- ceiling(cfaq.bb[c("xmax","ymax")]/1000)*1000

#- 500m
gridkm <- st_as_stars(cfaq.bb, dx = 500)
gridkm

plot(gridkm); grid()

#-- idw @ 0.5, 1, 2.5, 10
idw_results <- list()
idp_values <- c(0.5, 1, 2.5, 10)
for (idp in idp_values) {
  idw_result <- idw(log(waterLevel) ~ 1, cfaq.sf, gridkm, idp = idp)
  #idw <- gridkm
  #idw <- idw_result
  idw_results[[as.character(idp)]] <- idw_result
}

# Plot IDW results with overlay of original points
num_plots <- length(idp_values)
# Arrange plots in 2 rows and 2 columns
par(mfrow = c(2, 2))  
# Set the size of the individual plots
plot_width <- 8
plot_height <- 8

# Set the margin and space between plots
margin_size <- 0.5
space_between_plots <- 0.3

# Calculate the total plot area
total_width <- plot_width * 2 + space_between_plots
total_height <- plot_height * 2 + space_between_plots

min.x <- floor(min(cfaq.sf$X)/1000)*1000
max.x <- ceiling(max(cfaq.sf$X)/1000)*1000

min.y <- floor(min(cfaq.sf$Y)/1000)*1000
max.y <- ceiling(max(cfaq.sf$Y)/1000)*1000

# Set par settings for plot layout
par(mfrow = c(2, 2), mar = c(0.6, 0.6, 0.8, 0.1),#margin_size, margin_size, margin_size, margin_size),
    oma = c(0, 0, 0, 0), mgp = c(0.5, 0.7, 0))

for (i in 1:num_plots) {
  idp <- idp_values[i]
  idw_result <- idw_results[[as.character(idp)]]
  title <- paste("IDW @ (IDP =", idp, ")", sep = " ")
  
  # Plot interpolated surface
  image(idw_result, main = title, col = rainbow(100),
        xlim = c(min.x, max.x), ylim = c(min.y, max.y),
        xlab = "", ylab = "")
  
  # Overlay original points
  points(cfaq.sf$X, cfaq.sf$Y, pch = 21,
         bg = adjustcolor("black", alpha.f = 0.2),
         col = "black",
         cex = 0.9, lwd = 0.5)
  
  contour(idw_result, add = TRUE, nlevels = 10, col = "black")
}

# Reset layout to default
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))


##--extra: with voronoi
# Plot IDW results with overlay of original points
num_plots <- length(idp_values)
par(mfrow = c(2, 2))  # Arrange plots in 2 rows and 2 columns
# Set the size of the individual plots
plot_width <- 9
plot_height <- 9

# Set the margin and space between plots
margin_size <- 0.5
space_between_plots <- 0.3

# Calculate the total plot area
total_width <- plot_width * 2 + space_between_plots
total_height <- plot_height * 2 + space_between_plots

# Set par settings for plot layout
par(mfrow = c(2, 2), mar = c(0.6, 0.6, 0.8, 0.1),#margin_size, margin_size, margin_size, margin_size),
    oma = c(0, 0, 0, 0), mgp = c(0.5, 0.7, 0))

for (i in 1:num_plots) {
  idp <- idp_values[i]
  idw_result <- idw_results[[as.character(idp)]]
  title <- paste("IDW @ (IDP =", idp, ")", sep = " ")
  
  # Plot interpolated surface
  image(idw_result, main = title, col = rainbow(100),
        xlim = c(min.x, max.x), ylim = c(min.y, max.y),
        xlab = "", ylab = "")
  
  # Overlay Voronoi polygons
  plot(voronoi_grid, add = TRUE, border = "black", col = NA, type="l", lty=2) #lwd = 1,
  
  # Overlay original points
  points(cfaq.sf$X, cfaq.sf$Y, pch = 21,
         bg = adjustcolor("black", alpha.f = 0.2),
         col = "black",
         cex = 0.9, lwd = 0.5)
  
  contour(idw_result, add = TRUE, nlevels = 10, col = "black")
}

# Reset layout to default
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))