


options(prompt="> ", continue="+ ", digits=3, width=70, show.signif.stars=T)
rm(list=ls()) 

# Install necessary packages: You only need to run this part once
install.packages(c("sp", "gstat", "lattice", "automap"))

# Load the required libraries
#_________________________________________________________
library(sp)
library(gstat)
library(lattice)
library(automap)

# get help for the gstat package:
#______________
help(package="gstat")

# Check for example data sets in the sp package
#______________

data(package="gstat")


# Description of the data from the package documentation
?meuse

# Load the data into R and explore the data
data(meuse)
class(meuse)
str(meuse)

# The gstat package assumes that data are projected, 
# i.e. they should not be provided as lattitude/longitude # and have to tell it which vectors are the corrdinates

# The command below specifies the corrdinates and changes the dataframe 
# to a "SpatialPointDataFrame", which is necessary for variogram modelling

coordinates(meuse) <- ~ x + y    
proj4string(meuse) <- CRS("+init=epsg:28992")

# Explore the projected data
class(meuse)
str(meuse)

# Read the interpolation grid data (set of regularly-spaced points that covers 
# the study area and specify locations were predictions will be done)
data(meuse.grid)
class(meuse.grid)


coordinates(meuse.grid) <-  ~ x + y   
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
class(meuse.grid)

# Plot the interpolation grid to see what you have
plot(meuse.grid, main = "Meuse Interpolation Grid with Control Points")
points(meuse,pch=10)
# Promote the grid data 
gridded(meuse.grid) <- TRUE
class(meuse.grid)

# Read in the meuse river outline

data(meuse.riv)
meuse.sr = SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)),"meuse.riv")))
meuse.lt = list("sp.polygons", meuse.sr, fill = "grey")

# Summary Statistics
summary(meuse$cadmium) 
summary(log(meuse$cadmium))

# Steam and leaf plot
stem(meuse$cadmium) 
stem(log(meuse$cadmium))

# Histogram and Q-Q Plots
par(mfrow=c(3,2)) # 6 figures per page arranged in 3 rows and 2 columns
hist(meuse$cadmium, n=20, main = "Histogram of Cadmium (ppm)")
hist(log(meuse$cadmium), n=20, main = "Histogram of Log-Cadmium  (ppm)")

boxplot(meuse$cadmium, main = "Boxplot of Cadmium (ppm)")
boxplot(log(meuse$cadmium), main = "Boxplot of Log-Cadmium (ppm)")

qqnorm(meuse$cadmium,  main = "Q-Q Plot of Cadmium  (ppm)")
qqnorm(log(meuse$cadmium),  main = "Q-Q Plot of Log-Cadmium (ppm)")

par(mfrow=c(1,1)) # Reset to default ploting of 1 figure per page

# Shapiro-Wilk normality test

shapiro.test(meuse$cadmium)
shapiro.test(log(meuse$cadmium))



# Obtain a bubble plot of the cadmium data with the dots scaled to concentration
bubble(meuse, "cadmium", col = "green" , main = "cadmium concentrations (ppm)")


# Obtain a dot plot and add some context to the plot
sp.theme(TRUE)
spplot(meuse, "cadmium", key.space = "right", col.regions=bpy.colors(),
       main = "Cadmium concentrations (ppm)",
       scales = list(draw = TRUE),# Add a reference by showing the coordinate system
       sp.layout= list("sp.polygons", meuse.sr, fill = "lightblue")
       # Add geographic reference (Meuse river boundaries)
)


#_________________________________________________________
### To produce  Fig. like 8.1 in Ch 8 of Bivand et al. (2008)
#_________________________________________________________

# Map of distance to river: 
meuse.grid$sqrtdist = sqrt(meuse.grid$dist)
spplot(meuse.grid["sqrtdist"], col.regions = bpy.colors() , 
       sp.layout = list("sp.points", meuse, col = 3, cex=.5), 
       main = "Distance to river")

# Cadmium (ppm) vs. distance to river
xyplot(log(cadmium)~sqrt(dist), as.data.frame(meuse),
       main="Scatterplot of Cadmium vs. distance")


# Fit the regression model
cadmium.lm <- lm(log(cadmium)~sqrt(dist), meuse)

# Get a summary of the regression model
summary(cadmium.lm)

# Get diagnostic plots
layout(matrix(1:4, ncol=2))
plot(cadmium.lm, add.smooth = FALSE)
layout(1)

# Get Predicted Values and Standard Error of Fit for all locations on the grid
meuse.grid$lzn.fit <- predict(cadmium.lm, meuse.grid)
meuse.grid$se.fit <- predict(cadmium.lm, meuse.grid, se.fit=TRUE)$se.fit

# Plot the predicted values
spplot(meuse.grid, "lzn.fit", sp.layout = meuse.lt,
       main = "Log(Cadmium) - ppm: Regression Interpolation \n Predicted values")

# Plot the Standard Error of fit
spplot(meuse.grid, "se.fit", sp.layout = meuse.lt,
       main = "Log(Cadmium) - ppm: Regression Interpolation \n Standard Error of fit")


#_________________________________________________________
#  # Trend surface up to degree 3
# Note: tr1, tr2 and tr3 are names for Trend surface of order 1, 2 and 3 respectively
#_________________________________________________________

meuse.grid$tr1 = krige(log(cadmium) ~ 1, meuse, meuse.grid, degree = 1)$var1.pred
meuse.grid$tr2 = krige(log(cadmium) ~ 1, meuse, meuse.grid, degree = 2)$var1.pred
meuse.grid$tr3 = krige(log(cadmium) ~ 1, meuse, meuse.grid, degree = 3)$var1.pred
spplot(meuse.grid, c("tr1", "tr2", "tr3"), sp.layout = meuse.lt,
       main = "Log(cadmium) - ppm \n Trend Surface Interpolation")


# Run the IDW interpolation
# Check the effect of power (p) on the IDW interpolation 
# by changing the value of "idp"

meuse.grid$idwp05 = idw(log(cadmium)  ~ 1, meuse, meuse.grid, idp = 0.5)$var1.pred
meuse.grid$idwp1 = idw(log(cadmium)  ~ 1, meuse, meuse.grid, idp = 1)$var1.pred
meuse.grid$idwp25 = idw(log(cadmium)  ~ 1, meuse, meuse.grid, idp = 2.5)$var1.pred
meuse.grid$idwp5 = idw(log(cadmium)  ~ 1, meuse, meuse.grid, idp = 5)$var1.pred
meuse.grid$idwp10 = idw(log(cadmium)  ~ 1, meuse, meuse.grid, idp = 10)$var1.pred
# Plot the outputs and the control points 
spplot(meuse.grid, c("idwp05", "idwp1", "idwp25","idwp5", "idwp10"), 
       sp.layout = list("sp.points", meuse, col = 3, cex=.5),   
       main = "Log(Cadmium) - ppm , IDW Interpolation ")

###################################################
### 8.4 Estimating Spatial Correlation: The Variogram - Bivand et al. (2008)
### 8.4.1 Exploratory Variogram Analysis
###################################################

### h-scatterplots/ lagged scatterplots
hscat(log(cadmium)~1,meuse,(0:9)*100)

### Variogram cloud
plot(variogram(log(cadmium) ~ 1, meuse, cloud = TRUE))


###################################################
### Sample variogram (binned variogram) plot of (8.4)
###################################################
plot(variogram(log(cadmium) ~ 1, meuse))

###################################################
### Variograms in four different angles
###################################################
plot(variogram(log(cadmium) ~ 1, meuse, alpha = c(0, 45, 90, 135)))

###################################################
### Override the default cutoff and interval width values ###  See Bivand et al. (2008)
###################################################
plot(variogram(log(cadmium) ~ 1, meuse, cutoff = 1000, width = 50))

###################################################
### Specifying interval for the distance vector  - See Bivand et al. (2008)
###################################################
variogram(log(cadmium) ~ 1, meuse, boundaries = c(0,50,100,seq(250,1500,250)))

######################################### ##########
### Variogram in Fig. 8.6 - Bivand et al. (2008)
###################################################
v <- variogram(log(cadmium) ~ 1, meuse)
plot(v)

###################################################
### Initial values for the variogram fit 
###################################################
v.fit <- fit.variogram(v, vgm(1, "Sph", 800, 1))

###################################################
### Partial fitting of variogram coefficients : PAGE 204 - Bivand et al. (2008)
###################################################
fit.variogram(v, vgm(1, "Sph", 800, 0.06), fit.sills = c(FALSE, TRUE))


###################################################
###  REML (restricted maximum likelihood) fitting : PAGE 205 - Bivand et al. (2008)
###################################################
fit.variogram.reml(log(cadmium)~1, meuse, model=vgm(0.6, "Sph", 800, 0.06))


###################################################
### 8.4.4 Anisotropy  - Bivand et al. (2008)
###################################################
v.dir <- variogram(log(cadmium)~1,meuse,alpha=(0:3)*45) 
v.anis <- vgm(.6, "Sph", 1600, .05, anis=c(45, 0.3))


###################################################
### Fig. 8.7 - Bivand et al. (2008)
###################################################
plot(v.dir, v.anis)

###################################################
### variogram map - Bivand et al. (2008)
###################################################
plot(variogram(log(cadmium)~1,meuse, map=TRUE, cutoff=1000, width=100))

###################################################
### 8.5.1 Simple Kriging and Ordinary Kriging  - Bivand et al. (2008)
###################################################

lz.sk <- krige(log(cadmium)~1, meuse, meuse.grid, v.fit, beta = 5.9)
lz.ok <- krige(log(cadmium)~1, meuse, meuse.grid, v.fit)


