rm(list=ls())
getwd()
#setwd("~/student")

# Example of how to install a package:
# install.packages("rgdal")
options(scipen = 999) # turn off scientific notation (so p-values are readable)
options(digits = 4) 

library(RColorBrewer)
library(classInt) 
library(sp)
library(rgdal) 
library(maptools)
library(spdep)
library(spgwr)
library(PerformanceAnalytics)

##############
### Read shapefile
##############
# Several packages to do this
# rgdal::readOGR
#maptools::readShapePoly

# Read data into R
#columbus <- readShapePoly(system.file("etc/shapes/columbus.shp", package="spdep")[1])
columbus <- readOGR(system.file("etc/shapes/columbus.shp", package="spdep")[1])


class(columbus) # Class of object
## [1] "SpatialPolygonsDataFrame"
## attr(,"package")
## [1] "sp"
slotNames(columbus) # Check the Components of the SpatialPolygonsDataFrame
## [1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"
# str(columbus)  # Full structure of the object 
crs(columbus)

# Change map projection if necessary
# readshape <- spTransform(readshape, CRS("+init=epsg:2154"))

# Example of reading shapefile and setting projection if working with your own data
#  projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84" 
# readshape <-readShapePoly("datashp.shp", proj4string=CRS(projection))

##-- or try this
data(columbus)
columbus_sp <- SpatialPointsDataFrame(columbus, coords = c("X", "Y"), proj4string = CRS("+proj=longlat +datum=WGS84"))

pacman::p_unload(graphics)
## The following packages are a base install and will not be unloaded:
## graphics
#Extract the data frame from your shapefile
columbusdata <- columbus@data

class(columbusdata) # Class of object
## [1] "data.frame"
names(columbusdata) # Names of variables
##  [1] "AREA"       "PERIMETER"  "COLUMBUS_"  "COLUMBUS_I" "POLYID"    
##  [6] "NEIG"       "HOVAL"      "INC"        "CRIME"      "OPEN"      
## [11] "PLUMB"      "DISCBD"     "X"          "Y"          "NSA"       
## [16] "NSB"        "EW"         "CP"         "THOUS"      "NEIGNO"
str(columbusdata)   # Structure of the object (gives you details of variable types)

#- subset
columbusdata_sub <- columbusdata[,c(7, 8, 9, 10, 11, 12)]
head(columbusdata_sub, 3)

# Spatial Heterogeneity
cor(columbusdata_sub) # Correlation 
# Correlation plot using PerformanceAnalytic package
chart.Correlation(columbusdata_sub, histogram=TRUE, pch=19)

# Plot the zones from the Columbus, Ohio data

plot(columbus,col='wheat') # Create a plot of columbus
# Add labels for each of the zones
text(coordinates(columbus),
     labels=as.character(columbus@data$POLYID),
     cex=0.6,font=2, col="darkred")
box(which='outer',lwd=2)

# Visualising a variable - Binary classification
plot(columbus, col = ifelse(columbus$CRIME > 40, "lightgrey", "red"))

# You could view your data using a more detailed classification
# Select Color scale - Create a Palette
pal = brewer.pal(7,"Greens") # Makes a 7-color spectral palette
display.brewer.pal(7, "Greens")  # This displays the colors

# Classifying Data using classIntervals() 
# Create class breaks 
brks.eq = classIntervals(columbus$CRIME, n = 7, style = "equal")
brks.qt = classIntervals(columbus$CRIME, n = 7, style = "quantile")
brks.jk = classIntervals(columbus$CRIME, n = 7, style = "jenks")
# Other style options "fixed", "sd", "pretty", "kmeans", "hclust", "bclust" and "fisher"

# Link the color pallette to the class breaks (categories) using
# findColours(CATEGORIES,PALETTE)
brks.eqcol = findColours(brks.eq, pal)
brks.qtcol = findColours(brks.qt, pal)
brks.jkcol = findColours(brks.jk, pal)

# Plot with Equal Breaks
plot(columbus,  col=brks.qtcol, border="black")
legend("topleft",leglabs(round(brks.eq$brks,digits=2)), fill=pal, cex=0.8, bty="n")
title(main="Columbus OH: Crime per 1000 households, 1980  \n (Equal breaks)")

# Plot with Quantile Breaks
plot(columbus,  col=brks.qtcol, border="black")
legend("topleft",leglabs(round(brks.qt$brks,digits=2)), fill=pal, cex=0.8, bty="n")
title(main="Columbus OH: Crime per 1000 households, 1980  \n (Quantile breaks)")
# Plot with Jenks Breaks
plot(columbus,  col=brks.jkcol, border="black")
legend("topleft",leglabs(round(brks.jk$brks,digits=2)), fill=pal, cex=0.8, bty="n")
title(main="Columbus OH: Crime per 1000 households, 1980  \n (Jenks breaks)", cex=0.5)

######
# Constructing weight matrix from shapefile 
# To deduce neighbourhood structure from spatial polygons by poly2nb
######

columbus.nb = poly2nb(columbus, queen = T)  # Queen's case is the default. To change to Rook's case, set "queen = F i.e  columbus.nb = poly2nb(columbus, queen = F)

#row standardize the weight matrix
columbus.wts = nb2listw(columbus.nb, style="W") 
m = length(columbus$CRIME)
s = Szero(columbus.wts)

# Explore the weight matrix
summary(columbus.wts)  
# coordinates function extracts coordinates of the shapefile
columbuscoords <- coordinates(columbus)  
plot(columbus)
plot(columbus.wts, columbuscoords, pch=19, cex=0.6, add=TRUE)

moran(columbus$CRIME, columbus.wts, n = m, S0 = s)

moran.test(columbus$CRIME, columbus.wts)
## 
##  Moran I test under randomisation
## 
## data:  columbus$CRIME  
## weights: columbus.wts  
## 
## Moran I statistic standard deviate = 5.6, p-value = 0.00000001
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##          0.500189         -0.020833          0.008689
# Moran Scatterplot
columbusmp <- moran.plot(columbus$CRIME, columbus.wts, 
                         labels = as.character(columbus.wts$POLYID), 
                         xlab = "CRIME", ylab = "Lag of CRIME")

columbus_cor5 <- sp.correlogram(columbus.nb, columbus$CRIME, order = 5, method = "I")
columbus_cor5 

plot(columbus_cor5, main = "Correlelogram of CRIME")

### Local Moran's I

columbus_locm <- localmoran(columbus$CRIME, columbus.wts)

CRIME <- scale(columbus$CRIME)
columbus$lag_sCRIME <- lag.listw(columbus.wts, CRIME)


# Identify the Moran plot quadrant for each observation
columbus$quad_sig <- NA 
columbus[(columbus$CRIME >= 0 & columbus$lag_sCRIME >= 0) & (columbus_locm[, 5] <= 0.05), 
         "quad_sig"] <- 1
columbus[(columbus$CRIME <= 0 & columbus$lag_sCRIME <= 0) & (columbus_locm[, 5] <= 0.05), 
         "quad_sig"] <- 2
columbus[(columbus$CRIME >= 0 & columbus$lag_sCRIME <= 0) & (columbus_locm[, 5] <= 0.05), 
         "quad_sig"] <- 3
columbus[(columbus$CRIME <= 0 & columbus$lag_sCRIME >= 0) & (columbus_locm[, 5] <= 0.05), 
         "quad_sig"] <- 4
columbus[(columbus_locm[, 5] > 0.05), "quad_sig"] <- 5

#Plot Results
breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(columbus$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(columbus, col = colors[np])  
mtext("Local Moran's I\n% Crime in Columbus ", cex = 0.7, side = 3, 
      line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n",cex = 0.7)

# OLS Regression to predict CRIME using INC and HOVAL as independent variables
reg1 <- lm( columbusdata$CRIME ~ columbusdata$INC + columbusdata$HOVAL )

# Moran's I on the residuals 
moran.test(reg1$residuals, columbus.wts, alternative="two.sided")

# Plot the residuals
res <- residuals(reg1)
classes_sd <- classIntervals(res, n=4, style = "sd", rtimes = 1, dataPrecision = 3)
res.palette <- colorRampPalette(c("blue","white","red"), space = "rgb")
pal <- res.palette(4)
cols <- findColours(classes_sd, pal)
par(mar=rep(0, 4))
plot(columbus,col=cols, main="Residuals from OLS Model", border="grey")
legend("bottomright",cex=0.7,fill=attr(cols,"palette"),bty="n",
       legend=names(attr(cols, "table")),title="Residuals from OLS Model", ncol=4)

##-- stop here

library(spgwr)
data(columbus)
names(columbus)
## [1] "crime"   "income"  "housing" "x"       "y"
# GWR with Gauss
crime.bw <- gwr.sel(columbus$CRIME ~ columbus$INC + columbus$HOVAL, 
                    data=columbus,
                    coords=cbind(columbus$X, columbus$Y))

#Next fit a geographic regression. This is done with the gwr() function.
# Brackets around the code prints the results
(crime.gauss <- gwr(columbus$CRIME ~ columbus$INC + columbus$HOVAL,
                    data=columbus,
                    coords=cbind(columbus$X, columbus$Y),
                    bandwidth=crime.bw, hatmatrix=TRUE)
  
crime.gauss$SDF$columbus.INC
  
#Paste your results on your report.
# Distribution of betas
#d <- cbind(crime.gauss$SDF$INC, crime.gauss$SDF$HOVAL)
d <- cbind(crime.gauss$columbus.INC, crime.gauss$columbus.HOVAL)


par(mar=c(3,4,2,2))
boxplot(d, xaxt="n",yaxt="n", pars=list(boxwex=0.3))
#axis(1, at=1:2,label=c("Income", "Housing"))
#axis(2, at=seq(-4,2,.2),las=1)
#ylim = c(0,300)
#abline(h=0,lty="4343",col="#7E7E7E")
#mtext("Beta i",2,line=3)

boxplot(crime.gauss$columbus$SDF$INC)#, main = "Boxplot of Cadmium (ppm)")
boxplot(log(crime.gauss$columbus$SDF$HOVAL))#, main = "Boxplot of Log-Cadmium (ppm)")

 
 ###-- try this code snipet
 # Load required library
library(spgwr)

# Load dataset
data(columbus)

# Perform GWR with Gauss
columbus.bw <- gwr.sel(CRIME ~ INCOME + HOVAL, data = columbus, coords = cbind(columbusY))

# Extract the betas for INCOME and HOVAL
betas_df <- data.frame(INCOME = columbus.bw$SDF$INCOME, HOVAL = columbus.bw$SDF$HOVAL)

# Load ggplot2 library
library(ggplot2)

# Create side-by-side box plots
ggplot(melt(betas_df), aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Distribution of INCOME and HOVAL Betas",
       x = "Variable", y = "Beta Value")

 ##-- or this with more modern libraries
 # Install and load required packages
install.packages("sf")
install.packages("spdep") # for creating neighbors list, if not already installed
install.packages("ggplot2")

library(sf)
library(spdep)
library(ggplot2)

# Load the data (assuming columbus data is in the correct format)
data(columbus)

# Convert the columbus data.frame to an sf object
columbus_sf <- st_as_sf(columbus, coords = c("X", "Y"))

# Generate neighborhood list for spatial weights
nb <- poly2nb(columbus_sf)

# GWR with Gauss
columbus_gwr <- gwr.sel(CRIME ~ INCOME + HOVAL, data = columbus_sf, 
                        kernel = "gaussian", adaptive = TRUE, gweight = nb)

# Extract coefficients and create a data.frame
gwr_coeffs <- as.data.frame(coef(columbus_gwr))

# Create side-by-side box plots
ggplot(gwr_coeffs, aes(x = factor(Variable), y = Estimate)) +
  geom_boxplot() +
  labs(x = "Variables", y = "Coefficients") +
  ggtitle("Distribution of INCOME and HOVAL Betas") +
  theme_minimal()







