## APG3040C-APG4013C July-Sept 2023
## arkriger

##-- create a /data/ folder on your C:/ and place the relevent files there. 
##-- work through the script block-by-block. 
##-- i.e.: highlight #1. 
##         Click Run. read the output. Answer the questions and follow the instructions; if any. Then #2. etc.

#-- you might need to install these three very useful packages
#1
install.packages(c("sf", "stars", "tidyverse"))

#2
library(sf)
library(tidyverse)
library(stars)

##-- WE'LL START WITH VECTORS

#3
#-- read the dataset
veg <- st_read("c:/data/Vegetation_Indigenous.shp")

#-- Q1: What is the CRS of the vector dataset?
#-- [delete this line and write your answer here]

#4
#-- We can have a more detailed look at the CRS
st_crs(veg)

#5
#-- look at the data
head(veg)

#-- Q2: On which Longitude is the dataset centered? (hint: its "Longitude of natural origin")
#-- [delete this line and write your answer here]

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
#-- Export the plot as an image and name it Rpolt02. Hand these in

