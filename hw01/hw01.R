## APG3040C-APG4013C July-Sept 2023
## arkriger

##-- create a /data/ folder on your C:/ and place the relevent files there. 
##-- work through the script block-by-block. 
##-- i.e.: highlight #1. 
##         Click Run. read the output. Answer the questions and follow the instructions; if any. Then #2. etc.

#-- you might need to install these two very useful packages
#1
install.packages(c("sf", "stars"))

#2
library(sf)

#3
veg <- st_read("c:/data/Vegetation_Indigenous.shp")

# What is the CRS of the vector dataset?
# [delete this line and write your answer here]


#4
# We can have a more detailed look at the CRS
st_crs(veg)

# On which Longitude is the dataset centered? (hint: its "Longitude of natural origin")
# [delete this line and write your answer here]