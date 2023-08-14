install.packages("sf")
install.packages("gstat")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("units")
install.packages("terra")

library(sf) # 'simple features' representations of spatial objects
library(gstat) # geostatistics
library(ggplot2) # Grammer of Graphics plots
library(gridExtra) # arrange multiple ggplot graphics on one figure
library(units) # units of measure
library(terra) 


setwd('C://Users/T0084076/Downloads/')
file = 'cptFlatsAquifer_watertable4.txt'

#-- import
cfaq <- read.csv(file, header = 1, sep = ',', dec = '.') 
#- set as a spatial feature with xy coords an existing projection
cfaq.sf <- st_as_sf(cfaq, coords=c("long", "lat"), crs = 4326) #wgs84
#- transform to local crs
cfaq.sf <- st_transform(cfaq.sf, crs = 32734) #utm 34s
cfaq.sf$X <- st_coordinates(cfaq.sf)[, "X"]
cfaq.sf$Y <- st_coordinates(cfaq.sf)[, "Y"]
head(cfaq ,3)

#head(model.matrix(~st_coordinates(cfaq.sf)[,1] + st_coordinates(cfaq.sf)[,2], 
#                  data=cfaq))
head(model.matrix(~X + Y, data=cfaq.sf))


#model.ts1 <- lm(cfaq$waterLevel ~ st_coordinates(cfaq.sf)[,1] + st_coordinates(cfaq.sf)[,2])#, data=drop_units(cfaq))
model.ts1 <- lm(waterLevel ~ X + Y, data=drop_units(cfaq.sf))
summary(model.ts1)

#- summarize residuals (lack of fit)
res.ts1 <- set_units(residuals(model.ts1), m); summary(res.ts1)

#- histogram
hist(res.ts1, breaks=16, main="Residuals from 1st-order trend", 
     xlab="residual elevation (m)")

rug(res.ts1)
range(res.ts1)

max(abs(res.ts1))/median(cfaq.sf$waterLevel)*100

#- diagnostic plot of linear model
par(mfrow=c(1,2))
plot(model.ts1, which=1:2)
par(mfrow=c(1,1))

#- residuals as postplot
plot(cfaq.sf$Y ~ cfaq.sf$X, 
     #st_coordinates(cfaq.sf)[,1], st_coordinates(cfaq.sf)[,2],
     cex=3*abs(res.ts1)/max(abs(res.ts1)),
     col=ifelse(res.ts1 > set_units(0, m), "green", "red"),
     xlab="X", ylab="Y",
     main="Residuals from 1st-order trend",
     sub="Positive: green; negative: red", asp=1)
grid()

cfaq.sf

#head(model.matrix(~st_coordinates(cfaq.sf)[,1] + st_coordinates(cfaq.sf)[,2] + I(st_coordinates(cfaq.sf)[,1]^2) + I(st_coordinates(cfaq.sf)[,2]^2)
#                  + I(st_coordinates(cfaq.sf)[,1]*st_coordinates(cfaq.sf)[,2]), data=cfaq))
head(model.matrix(~ X + Y + I(X^2) + I(Y^2) + I(X*Y), data=cfaq.sf))

#- fit the 2nd order
#model.ts2 <- lm(cfaq$waterLevel ~ st_coordinates(cfaq.sf)[,1] + st_coordinates(cfaq.sf)[,2] + I(st_coordinates(cfaq.sf)[,2]^2) + I(st_coordinates(cfaq.sf)[,1]^2) + I(st_coordinates(cfaq.sf)[,2]*st_coordinates(cfaq.sf)[,1]), 
#                data=drop_units(cfaq))
model.ts2 <- lm(waterLevel ~ X + Y + I(X^2) + I(Y^2) + I(X*Y), 
                data=drop_units(cfaq.sf))
summary(model.ts2)

#- compare the 1st and 2nd order models statistically
anova(model.ts1, model.ts2)

#- summary
res.ts2 <- set_units(residuals(model.ts2), m)
summary(res.ts2)

#- graphic summary
hist(res.ts2, breaks=16, main="Residuals from 2nd-order trend", 
     xlab="residual elevation (m)")
rug(res.ts2)
max(abs(res.ts2))/median(cfaq.sf$waterLevel)

#- diagnostic plots
par(mfrow=c(1,2))
plot(model.ts2, which=1:2)
par(mfrow=c(1,1))

summary(sres.ts2 <- rstandard(model.ts2))

(ix <- which(sres.ts2 < -1))

(cbind(actual=cfaq.sf[ix, "waterLevel"], fitted=fitted(model.ts2)[ix],
       residual=res.ts2[ix],
       std.res <- sres.ts2[ix]))

#- display residuals as postplot
plot(cfaq.sf$Y ~ cfaq.sf$X,
     #st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], 
     cex=3*abs(res.ts2)/max(abs(res.ts2)), 
     col=ifelse(res.ts2 > set_units(0, m), "green", "red"),
     xlab="E", ylab="N", asp=1,
     main="Residuals from 2nd-order trend",
     sub="Positive: green; negative: red; black dots: severe over-predictions")
points(cfaq.sf[ix, "Y"] ~ cfaq.sf[ix, "X"],        pch=20)
       #st_coordinates(cfaq.sf)[ix, 'X'], st_coordinates(cfaq.sf)[ix , 'Y'],
#plot(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1],pch=20, col="blue")#, add = TRUE)
text(cfaq.sf[ix, "X"], cfaq.sf[ix, "Y"],
     #st_coordinates(cfaq.sf)[ix, 'X'], st_coordinates(cfaq.sf)[ix ,'Y'],
     round(sres.ts2[ix], 2), pos=4)
grid()

#--  create 2km grid
#(n.col <- length(seq.e <- seq(min.x <- floor(min(st_coordinates(cfaq.sf)[,2])/1000)*1000,
#                              max.x <- ceiling(max(st_coordinates(cfaq.sf)[,2])/1000)*1000, by=1000)))
#(n.row <- length(seq.n <- seq(min.y <- floor(min(st_coordinates(cfaq.sf)[,1])/1000)*1000,
#                              max.y <- ceiling(max(st_coordinates(cfaq.sf)[,1])/1000)*1000, by=1000)))

(n.col <- length(seq.e <- seq(min.x <- floor(min(cfaq.sf$X)/1000)*1000,
                              max.x <- ceiling(max(cfaq.sf$X)/1000)*1000, by=1000)))

(n.row <- length(seq.n <- seq(min.y <- floor(min(cfaq.sf$Y)/1000)*1000,
                              max.y <- ceiling(max(cfaq.sf$Y)/1000)*1000, by=1000)))


#we want a 500m grid
grid <- rast(nrows = n.row, ncols = n.col,
                xmin=min.x, xmax=max.x,
                ymin=min.y, ymax=max.y, crs = st_crs(cfaq.sf)$proj4string,
                resolution = 500, names="waterLevel")

values(grid) <- NA_real_
class(grid)

crs(grid)

plot(grid); grid()


grid.df <- as.data.frame(grid, xy = TRUE, na.rm = FALSE)
names(grid.df)[1:2] <- c("X", "Y") # match the names of the point dataset
summary(grid.df)


pred.ts2 <- predict.lm(model.ts2,
                       newdata = grid.df,
                       interval = "prediction", level = 0.95)
summary(pred.ts2)


class(pred.ts2)

summary(values(grid))

summary(pred.ts2[,"fit"])

values(grid) <- pred.ts2[,"fit"]
summary(values(grid))


plot(grid, main = "OLS 2nd order predicted surface")
points(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], pch=16,
       col = ifelse(res.ts2 < set_units(0, m), "red", "green"),
       cex=2*abs(res.ts2)/max(abs(res.ts2)))


