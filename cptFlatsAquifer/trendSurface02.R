# Advanced Spatial Data Analysis ~ Interpolation Technics
# arkriger - august 2023

library(sf) # 'simple features' representations of spatial objects
library(gstat) # geostatistics
library(units) # units of measure
library(terra) 

#-- import
cfaq <- read.csv(file, header = 1, sep = ',', dec = '.') 
#- set as a spatial feature with xy coords an existing projection
cfaq.sf <- st_as_sf(cfaq, coords=c("long", "lat"), crs = 4326) #wgs84
#- transform to local crs
cfaq.sf <- st_transform(cfaq.sf, crs = 32734) #utm 34s
# Extract x and y coordinates from the geometry column
cfaq$X <- st_coordinates(cfaq.sf)[, "X"]
cfaq$Y <- st_coordinates(cfaq.sf)[, "Y"]
head(cfaq ,3)

#head(model.matrix(~ st_coordinates(cfaq.sf)[,1] + st_coordinates(cfaq.sf)[,2],data=cfaq))
head(model.matrix(~X + Y, data=cfaq))

#model.ts1 <- lm(cfaq$waterLevel ~ st_coordinates(cfaq.sf)[,1] + st_coordinates(cfaq.sf)[,2], data=drop_units(cfaq))
model.ts1 <- lm(waterLevel ~ X + Y, data=drop_units(cfaq))
summary(model.ts1)

#- summarize residuals (lack of fit)
res.ts1 <- set_units(residuals(model.ts1), m); summary(res.ts1)

#- histogram
hist(res.ts1, breaks=16, main="Residuals from 1st-order trend", xlab="residual elevation (m)")

rug(res.ts1)
range(res.ts1)

max(abs(res.ts1))/median(cfaq$waterLevel)*100

#- diagnostic plot of linear model
par(mfrow=c(1,2))
plot(model.ts1, which=1:2)
par(mfrow=c(1,1))

#- residuals as postplot
plot(cfaq$Y ~ cfaq$X, 
     #st_coordinates(cfaq.sf)[,1], st_coordinates(cfaq.sf)[,2],
     cex=3*abs(res.ts1)/max(abs(res.ts1)),
     col=ifelse(res.ts1 > set_units(0, m), "green", "red"),
     xlab="X", ylab="Y",
     main="Residuals from 1st-order trend",
     sub="Positive: green; negative: red", asp=1)
grid()

cfaq.sf

##- 2nd-order
#head(model.matrix(~ st_coordinates(cfaq.sf)[,1] + st_coordinates(cfaq.sf)[,2] + I(st_coordinates(cfaq.sf)[,1]^2) + I(st_coordinates(cfaq.sf)[,2]^2)
#                  + I(st_coordinates(cfaq.sf)[,1]*st_coordinates(cfaq.sf)[,2]), data=cfaq))
head(model.matrix(~ X + Y + I(X^2) + I(Y^2) + I(X*Y), data=cfaq))

#- fit the 2nd order
#model.ts2 <- lm(cfaq$waterLevel ~ st_coordinates(cfaq.sf)[,1] + st_coordinates(cfaq.sf)[,2] + I(st_coordinates(cfaq.sf)[,2]^2) + I(st_coordinates(cfaq.sf)[,1]^2) + I(st_coordinates(cfaq.sf)[,2]*st_coordinates(cfaq.sf)[,1]), 
#                data=drop_units(cfaq))
model.ts2 <- lm(waterLevel ~ X + Y + I(X^2) + I(Y^2) + I(X*Y), data=drop_units(cfaq))

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
max(abs(res.ts2))/median(cfaq$waterLevel)

#- diagnostic plots
par(mfrow=c(1,2))
plot(model.ts2, which=1:2)
par(mfrow=c(1,1))

summary(sres.ts2 <- rstandard(model.ts2))

(ix <- which(sres.ts2 < -1))

(cbind(actual=cfaq[ix, "waterLevel"], fitted=fitted(model.ts2)[ix],
       residual=res.ts2[ix],
       std.res <- sres.ts2[ix]))

#- display residuals as postplot
plot(cfaq$Y ~ cfaq$X,
     #st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], 
     cex=3*abs(res.ts2)/max(abs(res.ts2)), 
     col=ifelse(res.ts2 > set_units(0, m), "green", "red"),
     xlab="E", ylab="N", asp=1,
     main="Residuals from 2nd-order trend",
     sub="Positive: green; Negative: red; Black dots: severe over-predictions")
points(cfaq[ix, "Y"] ~ cfaq[ix, "X"], 
       #st_coordinates(cfaq.sf)[ix, 'Y'] ~ st_coordinates(cfaq.sf)[ix , 'X'],
       pch=20)
text(cfaq[ix, "X"], cfaq[ix, "Y"],
     #st_coordinates(cfaq.sf)[ix, 'X'], st_coordinates(cfaq.sf)[ix ,'Y'],
     round(sres.ts2[ix], 2), pos=4)
grid()

#--  create grid
#(n.col <- length(seq.e <- seq(min.x <- floor(min(st_coordinates(cfaq.sf)[,1])/1000)*1000,
#                              max.x <- ceiling(max(st_coordinates(cfaq.sf)[,1])/1000)*1000, by=1000)))
(n.col <- length(seq.e <- seq(min.x <- floor(min(cfaq$X)/1000)*1000,
                              max.x <- ceiling(max(cfaq$X)/1000)*1000, by=1000)))
#(n.col <- length(seq.e <- seq(min.x <- floor(min(st_coordinates(cfaq.sf)[,2])/1000)*1000,
#                              max.x <- ceiling(max(st_coordinates(cfaq.sf)[,2])/1000)*1000, by=1000)))
(n.row <- length(seq.n <- seq(min.y <- floor(min(cfaq$Y)/1000)*1000,
                              max.y <- ceiling(max(cfaq$Y)/1000)*1000, by=1000)))

#we want a XXXXm grid
grid <- rast(nrows = n.row, ncols = n.col,
             xmin=min.x, xmax=max.x,
             ymin=min.y, ymax=max.y, crs = st_crs(cfaq.sf)$proj4string,
             resolution = 500, names="waterLevel")

values(grid) <- NA_real_

#class(grid)
#crs(grid)
#plot(grid); grid()

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

#-- add the best fit predictions to the grid
values(grid) <- pred.ts2[,"fit"]
summary(values(grid))

#- plot
plot(grid, col = rainbow(100), main = "OLS 2nd order predicted surface")
points(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], pch=21, #pch=16,
       #col = ifelse(res.ts2 < set_units(0, m), "red", "green"),
       #cex=2*abs(res.ts2)/max(abs(res.ts2)))
       bg = adjustcolor("black", alpha.f = 0.2),
       col = "black",
       cex = 0.9, lwd = 0.5)

#-- compare 1st-order
pred.ts1 <- predict.lm(model.ts1,
                       newdata = grid.df,
                       interval = "prediction", level = 0.95)
gridb <- grid
values(gridb) <- pred.ts1[,"fit"]

# Plot the first grid
plot(gridb, col = rainbow(100), main = "OLS 1st-order predicted surface")
points(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], pch=21,
       bg = adjustcolor("black", alpha.f = 0.2),
       col = "black",
       cex = 0.9, lwd = 0.5)

summary(pred.ts2)
#pred.ts2
summary(grid.df)
#grid.df

#-- add the three prediction fields (fit, lower, upper) to the data
grid.df[, 3:5] <- pred.ts2
names(grid.df)[3:5] <- c("ts2.fit", "ts2.lwr", "ts2.upr")
summary(grid.df)

#-- sumarize the uncertainty from the trend surface, as absolute differences between the upper and lower prediction limits
summary(grid.df$ts2.diff.range <- grid.df$ts2.upr - grid.df$ts2.lwr)
#-- the above as a percentage of the best fit value
summary(100*grid.df$ts2.diff.range/grid.df$ts2.fit)

#-- plot: prediction interval of the trend surface
grid.diff <- grid
values(grid.diff) <- grid.df$ts2.diff.range
plot(grid.diff, col=cm.colors(64),
     main="Range of 95% prediction interval, 2nd-order trend, OLS fit")
grid()
points(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], pch=16,
       col = "gray")