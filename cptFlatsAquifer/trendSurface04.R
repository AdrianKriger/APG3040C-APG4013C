# Advanced Spatial Data Analysis ~ Interpolation Technics
# arkriger - august 2023

library(sf) # 'simple features' representations of spatial objects
library(gstat) # geostatistics
library(ggplot2) # geostatistics
library(gridExtra) 
library(units) # units of measure
library(terra) 
library(mgcv) 
library(fields)

setwd('C://Users/T0084076/Downloads/')

file = 'cptFlatsAquifer_watertable4.txt'

#-- import
cfaq <- read.csv(file, header = 1, sep = ',', dec = '.') 
#- set as a spatial feature with xy coords an existing projection
cfaq.sf <- st_as_sf(cfaq, coords=c("long", "lat"), crs = 4326) #wgs84
#- transform to local crs
cfaq.sf <- st_transform(cfaq.sf, crs = 32734) #utm 34s
# Extract x and y coordinates from the geometry column
cfaq$X <- st_coordinates(cfaq.sf)[, "X"]
cfaq$Y <- st_coordinates(cfaq.sf)[, "Y"]

##--previous
model.ts2 <- lm(waterLevel ~ X + Y + I(X^2) + I(Y^2) + I(X*Y), data=drop_units(cfaq))

#--  create grid
#(n.col <- length(seq.e <- seq(min.x <- floor(min(st_coordinates(cfaq.sf)[,1])/1000)*1000,
#                              max.x <- ceiling(max(st_coordinates(cfaq.sf)[,1])/1000)*1000, by=1000)))
#(n.col <- length(seq.e <- seq(min.x <- floor(min(st_coordinates(cfaq.sf)[,2])/1000)*1000,
#                              max.x <- ceiling(max(st_coordinates(cfaq.sf)[,2])/1000)*1000, by=1000)))
(n.col <- length(seq.e <- seq(min.x <- floor(min(cfaq$X)/1000)*1000,
                              max.x <- ceiling(max(cfaq$X)/1000)*1000, by=1000)))
(n.row <- length(seq.n <- seq(min.y <- floor(min(cfaq$Y)/1000)*1000,
                              max.y <- ceiling(max(cfaq$Y)/1000)*1000, by=1000)))

#we want a XXXXm grid
grid <- rast(nrows = n.row, ncols = n.col,
             xmin=min.x, xmax=max.x,
             ymin=min.y, ymax=max.y, crs = st_crs(cfaq.sf)$proj4string,
             resolution = 500, names="waterLevel")

values(grid) <- NA_real_

grid.df <- as.data.frame(grid, xy = TRUE, na.rm = FALSE)
names(grid.df)[1:2] <- c("X", "Y") # match the names of the point dataset
summary(grid.df)

#--
pred.ts2 <- predict.lm(model.ts2,
                       newdata = grid.df,
                       interval = "prediction", level = 0.95)

values(grid) <- pred.ts2[,"fit"]
#- residuals
res.ts2 <- set_units(residuals(model.ts2), m)

#-- add the three prediction fields (fit, lower, upper) to the data
grid.df[, 3:5] <- pred.ts2
names(grid.df)[3:5] <- c("ts2.fit", "ts2.lwr", "ts2.upr")

#-- gam
model.gam <- gam(waterLevel ~ s(X, Y, k = 29), data=drop_units(cfaq))
#-- predict aquifer elevation, standard error of prediction using the fitted GAM
tmp <- predict.gam(object=model.gam,
                   newdata=grid.df,
                   se.fit=TRUE)

grid.df$pred.gam <- as.numeric(tmp$fit)
grid.df$pred.gam.se <- as.numeric(tmp$se.fit)

grid.gam <- grid
values(grid.gam) <- grid.df$pred.gam

##--end previous

#- gls with coefficient
model.ts2.gls <- gls(
  model = waterLevel ~ Y + X + I(Y^2) + I(X^2) + I(X * Y),
  data = drop_units(cfaq),
  method = "ML",
  correlation = corExp(form = ~X + Y,
                       nugget = FALSE,
                       value = 10000) # initial value of the range parameter
)
#model.ts2.gls
class(model.ts2.gls)

#- generic coef method extracts coefficients from model objects
coef(model.ts2.gls) - coef(model.ts2)

round(100*(coef(model.ts2.gls) - coef(model.ts2))/coef(model.ts2),1)

#- generic intervals method has a specific method for a fitted GLS model
intervals(model.ts2.gls, level=0.90)


#- residuals of surface and compare with OLS
summary(res.ts2.gls <- residuals(model.ts2.gls))

summary(res.ts2)

#- predict.gls function (of the nlme package) specific method for a fitted GLS model
pred.ts2.gls <- predict(model.ts2.gls, newdata=grid.df)
summary(pred.ts2.gls)

#- SpatRast of predictions 
grid.gls <- grid
values(grid.gls) <- pred.ts2.gls
summary(values(grid.gls))

#- plot
plot(grid.gls, col = rainbow(100), main = "GLS 2nd order predicted surface")
points(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], 
       #pch=16,
       #col = ifelse((res.ts2.gls < 0), "red", "green"),
       #cex=2*abs(res.ts2.gls)/max(abs(res.ts2.gls))
       col= ifelse((res.ts2.gls < 0), "red", "green"),
       cex=2*abs(res.ts2.gls)/max(abs(res.ts2.gls)),
       pch=21,
       bg = adjustcolor("black", alpha.f = 0.1),
       #col = "black",
       #cex = 0.9, 
       lwd = 0.5)

#- difference between the OLS and GLS trendsurfaces
grid.gls.ols.diff <- (grid.gls - grid)
plot(grid.gls.ols.diff, col = topo.colors(64),
     main = "GLS - OLS 2nd order trend surfaces, m")


#- residuals from the GLS trend surface and as a postplot
summary(res.ts2.gls)

#- plot
plot(cfaq$Y ~ cfaq$X, cex=3*abs(res.ts2.gls)/max(abs(res.ts2.gls)),
     col=ifelse(res.ts2.gls > 0, "green", "red"),
     xlab="X", ylab="Y",
     main="Residuals from 2nd-order trend, GLS fit",
     sub="Positive: green; negative: red", asp=1)
##--  ??local spatial correlation?? --##

#-  empirical variogram model residuals from the GLS trend surface model

#- extract the residuals into the point observations object
cfaq.sf$res.ts2.gls <- residuals(model.ts2.gls)
vr.gls <- variogram(res.ts2.gls ~ 1, loc=cfaq.sf, cutoff = 50000)
#- plot empirical variogram
plot(vr.gls, plot.numbers=T,
     main="Residuals from second-order GLS trend", cex=1,
     xlab = "separation [m]", ylab = "semivariance [m^2]")

vr.gls.m <- vgm(psill=200, model="Mat", range=15000)#, nugget=25)
#vr.gls.m <- vgm(model="Exp")#, range=20000)#, nugget=25)
(vr.gls.m.f <- fit.variogram(vr.gls, vr.gls.m))

plot(vr.gls, model=vr.gls.m.f, plot.numbers=T,
     xlab = "separation [m]", ylab = "semivariance [m^2]")

print(vr.gls.m.f)

intervals(model.ts2.gls)$corStruct[2]


grid.sf <- st_as_sf(grid.df, coords = c("X", "Y"))
st_crs(grid.sf) <- st_crs(grid)
kr <- krige(res.ts2.gls ~ 1,
            loc = cfaq.sf,
            newdata = grid.sf,
            model=vr.gls.m.f)

summary(kr)

class(kr)

#- ok residuals
plot(kr["var1.pred"], pch=15, nbreaks=24, main="Residuals from GLS trend, m")

#- prediction standard deviations
kr$var1.sd <- sqrt(kr$var1.var)
summary(kr)

plot(kr["var1.sd"], pch=15, nbreaks=24, pal = heat.colors,
     main="Standard errors of residuals from GLS trend, m")

#-- gls-regression kriging
grid.df$kr <- kr$var1.pred
grid.df$pred.ts2.gls <- pred.ts2.gls
grid.df$rkgls <- grid.df$pred.ts2.gls + grid.df$kr
summary(grid.df)

grid.rkgls <- grid
values(grid.rkgls) <- grid.df$rkgls

plot(grid.rkgls, col = rainbow(100),
     main="GLS-RK prediction, aquifer elevation, m.a.s.l.")


summary(grid.rkgls.gam <- (grid.rkgls - grid.gam))

plot(grid.rkgls.gam, main ="RK-GLS - GAM fits, difference, m",
     col = topo.colors(64))

#-- uk

#- what do we have thusfar?
names(grid.sf)

str(grid.sf$geometry)

#-- 
names(cfaq.sf)

#-- 
str(st_coordinates(grid.sf))

#- variables to grid
grid.sf$X <- st_coordinates(grid.sf)[ , "X"]
grid.sf$Y <- st_coordinates(grid.sf)[ , "Y"]
names(grid.sf)

#- empirical variogram
vr <- variogram(zm ~ E + N + I(E^2) + I(N^2) + I(E*N), locations = aq.sf, cutoff = 50000)
#- plot
plot(vr, plot.numbers = TRUE, main = "Residuals from 2nd-order OLS trend surface", xlab = "separation (m)", ylab = "semivariance (m^2)")

#- variogram model 
#(vr.m.f <- fit.variogram(vr, vgm(35, "Exp", 22000/3, 0)))
vr.m.f <- fit.variogram(vr, vgm(psill=200, model="Mat", range=15000)#, nugget=25)

#- plot old-variogram-model
plot(vr, plot.numbers=TRUE, xlab="separation (km)", ylab="semivariance (m^2)", model=vr.m.f, main="Fitted variogram model, residuals from 2nd-order OLS trend surface")

#- uk
k.uk <- krige(zm ~ E + N + I(E^2) + I(N^2) + I(E*N), locations = aq.sf, newdata = grid1km.sf, model=vr.m.f)

#- plot uk predictors
plot(k.uk["var1.pred"], pch=15, col = rainbow(100), nbreaks=24, main="UK predictions, m")

#- uk-prediction std-dev.
k.uk$var1.sd <- sqrt(k.uk$var1.var)
summary(k.uk)

#- plot std-dev
plot(k.uk["var1.sd"], pch=15, nbreaks=24, pal = heat.colors, main="Standard errors of UK predictions, m")

#- summary uk
summary(k.uk$var1.sd)

#- summary gls-rk
summary(kr$var1.sd)

#- diff. uk and gls-rk summary
grid.uk <- grid
values(grid.uk) <- k.uk$var1.pred
summary(grid.diff.uk.rkgls <- (grid.uk - grid.rkgls))

#- diff. uk and gls-rk hist.
hist(grid.diff.uk.rkgls, main = "UK - GLS-RK prediction differences", freq = FALSE, xlab = "difference, UK - GLS-RK")

#- plot uk minus gls-rk diff surface
plot(grid1km.diff.uk.rkgls, sub="UK - GLS-RK predictions", main="difference, m", xlab="East", ylab="North", col = topo.colors(64))