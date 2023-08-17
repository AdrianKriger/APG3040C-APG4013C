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

model.ts2 <- lm(waterLevel ~ X + Y + I(X^2) + I(Y^2) + I(X*Y), data=drop_units(cfaq))

head(cfaq ,3)

##- plot
g1 <- ggplot(drop_units(cfaq), aes(x=Y, y=waterLevel)) +
  geom_point() +
  geom_smooth(method="loess") +
  labs(y = "elevation [m]")
g2 <- ggplot(drop_units(cfaq), aes(x=X, y=waterLevel)) +
  geom_point() +
  geom_smooth(method="loess") +
  labs(y = "elevation [m]")
grid.arrange(g1, g2, ncol = 2)

##-- restricted through the quantity of observations in our dataset
model.gam <- gam(waterLevel ~ s(X, Y, k = 29), data=drop_units(cfaq))
summary(model.gam)

resid.gam <- residuals(model.gam)
summary(resid.gam)

summary(residuals(model.ts2))

#breaks <- seq(cfaq$waterLevel, cfaq$waterLevel, length.out = 7); print(breaks)

par(mfrow=c(1,2))
hist(resid.gam)#, xlim=c(-5, 5))#,
     #breaks=c(min(cfaq$waterLevel), max(cfaq$waterLevel), by=2)), main="Residuals from GAM")
rug(residuals(model.gam))
hist(residuals(model.ts2))#, xlim=c(-30, 30))#,
     #breaks=c(min(cfaq$waterLevel), max(cfaq$waterLevel), by=2), main="Residuals from 2nd-order OLS trend")
rug(residuals(model.ts2))
par(mfrow=c(1,1))

#summary(residuals(model.ts2))


#-- plot
g1 <- ggplot(data=as.data.frame(resid.gam), aes(resid.gam)) +
  geom_histogram(#breaks=seq(-20,20,by=2),
                 fill="lightblue", color="black", alpha=0.9) +
  geom_rug() +
  labs(title = "Residuals from GAM",
       x = expression(paste(Delta, m)))
g2 <- ggplot(data=as.data.frame(residuals(model.ts2)), aes(residuals(model.ts2))) +
  geom_histogram(#breaks=seq(-20,20,by=2),
                 fill="lightgreen", color="darkblue", alpha=0.9) +
  geom_rug() +
  labs(title = "Residuals from 2nd order polynomial trend surface",
       x = expression(paste(Delta, m)))
grid.arrange(g1, g2, nrow=1)


#-- residuals as a bubble plot. show the size of the residuals by the size of a point, and the polarity (positive vs. negative)
cfaq$resid.gam <- resid.gam
ggplot(data = drop_units(cfaq)) +
  aes(x=X, y=Y, size = abs(resid.gam),
      col = ifelse((resid.gam < 0), "red", "green")) +
  geom_point(alpha=0.7) +
  scale_size_continuous(name = expression(paste(plain("residual ["),
                                                reDelta, m, plain("]"))),
                        breaks=seq(0,12, by=2)) +
  scale_color_manual(name = "polarity",
                     labels = c("negative","positive"),
                     values = c("red","green","blue"))

#model.gam

plot.gam(model.gam, rug = FALSE, se = FALSE, select=2,
         scheme=1, theta=45, phi=30)

vis.gam(model.gam, plot.type="persp", color="terrain",
        theta=160, zlab="elevation", se=1.96)

(rmse.gam <- sqrt(sum(residuals(model.gam)^2)/length(residuals(model.gam))))

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

#--
pred.ts2 <- predict.lm(model.ts2,
                       newdata = grid.df,
                       interval = "prediction", level = 0.95)

#-- add the three prediction fields (fit, lower, upper) to the data
grid.df[, 3:5] <- pred.ts2
names(grid.df)[3:5] <- c("ts2.fit", "ts2.lwr", "ts2.upr")
##-- 

#-- predict aquifer elevation, standard error of prediction using the fitted GAM
tmp <- predict.gam(object=model.gam,
                   newdata=grid.df,
                   se.fit=TRUE)
summary(tmp$fit)

summary(tmp$se.fit)

str(tmp)

grid.df$pred.gam <- as.numeric(tmp$fit)
grid.df$pred.gam.se <- as.numeric(tmp$se.fit)

#grid.df$pred.gam

grid.gam <- grid
values(grid.gam) <- grid.df$pred.gam
plot(grid.gam, col = rainbow(100), main="GAM prediction"); grid()
points(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], 
#       pch=16,
       col=ifelse(cfaq$resid.gam < 0, "red", "green"),
       cex=2*abs(cfaq$resid.gam)/max(abs(cfaq$resid.gam)),
                                     pch=21,
                                     bg = adjustcolor("black", alpha.f = 0.1),
                                     #col = "black",
                                     #cex = 0.9, 
                                     lwd = 0.5)

grid.gam.se <- grid
values(grid.gam.se) <- grid.df$pred.gam.se
plot(grid.gam.se, main="GAM prediction standard error",
     col=cm.colors(64))
grid()
points(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], pch=16,
       col="grey")

#grid.df$ts2.fit
#grid.df$pred.gam

summary(grid.df$diff.gam.ols <- grid.df$pred.gam - grid.df$ts2.fit)

grid.diff.gam.ols <- grid
values(grid.diff.gam.ols) <- grid.df$diff.gam.ols
plot(grid.diff.gam.ols,
     main="Difference (GAM - 2nd order trend surface) predictions, [m]",
     col=topo.colors(64))
grid()

cfaq.tps <- cfaq[, c("X","Y", "waterLevel")]
cfaq.tps$coords <- matrix(c(cfaq.tps$X, cfaq.tps$Y), byrow=F, ncol=2)
str(cfaq.tps$coords)

surf.1 <-Tps(cfaq.tps$coords, cfaq.tps$waterLevel)

summary(surf.1)

grid.coords.m <- as.matrix(grid.df[, c("X", "Y")], ncol=2)
str(grid.coords.m)

surf.1.pred <- predict.Krig(surf.1, grid.coords.m)
summary(grid.df$pred.tps <- as.numeric(surf.1.pred))

grid.tps <- grid
values(grid.tps) <- surf.1.pred
tmp <- extract(grid.tps, st_coordinates(cfaq.sf))
names(tmp)

summary(cfaq.sf$resid.tps <- (cfaq$waterLevel - tmp$waterLevel))
#summary(cfaq.sf$resid.tps <- (drop_units(cfaq.sf$waterLevel) - tmp$waterLevel))

hist(cfaq.sf$resid.tps, main="Thin-plate spline residuals", breaks=16)
rug(cfaq.sf$resid.tps)

plot(grid.tps, col = rainbow(100),
     main = "2D Thin-plate spline surface")
points(st_coordinates(cfaq.sf)[,2] ~ st_coordinates(cfaq.sf)[,1], 
       #       pch=16,
       col=ifelse(cfaq.sf$resid.tps < 0, "red", "green"),
       cex=2*abs(cfaq.sf$resid.tps)/max(abs(cfaq.sf$resid.tps)),
       pch=21,
       bg = adjustcolor("black", alpha.f = 0.1),
       #col = "black",
       #cex = 0.9, 
       lwd = 0.5)

grid.diff.tps.gam <- grid
grid.diff.tps.gam <- grid.tps - grid.gam
values(grid.diff.tps.gam) <- (values(grid.tps) - values(grid.gam))
plot(grid.diff.tps.gam,
     col = topo.colors(64),
     main = "Thin-plate spline less GAM fits, [m]",
     xlab = "X", ylab = "Y")

#- emphirical variogram
cfaq$fit.ts2 <- fitted(model.ts2)
cfaq.sf$fit.ts2 <- fitted(model.ts2)
cfaq$res.ts2 <- residuals(model.ts2)
cfaq.sf$res.ts2 <- residuals(model.ts2)

vr.c <- variogram(res.ts2 ~ 1, loc = cfaq.sf, cutoff = 50000, cloud = T)
vr <- variogram(res.ts2 ~ 1, loc = cfaq.sf, cutoff = 50000)
#vr.fit = fit.variogram(vr, vgm(1, "Sph"))#, 700, 1)
vr.fit = fit.variogram(vr, vgm(c("Exp", "Mat", "Sph")), fit.kappa = TRUE)
vr.fit

p1 <- plot(vr.c, col = "blue", pch = 20, cex = 0.5,
           xlab = "separation [m]", ylab = "semivariance [m^2]")
p2 <- plot(vr, plot.numbers = T, col = "blue", pch = 20, cex = 1.5,
          xlab = "separation [m]", ylab = "semivariance [m^2]")
#p2 <- plot(vr, vr.fit, col = "blue", pch = 20, cex = 1.5,
           #xlab = "separation [m]", ylab = "semivariance [m^2]")
print(p1, split = c(1,1,2,1), more = T)
print(p2, split = c(2,1,2,1), more = F)

