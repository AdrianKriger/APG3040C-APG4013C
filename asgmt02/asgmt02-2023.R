## APG3040C-APG4013C July-Sept 2023
## arkriger

##---------
#-- student number: [write your student number here]
#-- name:           [write your name.surname (exactly like that {name.surname}) here]
#----------

install.packages(c("sf", "gstat"))

library(sp)
library(gstat)

data(meuse)
class(meuse)

names(meuse)

coordinates(meuse) = ~x+y
class(meuse)

#attr("package")

summary(meuse)

coordinates(meuse)[1:5,]

bubble(meuse, "zinc", col=c("#00ff0088", "#00ff0088"), main = "zinc concentrations (ppm)")

##-- Spatial data on a regular grid
data(meuse.grid)
summary(meuse.grid)

class(meuse.grid)

cooinstall.packages(c("sp", "gstat", "lattice", "automap"))install.packages(c("sp", "gstat", "lattice", "automap"))install.packages(c("sp", "gstat", "lattice", "automap"))rdinates(meuse.grid) = ~x+y
class(meuse.grid)

gridded(meuse.grid) = TRUE
class(meuse.grid)

image(meuse.grid["dist"])
title("distance to river (red = 0)")
zinc.idw = idw(zinc~1, meuse, meuse.grid)


##-- [inverse distance weighted interpolation]
class(zinc.idw)

spplot(zinc.idw["var1.pred"], main = "zinc inverse distance weighted interpolations")

plot(log(zinc)~sqrt(dist), meuse)
abline(lm(log(zinc)~sqrt(dist), meuse))


##-- Variograms
lzn.vgm = variogram(log(zinc)~1, meuse)
lzn.vgm

lzn.fit = fit.variogram(lzn.vgm, model = vgm(1, "Sph", 900, 1))
lzn.fit

plot(lzn.vgm, lzn.fit)

lznr.vgm = variogram(log(zinc)~sqrt(dist), meuse)
lznr.fit = fit.variogram(lznr.vgm, model = vgm(1, "Exp", 300, 1))
lznr.fit

plot(lznr.vgm, lznr.fit)


##-- Kriging
lzn.kriged = krige(log(zinc)~1, meuse, meuse.grid, model = lzn.fit)

#- [using ordinary kriging]
spplot(lzn.kriged["var1.pred"])


##-- Conditional simulation
lzn.condsim = krige(log(zinc)~1, meuse, meuse.grid, model = lzn.fit, nmax = 30, nsim = 4)

#- [using conditional Gaussian simulation]
spplot(lzn.condsim, main = "four conditional simulations")

lzn.condsim2 = krige(log(zinc)~sqrt(dist), meuse, meuse.grid, model = lznr.fit, nmax = 30, nsim = 4)

#- [using conditional Gaussian simulation]
spplot(lzn.condsim2, main = "four UK conditional simulations")


##-- Directional variograms
lzn.dir = variogram(log(zinc)~1, meuse, alpha = c(0, 45, 90, 135))
lzndir.fit = vgm(.59, "Sph", 1200, .05, anis = c(45, .4))
plot(lzn.dir, lzndir.fit, as.table = TRUE)

lznr.dir = variogram(log(zinc)~sqrt(dist), meuse, alpha = c(0, 45, 90, 135))
plot(lznr.dir, lznr.fit, as.table = TRUE)


##-- Variogram maps
vgm.map = variogram(log(zinc)~sqrt(dist), meuse, cutoff = 1500, width = 100, map = TRUE)
plot(vgm.map, threshold = 5)


##-- Cross variography
g = gstat(NULL, "log(zn)", log(zinc)~sqrt(dist), meuse)
g = gstat(g, "log(cd)", log(cadmium)~sqrt(dist), meuse)
g = gstat(g, "log(pb)", log(lead)~sqrt(dist), meuse)
g = gstat(g, "log(cu)", log(copper)~sqrt(dist), meuse)
v = variogram(g)
g = gstat(g, model = vgm(1, "Exp", 300, 1), fill.all = TRUE)
g.fit = fit.lmc(v, g)
g.fit


plot(v, g.fit)
vgm.map = variogram(g, cutoff = 1500, width = 100, map = TRUE)
plot(vgm.map, threshold = 5, col.regions = bpy.colors(), xlab = "", ylab = "")
