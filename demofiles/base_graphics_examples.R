## Run barplot examples
par(ask=FALSE)
example(barplot)

require(grDevices) # for colours
tN <- table(Ni <- stats::rpois(100, lambda = 5))
?rpois
barplot(tN, col = gray.colors(length(tN)))

example(boxplot)

?OrchardSprays

rb <- boxplot(decrease ~ treatment, data = OrchardSprays, col = "bisque")
title("Comparing boxplot()s and non-robust mean +/- SD")
mn.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, mean)
sd.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, sd)
xi <- 0.3 + seq(rb$n)
points(xi, mn.t, col = "orange", pch = 18)
arrows(xi, mn.t - sd.t, xi, mn.t + sd.t, code = 3, col = "pink", angle = 75, length = .1)

## Datasets with 3 variables: 3d plots
example(persp)

## rotated two-dimensional unnormalized sinc function: obligatory mathematical surface
require(grDevices) # for trans3d
x <- seq(-10, 10, length= 30)
y <- x
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
op <- par(bg = "white")

persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Sinc( r )"
) -> res
# (2) Add to existing persp plot - using trans3d() :
xE <- c(-10,10); xy <- expand.grid(xE, xE)
points(trans3d(xy[,1], xy[,2], 6, pmat = res), col = 2, pch = 16)
lines (trans3d(x, y = 10, z = 6 + sin(x), pmat = res), col = 3)
phi <- seq(0, 2*pi, len = 201)
r1 <- 7.725 # radius of 2nd maximum
xr <- r1 * cos(phi)
yr <- r1 * sin(phi)
lines(trans3d(xr,yr, f(xr,yr), res), col = "pink", lwd = 2)


## Datasets with more than 3 variables
example(pairs)

pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

example(stars)

## Spider or Radar plot:
stars(mtcars[, 1:7], key.loc = c(14, 1.5),
      main = "Motor Trend Cars : full stars()", flip.labels = FALSE)

## Segment Diagrams:
palette(rainbow(12, s = 0.6, v = 0.75))
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5),
      main = "Motor Trend Cars", draw.segments = TRUE)

example(mosaicplot)

par(mfrow = c(1,2))
mosaicplot(HairEyeColor, shade = TRUE)
mosaicplot(HairEyeColor, shade = TRUE, margin = list(1:2, 3))


## Bill Cleveland: based on principles of human perception
example(dotchart)


example(coplot) # conditioning plot

## Tonga Trench Earthquakes
coplot(lat ~ long | depth, data = quakes)

## Example with empty panels:
with(data.frame(state.x77), {
coplot(Life.Exp ~ Income | Illiteracy * state.region, number = 3,
       panel = function(x, y, ...) panel.smooth(x, y, span = .8, ...))
## ## y ~ factor -- not really sensible, but 'show off':
## coplot(Life.Exp ~ state.region | Income * state.division,
##        panel = panel.smooth)
})

## install.packages("maps")
library(maps)
coplot(lat ~ long | depth, data = quakes, number=4,
panel=function(x, y, ...) {
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], col="white")
map("world2", regions=c("New Zealand", "Fiji"),
add=TRUE, lwd=0.1, fill=TRUE, col="grey")
text(180, -13, "Fiji", adj=1, cex=0.7)
text(170, -35, "NZ", cex=0.7)
points(x, y, pch=".")
})



## lm example
lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
par(mfrow=c(2,2))
plot(lm.SR)

## agnes example: Agglomerative Nesting (Hierarchical Clustering)
require(cluster)
subset <- sample(1:150, 20)
cS <- as.character(Sp <- iris$Species[subset])
cS[Sp == "setosa"] <- "S"
cS[Sp == "versicolor"] <- "V"
cS[Sp == "virginica"] <- "g"
ai <- agnes(iris[subset, 1:4])
par(mfrow=c(1,2))
plot(ai, labels = cS)

require(lattice)
example(lattice)

install.packages("oz")
install.packages("RGraphics")
require(RGraphics)
require(grid)
require(oz)
data(ozTemp)
source(file.path(dbpath, "GitHub", "as_seminar_report", "assets", "code", "interactgrid-oztempfig.R"))


source(file.path(dbpath, "GitHub", "as_seminar_report", "assets", "code", "combine-gridbaselattice.R"))
