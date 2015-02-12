library(oz)



makeOzViewports <- function(ozRegion) {
  vpStack(viewport(name="ozlay", layout=grid.layout(1, 1,
                     widths=diff(ozRegion$rangex),
                     heights=diff(ozRegion$rangey), 
                     respect=TRUE)),
          viewport(name="ozvp", layout.pos.row=1, 
                   layout.pos.col=1,
                   xscale=ozRegion$rangex, 
                   yscale=ozRegion$rangey, 
                   clip=TRUE))
}

makeOzLines <- function(ozRegion) {
  numLines <- length(ozRegion$lines)
  lines <- vector("list", numLines)
  index <- 1
  for(i in ozRegion$lines) {
    lines[[index]] <- linesGrob(i$x, i$y, 
                    default.units="native",
                    vp=vpPath("ozlay", "ozvp"), 
                    name=paste("ozlines", index, sep=""))
    index <- index + 1
  }
  do.call("gList", lines)
}

ozGrob <- function(ozRegion, name=NULL, gp=NULL, vp=NULL) {
  gTree(ozRegion=ozRegion, name=name, gp=gp, vp=vp, 
    childrenvp=makeOzViewports(ozRegion), 
    children=makeOzLines(ozRegion), 
    cl="ozGrob")
}

grid.ozGrob <- function(...) {
  grid.draw(ozGrob(...))
}


calcBreaks <- function(nlevels, breaks, scale) {
  if (is.null(breaks)) {
    seq(min(scale), max(scale), diff(scale)/nlevels)
  } else {
    breaks
  }
}

ribbonVps <- function(nlevels, breaks, margin, scale) {
  breaks <- format(signif(calcBreaks(nlevels, breaks, scale), 
                          3))
  vpTree(
    viewport(name="layout", layout=
      grid.layout(3, 4,
        widths=unit.c(margin, unit(1, "lines"),
                      max(unit(0.8, "lines") + 
                          stringWidth(breaks)), margin),
        heights=unit.c(margin, unit(1, "null"), margin))),
    vpList(viewport(layout.pos.col=2, layout.pos.row=2,
                    yscale=scale, name="ribbon"),
           viewport(layout.pos.col=3, layout.pos.row=2,
                    yscale=scale, name="labels")))
}

ribbonKids <- function(nlevels, breaks, cols, scale) {
  breaks <- calcBreaks(nlevels, breaks, scale)
  nb <- length(breaks)
  tickloc <- breaks[-c(1, nb)]
  gList(rectGrob(y=unit(breaks[-1], "native"), 
                 height=unit(diff(breaks), "native"),
                 just="top", gp=gpar(fill=cols),
                 vp=vpPath("layout", "ribbon")),
        segmentsGrob(x1=unit(0.5, "lines"),
                     y0=unit(tickloc, "native"),
                     y1=unit(tickloc, "native"),
                     vp=vpPath("layout", "labels")),
        textGrob(x=unit(0.8, "lines"),
                 y=unit(tickloc, "native"),
                 just="left", 
                 label=format(signif(tickloc, 3)),
                 vp=vpPath("layout", "labels")))
}


ribbonLegend <- function(nlevels=NULL, breaks=NULL, cols, 
                         scale=range(breaks), 
                         margin=unit(0.5, "lines"), 
                         gp=NULL, vp=NULL, name=NULL) {
  gTree(
    nlevels=nlevels, breaks=breaks, cols=cols, scale=scale, 
    children=ribbonKids(nlevels, breaks, cols, scale),
    childrenvp=ribbonVps(nlevels, breaks, margin, scale),
    gp=gp, vp=vp, name=name, cl="ribbonLegend")
}

widthDetails.ribbonLegend <- function(x) { 
  sum(layout.widths(viewport.layout(x$childrenvp[[1]]))) 
} 


grid.ozGrob(ozRegion())
downViewport("ozvp")
for (i in 1:(dim(ozTemp)[1])) {
  grid.points(ozTemp$long[i], ozTemp$lat[i], pch=16)
  rl <- ribbonLegend(breaks=c(min(ozTemp$min), 
                              ozTemp$min[i], 
                              ozTemp$max[i], 
                              max(ozTemp$max)),
                     cols=c("white", "grey", "white"))
  pushViewport(viewport(x=unit(ozTemp$long[i], "native"),
                        y=unit(ozTemp$lat[i], "native"),
                        height=unit(1, "inches"),
                        width=grobWidth(rl),
                        clip="off"))
  grid.circle(r=0.7, 
              gp=gpar(col="grey", fill="white", alpha=0.8))
  grid.draw(rl)
  popViewport()
}
upViewport(0)



