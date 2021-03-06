---
title       : R Graphics
subtitle    : Generating graphics for statistical analysis using R
author      : Bo Werth
job         : Statistician STI/EAS
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # {arta, ascetic, brown_paper, dark, default, far, github, googlecode, hemisu-light, idea, ir_black, magula, monokai, pojoaque, school_book, solarized_dark, solarized_light, sunburst, tomorrow_night, vs, xcode, zenburn}
widgets     : []            # {mathjax, quiz, bootstrap}
knit        : slidify::knit2slides
mode        : selfcontained # {standalone, draft}
ext_widgets : {rCharts: [libraries/highcharts, libraries/nvd3]}
github      : []

--- &twocol



## Graphics in statistical software

*** =left
programming
  - quality: reproducibility, calculations

  - efficiency gains: scaling

  - output flexibility: PDF, Word, HTML

*** =right
interactive creation (e.g. Excel, Tableau)
  - development time

  - changing requirements

  - maintenance and transparency

--- &twocol

## `R` Graphics Systems

[google trends: python, r, ggplot](https://www.google.com/trends/explore?date=all&q=python%20programming,r%20programming,ggplot)

*** =left
traditional graphics
  - graphics facilities of the S language
  - fast, intuitive
  - create plot with high-level function
  - add elements with low-level functions

*** =right
grid graphics system
  - produce complete plots
  - ideal proportions
  - facetting or multi-panel conditioning
  - `trellis`, `lattice`, `ggplot`

--- .class #id

## The organization of `R` graphics

<p style="text-align:center"><img src="assets/img/R_graphics_system.svg" alt="R graphics system" height="550px"/></p>

--- .class #id

#### Publication quality graphics with `ggplot`

- implementation of Leland Wilkinson's "grammar of graphics" (2005)
- independent components that can be composed in different ways
- not limited to a set of pre-specified graphics
- create new graphics that are precisely tailored

#### Interactive Graphics using JavaScript

- `ggvis`: `ggplot` for dynamic charts based on `vega.js`
- `rCharts`: interface to high-level js libraries building e.g. on `d3.js`
  - `highcharts`, `polyplot`, `nvd3`, `ricksaw` for statistical charts
  - `crosslet`, `datamaps`, `leaflet` etc. for map visualisations

--- .class #id

### traditional S-PLUS graphics

  - pen on paper model:
    - can only draw on top of the plot
    - cannot modify or delete existing content
  - no (user accessible) representation of the graphics
  - includes both tools for drawing primitives and entire plots
  - generally fast, but have limited scope


--- .class #id

### traditional S-PLUS graphics: `example(plot)`

![plot of chunk plotexample](assets/fig/plotexample-1.png)

--- &twocol

### traditional S-PLUS graphics: plot regions

*** =left
Single plot regions

![Single Plot Regions](assets/img/single_plot_regions.svg)

*** =right
Multiple plot regions

![Multi Plot Regions](assets/img/multi_plot_regions.svg)

--- .class #id

### traditional S-PLUS graphics: plot regions


```r
op <- par(mfrow = c(2, 2),
	      mar = c(3, 0, 0, 0))
plot(...); plot(...); plot(...); plot(...)
## At end of plotting, reset to previous settings:
par(op)
```

- the documentation can be looked up with `?par()`
- margins are measured in multiples of lines of text
- modifying traditional graphics state settings via `par()` has a persistent effect

--- &twocol

### traditional S-PLUS graphics: plot regions


```r
op <- par(mfrow = c(2, 2),
	      mar = c(3, 0, 0, 0))
plot(...); plot(...); plot(...); plot(...)
## At end of plotting, reset to previous settings:
par(op)
```

- `mfrow` and `mfcol` control the number of figure regions on a page

*** =left

![par mfrow](assets/img/par_mfrow.svg)

*** =right

![par mfcol](assets/img/par_mfcol.svg)

--- &twocol

### traditional S-PLUS graphics: controlling plot regions

*** =left

![Graphics state settings controlling plot regions](assets/img/graphic_state_setting_controlling_plot_regions.svg)

*** =right
- diagram for controlling widths and horizontal locations
- plot region = figure region - figure margins
- `plt`: location of the plot region (l, r, b, t)
- `pin`: size of the plot region, (width, height)
- `pty`: `m`: use all available space, `s`: preserve square format

--- .class #id

### traditional S-PLUS graphics: colors and colours


```r
colours()[1:4] # 657 color names
```

```
## [1] "white"         "aliceblue"     "antiquewhite"  "antiquewhite1"
```

```r
col2rgb("transparent") # see the RGB values for a particular color name
```

```
##       [,1]
## red    255
## green  255
## blue   255
```

```r
rgb(1, 0, 0) # Red-Green-Blue triplet of intensities, format #RRGGBB, FF = 255
```

```
## [1] "#FF0000"
```

--- .class #id

### traditional S-PLUS graphics: `pch` point symbols

![pch symbols](assets/img/pch_symbols.png)

A particular data symbol is selected by specifying an integer between 0 and 25 or a single character for the `pch` graphical setting. In the diagram, the relevant integer or character `pch` value is shown in grey to the left of the relevant symbol.

Source: http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/

--- &twocol

### traditional S-PLUS graphics

*** =left
example(barplot)

![plot of chunk barplot](assets/fig/barplot-1.png)

*** =right
example(boxplot)

![plot of chunk boxplotex](assets/fig/boxplotex-1.png)

--- &twocol

### traditional S-PLUS graphics

*** =left
example(pairs)

![plot of chunk pairplot](assets/fig/pairplot-1.png)

*** =right
example(persp)

![plot of chunk perspplot](assets/fig/perspplot-1.png)

--- &twocol

### traditional S-PLUS graphics: `example(stars)`

*** =left

![plot of chunk starsegment](assets/fig/starsegment-1.png)

*** =right

![plot of chunk starradar](assets/fig/starradar-1.png)

--- &twocol

### traditional S-PLUS graphics: `example(mosaicplot)`

*** =left

![plot of chunk mosaic1](assets/fig/mosaic1-1.png)

*** =right

![plot of chunk mosaic2](assets/fig/mosaic2-1.png)

--- &twocol

### traditional S-PLUS graphics: conditioning plot, `example(coplot)`

*** =left

![plot of chunk coplot1](assets/fig/coplot1-1.png)

*** =right

![plot of chunk coplot2](assets/fig/coplot2-1.png)

--- .class #id

### traditional S-PLUS graphics: lm example

![plot of chunk lmplot](assets/fig/lmplot-1.png)

--- .class #id

### traditional S-PLUS graphics: Agglomerative Nesting (Hierarchical Clustering)

![plot of chunk agnesplot](assets/fig/agnesplot-1.png)

--- .class #id

### traditional S-PLUS graphics




```r
plot(hclust(d = dist(USArrests), method = "average"), main=title)
```

![plot of chunk unnamed-chunk-2](assets/fig/unnamed-chunk-2-1.png)

--- &twocol

### lattice / grid graphics: pre and post drawing

*** =left

![plot of chunk oztemp](assets/fig/oztemp-1.png)

*** =right
- draw map of Australia
- draw average monthly temperatures for six cities

--- &twocol

### lattice / grid graphics: embedding plots in grid viewports

*** =left

![plot of chunk viewport](assets/fig/viewport-1.png)

*** =right
- create dendrogram object and cut it into four subtrees
- define lattice panel function to draw the dendrograms
- make base plot region correspond to the created viewport
- use traditional `plot()` function to draw the dendrogram

--- .class #id

### Layered Grammar of Graphics

A statistical graphic is a mapping from data to
  - geometric objects (points, lines, bars)
  - with aesthetic attributes (colour, shape, size)
  - in a coordinate system (cartesian, polar, map projection)

and optionally entails
  - statistical transformations of the data (binning, counting)
  - faceting to generate the same graphic for different subsets of the dataset

ggplot2 attempts to produce any kind of statistical graphic using
 - a compact syntax and independent components to facilitate extensions
 - the `grid` package to exercise low-level control over the appearance of the plot

Wickham, H. (2009). ggplot2. doi:10.1007/978-0-387-98141-3

--- &twocol

### Comparison `plot()` and `qplot()`



*** =left


```r
plot(x, y)
```

![plot of chunk plot1](assets/fig/plot1-1.png)

*** =right


```r
qplot(x, y)
```

![plot of chunk qplot1](assets/fig/qplot1-1.png)

--- &twocol

### Comparison `plot()` and `qplot()`

*** =left


```r
plot(x, y, type = "l")
```

![plot of chunk plot2](assets/fig/plot2-1.png)

*** =right


```r
qplot(x, y, geom = "line")
```

![plot of chunk qplot2](assets/fig/qplot2-1.png)

--- &twocol

### Comparison `plot()` and `qplot()`

*** =left


```r
plot(x, y, type = "s")
```

![plot of chunk plot3](assets/fig/plot3-1.png)

*** =right


```r
qplot(x, y, geom = "step")
```

![plot of chunk qplot3](assets/fig/qplot3-1.png)

--- &twocol

### Comparison `plot()` and `qplot()`

*** =left


```r
plot(x, y, type = "b")
```

![plot of chunk plot4](assets/fig/plot4-1.png)

*** =right


```r
qplot(x, y, geom = c("point", "line"))
```

![plot of chunk qplot4](assets/fig/qplot4-1.png)

--- .class #id

### `mtcars` dataset

Data from the 1974 _Motor Trend_ US magazine for 32 automobiles (1973-74 models). The variables are the following:
  - `mpg` Miles/(US) gallon
  - `cyl` Number of cylinders
  - `disp` Displacement (cu.in.)
  - `hp` Gross horsepower
  - `drat` Rear axle ratio
  - `wt` Weight (lb/1000)
  - `qsec` 1/4 mile time
  - `vs` V/S
  - `am` Transmission (0 = automatic, 1 = manual)
  - `gear` Number of forward gears
  - `carb` Number of carburetors

--- &twocol

### Comparison `plot()` and `qplot()`

*** =left


```r
boxplot(wt~cyl,
	    data=mtcars, col="lightgray")
```

![plot of chunk boxplotcomp](assets/fig/boxplotcomp-1.png)

*** =right


```r
qplot(factor(cyl), wt,
	  data=mtcars, geom=c("boxplot", "jitter"))
```

![plot of chunk boxqplotcomp](assets/fig/boxqplotcomp-1.png)

--- &twocol

### Comparison `plot()` and `qplot()`

*** =left


```r
hist(mtcars$wt)
```

![plot of chunk histplotcomp](assets/fig/histplotcomp-1.png)

*** =right


```r
qplot(mtcars$wt, geom = "histogram",
	  binwidth = 0.5, color = factor(0))
```

![plot of chunk histqplotcomp](assets/fig/histqplotcomp-1.png)

--- &twocol

### Comparison `plot()` and `qplot()`

*** =left


```r
cdplot(mtcars$wt, factor(mtcars$cyl))
```

![plot of chunk cdplotcomp](assets/fig/cdplotcomp-1.png)

*** =right


```r
qplot(mtcars$wt, fill=factor(mtcars$cyl),
	  geom="density")
```

![plot of chunk cdqplotcomp](assets/fig/cdqplotcomp-1.png)

--- .class #id

### `diamonds` dataset

A dataset containing the prices and other attributes of almost 54,000 diamonds. The variables are as follows:
  - `price` price in US dollars ($326-$18,823)
  - `carat` weight of the diamond (0.2-5.01)
  - `cut` quality of the cut (Fair, Good, Very Good, Premium, Ideal)
  - `colour` diamond colour, from J (worst) to D (best)
  - `clarity` a measurement of how clear the diamond is (I1 (worst), SI1, SI2, VS1, VS2, VVS1, VVS2, IF (best))
  - `x` length in mm (0-10.74)
  - `y` width in mm (0-58.9)
  - `z` depth in mm (0-31.8)
  - `depth` total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43-79)
  - `table` width of top of diamond relative to widest point (43-95)

--- &twocol

### ggplot2 `qplot()`: single line of code

*** =left


```r
qplot(x=carat, y=price, colour=clarity,
  data=diamonds, geom=("point"))
```

![plot of chunk qplotex1](assets/fig/qplotex1-1.png)

*** =right


```r
qplot(carat, price,
  data = diamonds, geom = c("point", "smooth"))
```

![plot of chunk qplotex2](assets/fig/qplotex2-1.png)

--- .class #id

### ggplot2 `ggplot()`: add layers for more control using "`+`"


```r
ggplot(data=diamonds) +
	geom_point(aes(x=price, y=carat, colour=color)) +
    facet_grid(. ~ clarity)
```

![plot of chunk ggplotex](assets/fig/ggplotex-1.png)

--- .class #id

### ggplot2 `ggplot()`: add layers for more control using "`+`"








```r
ggplot(data = df,aes(x = x,y = y)) + # source: https://plot.ly/ggplot2/geom_errorbar/
    geom_errorbar(aes(ymin = ymin,ymax = ymax), colour = 'steelblue', width = 0.2) +
    geom_errorbarh(aes(xmin = xmin,xmax = xmax), colour = 'steelblue', height = 0.4) +
    geom_point(color = "black", size = 3)
```

![plot of chunk ggplotlayer](assets/fig/ggplotlayer-1.png)

--- &twocol

### ggplot Themes



*** =left
`theme_stata()`

![plot of chunk ggthemesstata](assets/fig/ggthemesstata-1.png)

*** =right
`theme_economist()`

![plot of chunk ggthemeseconomist](assets/fig/ggthemeseconomist-1.png)

--- &twocol

### ggplot Themes

*** =left
`theme_fivethirtyeight()`

![plot of chunk ggthemesfivethirtyeight](assets/fig/ggthemesfivethirtyeight-1.png)

*** =right
Tableau `theme_igray()`

![plot of chunk ggthemesigray](assets/fig/ggthemesigray-1.png)

--- .class #id

### ggplot Themes: `ggthemr`




```r
pal.crN <- c('#95B3D7','#F79646','#8064A2','#4BACC6','#9BBB59','#C0504D')
ugly <- define_palette(
  swatch = pal.crN, gradient = c(lower = pal.crN[1L], upper = pal.crN[2L]))
ggthemr(ugly)
ggplot(dsamp, aes(x=price, fill=cut)) + geom_bar(binwidth = 500)
```

![plot of chunk ggthemr](assets/fig/ggthemr-1.png)

```r
ggthemr_reset()
```

--- &twocol

### gridSVG: gapminder

*** =left

<p style="text-align:center"><img src="assets/img/gapminderOnePanel.svg" alt="gapminder animation" height="550px"/></p>

*** =right

- `gridSVG` animates a ggplot object before the output is flattened for export
  to a graphics device
- the animation is obtained from mapping to the time dimension (annual since
  1950)
- `size` = population
- `color` = continent
    - `blue`: Europe
    - `red`: Asia
    - `green`: Africa
    - `yellow`: America

--- &twocol

### gridSVG: R&D Expendidures

*** =left

<p style="text-align:center"><img src="assets/img/RDplotOnePanel.svg" alt="rnd animation" height="550px"/></p>

*** =right

- 

--- &twocol

### ggvis: `/demo/dynamic.r`

*** =left
### dynamic stacked bars
<p style="text-align:center"><img src="assets/img/ggvis_demo_dynamic_stacked_bars.svg" alt="ggvis stacked bars" height="450px"/></p>

*** =right
### moving data points
<p style="text-align:center"><img src="assets/img/ggvis_demo_moving_points.svg" alt="ggvis moving points" height="450px"/></p>

--- .class #id

### rCharts: nvd3 Sparklines


```r
p2 <- nPlot(uempmed ~ date, data = economics, type = 'sparklinePlus')
p2$chart(xTickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#")
p2$print('chart2')
```


<div id = 'chart2' class = 'rChart nvd3'></div>
<script type='text/javascript'>
 $(document).ready(function(){
      drawchart2()
    });
    function drawchart2(){  
      var opts = {
 "dom": "chart2",
"width":    800,
"height":    400,
"x": "date",
"y": "uempmed",
"type": "sparklinePlus",
"id": "chart2" 
},
        data = [
 {
 "date": -915,
"pce":          507.4,
"pop": 198712,
"psavert":           12.5,
"uempmed":            4.5,
"unemploy": 2944 
},
{
 "date": -884,
"pce":          510.5,
"pop": 198911,
"psavert":           12.5,
"uempmed":            4.7,
"unemploy": 2945 
},
{
 "date": -853,
"pce":          516.3,
"pop": 199113,
"psavert":           11.7,
"uempmed":            4.6,
"unemploy": 2958 
},
{
 "date": -823,
"pce":          512.9,
"pop": 199311,
"psavert":           12.5,
"uempmed":            4.9,
"unemploy": 3143 
},
{
 "date": -792,
"pce":          518.1,
"pop": 199498,
"psavert":           12.5,
"uempmed":            4.7,
"unemploy": 3066 
},
{
 "date": -762,
"pce":          525.8,
"pop": 199657,
"psavert":           12.1,
"uempmed":            4.8,
"unemploy": 3018 
},
{
 "date": -731,
"pce":          531.5,
"pop": 199808,
"psavert":           11.7,
"uempmed":            5.1,
"unemploy": 2878 
},
{
 "date": -700,
"pce":          534.2,
"pop": 199920,
"psavert":           12.2,
"uempmed":            4.5,
"unemploy": 3001 
},
{
 "date": -671,
"pce":          544.9,
"pop": 200056,
"psavert":           11.6,
"uempmed":            4.1,
"unemploy": 2877 
},
{
 "date": -640,
"pce":          544.6,
"pop": 200208,
"psavert":           12.2,
"uempmed":            4.6,
"unemploy": 2709 
},
{
 "date": -610,
"pce":          550.4,
"pop": 200361,
"psavert":             12,
"uempmed":            4.4,
"unemploy": 2740 
},
{
 "date": -579,
"pce":          556.8,
"pop": 200536,
"psavert":           11.6,
"uempmed":            4.4,
"unemploy": 2938 
},
{
 "date": -549,
"pce":          563.8,
"pop": 200706,
"psavert":           10.6,
"uempmed":            4.5,
"unemploy": 2883 
},
{
 "date": -518,
"pce":          567.6,
"pop": 200898,
"psavert":           10.4,
"uempmed":            4.2,
"unemploy": 2768 
},
{
 "date": -487,
"pce":          568.8,
"pop": 201095,
"psavert":           10.4,
"uempmed":            4.6,
"unemploy": 2686 
},
{
 "date": -457,
"pce":          572.3,
"pop": 201290,
"psavert":           10.6,
"uempmed":            4.8,
"unemploy": 2689 
},
{
 "date": -426,
"pce":          577.4,
"pop": 201466,
"psavert":           10.4,
"uempmed":            4.4,
"unemploy": 2715 
},
{
 "date": -396,
"pce":          577.2,
"pop": 201621,
"psavert":           10.9,
"uempmed":            4.4,
"unemploy": 2685 
},
{
 "date": -365,
"pce":          584.2,
"pop": 201760,
"psavert":             10,
"uempmed":            4.4,
"unemploy": 2718 
},
{
 "date": -334,
"pce":          589.5,
"pop": 201881,
"psavert":            9.4,
"uempmed":            4.9,
"unemploy": 2692 
},
{
 "date": -306,
"pce":          589.7,
"pop": 202023,
"psavert":            9.9,
"uempmed":              4,
"unemploy": 2712 
},
{
 "date": -275,
"pce":          594.7,
"pop": 202161,
"psavert":            9.5,
"uempmed":              4,
"unemploy": 2758 
},
{
 "date": -245,
"pce":          601.1,
"pop": 202331,
"psavert":             10,
"uempmed":            4.2,
"unemploy": 2713 
},
{
 "date": -214,
"pce":          601.7,
"pop": 202507,
"psavert":           10.9,
"uempmed":            4.4,
"unemploy": 2816 
},
{
 "date": -184,
"pce":          603.5,
"pop": 202677,
"psavert":           11.7,
"uempmed":            4.4,
"unemploy": 2868 
},
{
 "date": -153,
"pce":          610.8,
"pop": 202877,
"psavert":           11.5,
"uempmed":            4.4,
"unemploy": 2856 
},
{
 "date": -122,
"pce":          614.1,
"pop": 203090,
"psavert":           11.5,
"uempmed":            4.7,
"unemploy": 3040 
},
{
 "date": -92,
"pce":          619.4,
"pop": 203302,
"psavert":           11.3,
"uempmed":            4.5,
"unemploy": 3049 
},
{
 "date": -61,
"pce":          621.4,
"pop": 203500,
"psavert":           11.5,
"uempmed":            4.8,
"unemploy": 2856 
},
{
 "date": -31,
"pce":          623.7,
"pop": 203675,
"psavert":           11.7,
"uempmed":            4.6,
"unemploy": 2884 
},
{
 "date": 0,
"pce":          629.6,
"pop": 203849,
"psavert":           11.7,
"uempmed":            4.6,
"unemploy": 3201 
},
{
 "date": 31,
"pce":          634.9,
"pop": 204008,
"psavert":           11.6,
"uempmed":            4.5,
"unemploy": 3453 
},
{
 "date": 59,
"pce":          633.2,
"pop": 204156,
"psavert":           12.3,
"uempmed":            4.6,
"unemploy": 3635 
},
{
 "date": 90,
"pce":            637,
"pop": 204401,
"psavert":           13.3,
"uempmed":            4.1,
"unemploy": 3797 
},
{
 "date": 120,
"pce":          643.4,
"pop": 204607,
"psavert":           12.3,
"uempmed":            4.7,
"unemploy": 3919 
},
{
 "date": 151,
"pce":          647.2,
"pop": 204830,
"psavert":           11.7,
"uempmed":            4.9,
"unemploy": 4071 
},
{
 "date": 181,
"pce":          649.5,
"pop": 205052,
"psavert":           13.2,
"uempmed":            5.1,
"unemploy": 4175 
},
{
 "date": 212,
"pce":          653.9,
"pop": 205295,
"psavert":           13.1,
"uempmed":            5.4,
"unemploy": 4256 
},
{
 "date": 243,
"pce":          660.1,
"pop": 205540,
"psavert":           12.9,
"uempmed":            5.2,
"unemploy": 4456 
},
{
 "date": 273,
"pce":          659.3,
"pop": 205788,
"psavert":             13,
"uempmed":            5.2,
"unemploy": 4591 
},
{
 "date": 304,
"pce":          657.6,
"pop": 206024,
"psavert":           13.3,
"uempmed":            5.6,
"unemploy": 4898 
},
{
 "date": 334,
"pce":          666.6,
"pop": 206238,
"psavert":           12.9,
"uempmed":            5.9,
"unemploy": 5076 
},
{
 "date": 365,
"pce":          677.2,
"pop": 206466,
"psavert":           13.1,
"uempmed":            6.2,
"unemploy": 4986 
},
{
 "date": 396,
"pce":          680.4,
"pop": 206668,
"psavert":           13.1,
"uempmed":            6.3,
"unemploy": 4903 
},
{
 "date": 424,
"pce":            683,
"pop": 206855,
"psavert":           13.3,
"uempmed":            6.4,
"unemploy": 4987 
},
{
 "date": 455,
"pce":          689.8,
"pop": 207065,
"psavert":             13,
"uempmed":            6.5,
"unemploy": 4959 
},
{
 "date": 485,
"pce":          692.2,
"pop": 207260,
"psavert":           13.4,
"uempmed":            6.7,
"unemploy": 4996 
},
{
 "date": 516,
"pce":          700.8,
"pop": 207462,
"psavert":           14.4,
"uempmed":            5.7,
"unemploy": 4949 
},
{
 "date": 546,
"pce":          699.9,
"pop": 207661,
"psavert":           13.6,
"uempmed":            6.2,
"unemploy": 5035 
},
{
 "date": 577,
"pce":            706,
"pop": 207881,
"psavert":           13.6,
"uempmed":            6.4,
"unemploy": 5134 
},
{
 "date": 608,
"pce":          714.1,
"pop": 208114,
"psavert":           12.9,
"uempmed":            5.8,
"unemploy": 5042 
},
{
 "date": 638,
"pce":          716.9,
"pop": 208345,
"psavert":             13,
"uempmed":            6.5,
"unemploy": 4954 
},
{
 "date": 669,
"pce":          722.1,
"pop": 208555,
"psavert":           12.8,
"uempmed":            6.4,
"unemploy": 5161 
},
{
 "date": 699,
"pce":          729.6,
"pop": 208740,
"psavert":           12.9,
"uempmed":            6.2,
"unemploy": 5154 
},
{
 "date": 730,
"pce":          732.6,
"pop": 208917,
"psavert":           12.4,
"uempmed":            6.2,
"unemploy": 5019 
},
{
 "date": 761,
"pce":          737.3,
"pop": 209061,
"psavert":           12.6,
"uempmed":            6.6,
"unemploy": 4928 
},
{
 "date": 790,
"pce":          750.4,
"pop": 209212,
"psavert":           11.5,
"uempmed":            6.6,
"unemploy": 5038 
},
{
 "date": 821,
"pce":          753.8,
"pop": 209386,
"psavert":           11.3,
"uempmed":            6.7,
"unemploy": 4959 
},
{
 "date": 851,
"pce":          759.2,
"pop": 209545,
"psavert":           11.5,
"uempmed":            6.6,
"unemploy": 4922 
},
{
 "date": 882,
"pce":          762.9,
"pop": 209725,
"psavert":           11.4,
"uempmed":            5.4,
"unemploy": 4923 
},
{
 "date": 912,
"pce":          771.2,
"pop": 209896,
"psavert":           11.4,
"uempmed":            6.1,
"unemploy": 4913 
},
{
 "date": 943,
"pce":          777.7,
"pop": 210075,
"psavert":           11.8,
"uempmed":              6,
"unemploy": 4939 
},
{
 "date": 974,
"pce":          782.5,
"pop": 210278,
"psavert":             12,
"uempmed":            5.6,
"unemploy": 4849 
},
{
 "date": 1004,
"pce":          796.3,
"pop": 210479,
"psavert":           12.7,
"uempmed":            5.7,
"unemploy": 4875 
},
{
 "date": 1035,
"pce":          801.8,
"pop": 210656,
"psavert":           13.4,
"uempmed":            5.7,
"unemploy": 4602 
},
{
 "date": 1065,
"pce":          807.5,
"pop": 210821,
"psavert":           13.4,
"uempmed":            6.1,
"unemploy": 4543 
},
{
 "date": 1096,
"pce":          817.9,
"pop": 210985,
"psavert":           12.1,
"uempmed":            5.7,
"unemploy": 4326 
},
{
 "date": 1127,
"pce":          827.2,
"pop": 211120,
"psavert":           12.2,
"uempmed":            5.2,
"unemploy": 4452 
},
{
 "date": 1155,
"pce":          834.2,
"pop": 211254,
"psavert":           12.4,
"uempmed":            5.5,
"unemploy": 4394 
},
{
 "date": 1186,
"pce":          837.2,
"pop": 211420,
"psavert":           12.8,
"uempmed":              5,
"unemploy": 4459 
},
{
 "date": 1216,
"pce":          843.1,
"pop": 211577,
"psavert":           12.8,
"uempmed":            4.9,
"unemploy": 4329 
},
{
 "date": 1247,
"pce":          845.8,
"pop": 211746,
"psavert":           13.2,
"uempmed":              5,
"unemploy": 4363 
},
{
 "date": 1277,
"pce":          855.7,
"pop": 211909,
"psavert":           12.8,
"uempmed":            5.2,
"unemploy": 4305 
},
{
 "date": 1308,
"pce":          854.9,
"pop": 212092,
"psavert":           13.6,
"uempmed":            4.9,
"unemploy": 4305 
},
{
 "date": 1339,
"pce":          870.9,
"pop": 212289,
"psavert":           12.8,
"uempmed":            5.4,
"unemploy": 4350 
},
{
 "date": 1369,
"pce":          869.8,
"pop": 212475,
"psavert":             14,
"uempmed":            5.5,
"unemploy": 4144 
},
{
 "date": 1400,
"pce":          878.6,
"pop": 212634,
"psavert":             14,
"uempmed":            5.1,
"unemploy": 4396 
},
{
 "date": 1430,
"pce":          878.4,
"pop": 212785,
"psavert":           14.4,
"uempmed":            4.7,
"unemploy": 4489 
},
{
 "date": 1461,
"pce":          886.4,
"pop": 212932,
"psavert":             14,
"uempmed":              5,
"unemploy": 4644 
},
{
 "date": 1492,
"pce":          891.6,
"pop": 213074,
"psavert":           13.8,
"uempmed":            5.1,
"unemploy": 4731 
},
{
 "date": 1520,
"pce":          903.3,
"pop": 213211,
"psavert":             13,
"uempmed":            4.8,
"unemploy": 4634 
},
{
 "date": 1551,
"pce":          912.7,
"pop": 213361,
"psavert":           12.7,
"uempmed":              5,
"unemploy": 4618 
},
{
 "date": 1581,
"pce":          924.3,
"pop": 213513,
"psavert":           12.3,
"uempmed":            4.6,
"unemploy": 4705 
},
{
 "date": 1612,
"pce":          929.9,
"pop": 213686,
"psavert":           12.5,
"uempmed":            5.3,
"unemploy": 4927 
},
{
 "date": 1642,
"pce":          939.8,
"pop": 213854,
"psavert":           12.7,
"uempmed":            5.7,
"unemploy": 5063 
},
{
 "date": 1673,
"pce":          956.6,
"pop": 214042,
"psavert":           11.6,
"uempmed":              5,
"unemploy": 5022 
},
{
 "date": 1704,
"pce":          956.8,
"pop": 214246,
"psavert":           12.3,
"uempmed":            5.3,
"unemploy": 5437 
},
{
 "date": 1734,
"pce":            961,
"pop": 214451,
"psavert":             13,
"uempmed":            5.5,
"unemploy": 5523 
},
{
 "date": 1765,
"pce":            958,
"pop": 214625,
"psavert":           13.4,
"uempmed":            5.2,
"unemploy": 6140 
},
{
 "date": 1795,
"pce":          963.6,
"pop": 214782,
"psavert":           13.6,
"uempmed":            5.7,
"unemploy": 6636 
},
{
 "date": 1826,
"pce":          977.4,
"pop": 214931,
"psavert":           12.8,
"uempmed":            6.3,
"unemploy": 7501 
},
{
 "date": 1857,
"pce":          991.3,
"pop": 215065,
"psavert":           12.1,
"uempmed":            7.1,
"unemploy": 7520 
},
{
 "date": 1885,
"pce":          992.6,
"pop": 215198,
"psavert":           12.3,
"uempmed":            7.2,
"unemploy": 7978 
},
{
 "date": 1916,
"pce":          997.2,
"pop": 215353,
"psavert":           13.9,
"uempmed":            8.7,
"unemploy": 8210 
},
{
 "date": 1946,
"pce":         1021.2,
"pop": 215523,
"psavert":             17,
"uempmed":            9.4,
"unemploy": 8433 
},
{
 "date": 1977,
"pce":         1029.1,
"pop": 215768,
"psavert":           13.9,
"uempmed":            8.8,
"unemploy": 8220 
},
{
 "date": 2007,
"pce":         1042.2,
"pop": 215973,
"psavert":           12.3,
"uempmed":            8.6,
"unemploy": 8127 
},
{
 "date": 2038,
"pce":         1049.4,
"pop": 216195,
"psavert":           12.6,
"uempmed":            9.2,
"unemploy": 7928 
},
{
 "date": 2069,
"pce":         1057.2,
"pop": 216393,
"psavert":           12.6,
"uempmed":            9.2,
"unemploy": 7923 
},
{
 "date": 2099,
"pce":         1063.2,
"pop": 216587,
"psavert":             13,
"uempmed":            8.6,
"unemploy": 7897 
},
{
 "date": 2130,
"pce":           1078,
"pop": 216771,
"psavert":           12.3,
"uempmed":            9.5,
"unemploy": 7794 
},
{
 "date": 2160,
"pce":         1094.4,
"pop": 216931,
"psavert":           11.5,
"uempmed":              9,
"unemploy": 7744 
},
{
 "date": 2191,
"pce":         1109.5,
"pop": 217095,
"psavert":           11.3,
"uempmed":              9,
"unemploy": 7534 
},
{
 "date": 2222,
"pce":         1110.1,
"pop": 217249,
"psavert":           11.9,
"uempmed":            8.2,
"unemploy": 7326 
},
{
 "date": 2251,
"pce":         1117.3,
"pop": 217381,
"psavert":           11.8,
"uempmed":            8.7,
"unemploy": 7230 
},
{
 "date": 2282,
"pce":         1127.8,
"pop": 217528,
"psavert":           11.2,
"uempmed":            8.2,
"unemploy": 7330 
},
{
 "date": 2312,
"pce":         1125.1,
"pop": 217685,
"psavert":           11.8,
"uempmed":            8.3,
"unemploy": 7053 
},
{
 "date": 2343,
"pce":         1142.9,
"pop": 217861,
"psavert":           10.9,
"uempmed":            7.8,
"unemploy": 7322 
},
{
 "date": 2373,
"pce":         1152.1,
"pop": 218035,
"psavert":           11.2,
"uempmed":            7.7,
"unemploy": 7490 
},
{
 "date": 2404,
"pce":         1160.5,
"pop": 218233,
"psavert":           11.2,
"uempmed":            7.9,
"unemploy": 7518 
},
{
 "date": 2435,
"pce":         1171.4,
"pop": 218440,
"psavert":           10.8,
"uempmed":            7.8,
"unemploy": 7380 
},
{
 "date": 2465,
"pce":         1179.5,
"pop": 218644,
"psavert":           10.6,
"uempmed":            7.7,
"unemploy": 7430 
},
{
 "date": 2496,
"pce":         1191.7,
"pop": 218834,
"psavert":           10.9,
"uempmed":            8.4,
"unemploy": 7620 
},
{
 "date": 2526,
"pce":         1214.1,
"pop": 219006,
"psavert":            9.9,
"uempmed":              8,
"unemploy": 7545 
},
{
 "date": 2557,
"pce":         1217.4,
"pop": 219179,
"psavert":            9.8,
"uempmed":            7.5,
"unemploy": 7280 
},
{
 "date": 2588,
"pce":         1233.7,
"pop": 219344,
"psavert":            8.5,
"uempmed":            7.2,
"unemploy": 7443 
},
{
 "date": 2616,
"pce":         1240.7,
"pop": 219504,
"psavert":            9.8,
"uempmed":            7.2,
"unemploy": 7307 
},
{
 "date": 2647,
"pce":         1249.7,
"pop": 219684,
"psavert":            9.9,
"uempmed":            7.3,
"unemploy": 7059 
},
{
 "date": 2677,
"pce":         1259.6,
"pop": 219859,
"psavert":            9.8,
"uempmed":            7.9,
"unemploy": 6911 
},
{
 "date": 2708,
"pce":         1266.3,
"pop": 220046,
"psavert":           10.2,
"uempmed":            6.2,
"unemploy": 7134 
},
{
 "date": 2738,
"pce":         1283.2,
"pop": 220239,
"psavert":           10.1,
"uempmed":            7.1,
"unemploy": 6829 
},
{
 "date": 2769,
"pce":         1288.5,
"pop": 220458,
"psavert":           10.5,
"uempmed":              7,
"unemploy": 6925 
},
{
 "date": 2800,
"pce":         1297.4,
"pop": 220688,
"psavert":           10.7,
"uempmed":            6.7,
"unemploy": 6751 
},
{
 "date": 2830,
"pce":         1314.3,
"pop": 220904,
"psavert":           10.7,
"uempmed":            6.9,
"unemploy": 6763 
},
{
 "date": 2861,
"pce":           1330,
"pop": 221109,
"psavert":           10.8,
"uempmed":              7,
"unemploy": 6815 
},
{
 "date": 2891,
"pce":         1339.3,
"pop": 221303,
"psavert":             11,
"uempmed":            6.8,
"unemploy": 6386 
},
{
 "date": 2922,
"pce":           1333,
"pop": 221477,
"psavert":           11.4,
"uempmed":            6.5,
"unemploy": 6489 
},
{
 "date": 2953,
"pce":         1358.9,
"pop": 221629,
"psavert":           10.7,
"uempmed":            6.7,
"unemploy": 6318 
},
{
 "date": 2981,
"pce":         1381.4,
"pop": 221792,
"psavert":           10.5,
"uempmed":            6.2,
"unemploy": 6337 
},
{
 "date": 3012,
"pce":         1400.2,
"pop": 221991,
"psavert":           10.3,
"uempmed":            6.1,
"unemploy": 6180 
},
{
 "date": 3042,
"pce":         1415.9,
"pop": 222176,
"psavert":            9.8,
"uempmed":            5.7,
"unemploy": 6127 
},
{
 "date": 3073,
"pce":         1429.8,
"pop": 222379,
"psavert":            9.5,
"uempmed":              6,
"unemploy": 6028 
},
{
 "date": 3103,
"pce":         1430.8,
"pop": 222585,
"psavert":           10.3,
"uempmed":            5.8,
"unemploy": 6309 
},
{
 "date": 3134,
"pce":           1451,
"pop": 222805,
"psavert":            9.9,
"uempmed":            5.8,
"unemploy": 6080 
},
{
 "date": 3165,
"pce":         1456.9,
"pop": 223053,
"psavert":             10,
"uempmed":            5.6,
"unemploy": 6125 
},
{
 "date": 3195,
"pce":           1471,
"pop": 223271,
"psavert":           10.2,
"uempmed":            5.9,
"unemploy": 5947 
},
{
 "date": 3226,
"pce":         1484.7,
"pop": 223477,
"psavert":             10,
"uempmed":            5.5,
"unemploy": 6077 
},
{
 "date": 3256,
"pce":         1500.5,
"pop": 223670,
"psavert":             10,
"uempmed":            5.6,
"unemploy": 6228 
},
{
 "date": 3287,
"pce":         1506.3,
"pop": 223865,
"psavert":           10.6,
"uempmed":            5.9,
"unemploy": 6109 
},
{
 "date": 3318,
"pce":         1521.6,
"pop": 224053,
"psavert":           10.5,
"uempmed":            5.9,
"unemploy": 6173 
},
{
 "date": 3346,
"pce":           1535,
"pop": 224235,
"psavert":           10.7,
"uempmed":            5.9,
"unemploy": 6109 
},
{
 "date": 3377,
"pce":         1542.3,
"pop": 224438,
"psavert":           10.4,
"uempmed":            5.4,
"unemploy": 6069 
},
{
 "date": 3407,
"pce":         1562.7,
"pop": 224632,
"psavert":            9.8,
"uempmed":            5.6,
"unemploy": 5840 
},
{
 "date": 3438,
"pce":         1579.6,
"pop": 224843,
"psavert":            9.3,
"uempmed":            5.6,
"unemploy": 5959 
},
{
 "date": 3468,
"pce":         1590.1,
"pop": 225055,
"psavert":             10,
"uempmed":            5.9,
"unemploy": 5996 
},
{
 "date": 3499,
"pce":         1619.7,
"pop": 225295,
"psavert":            9.2,
"uempmed":            4.8,
"unemploy": 6320 
},
{
 "date": 3530,
"pce":         1638.1,
"pop": 225547,
"psavert":            8.9,
"uempmed":            5.5,
"unemploy": 6190 
},
{
 "date": 3560,
"pce":           1646,
"pop": 225801,
"psavert":            9.3,
"uempmed":            5.5,
"unemploy": 6296 
},
{
 "date": 3591,
"pce":         1661.7,
"pop": 226027,
"psavert":            9.4,
"uempmed":            5.3,
"unemploy": 6238 
},
{
 "date": 3621,
"pce":         1670.7,
"pop": 226243,
"psavert":            9.8,
"uempmed":            5.7,
"unemploy": 6325 
},
{
 "date": 3652,
"pce":         1701.6,
"pop": 226451,
"psavert":            9.6,
"uempmed":            5.3,
"unemploy": 6683 
},
{
 "date": 3683,
"pce":         1705.6,
"pop": 226656,
"psavert":            9.8,
"uempmed":            5.8,
"unemploy": 6702 
},
{
 "date": 3712,
"pce":         1712.4,
"pop": 226849,
"psavert":            9.8,
"uempmed":              6,
"unemploy": 6729 
},
{
 "date": 3743,
"pce":         1699.5,
"pop": 227061,
"psavert":           10.5,
"uempmed":            5.8,
"unemploy": 7358 
},
{
 "date": 3773,
"pce":         1704.3,
"pop": 227251,
"psavert":           10.6,
"uempmed":            5.7,
"unemploy": 7984 
},
{
 "date": 3804,
"pce":           1723,
"pop": 227522,
"psavert":           10.4,
"uempmed":            6.4,
"unemploy": 8098 
},
{
 "date": 3834,
"pce":         1751.2,
"pop": 227726,
"psavert":           10.5,
"uempmed":              7,
"unemploy": 8363 
},
{
 "date": 3865,
"pce":         1767.7,
"pop": 227953,
"psavert":           10.6,
"uempmed":            7.5,
"unemploy": 8281 
},
{
 "date": 3896,
"pce":         1784.1,
"pop": 228186,
"psavert":           11.1,
"uempmed":            7.7,
"unemploy": 8021 
},
{
 "date": 3926,
"pce":         1820.4,
"pop": 228417,
"psavert":             11,
"uempmed":            7.5,
"unemploy": 8088 
},
{
 "date": 3957,
"pce":         1830.2,
"pop": 228612,
"psavert":           11.5,
"uempmed":            7.7,
"unemploy": 8023 
},
{
 "date": 3987,
"pce":         1855.5,
"pop": 228779,
"psavert":           11.2,
"uempmed":            7.5,
"unemploy": 7718 
},
{
 "date": 4018,
"pce":         1874.7,
"pop": 228937,
"psavert":           10.5,
"uempmed":            7.4,
"unemploy": 8071 
},
{
 "date": 4049,
"pce":         1889.4,
"pop": 229071,
"psavert":           10.4,
"uempmed":            7.1,
"unemploy": 8051 
},
{
 "date": 4077,
"pce":         1908.1,
"pop": 229224,
"psavert":           10.3,
"uempmed":            7.1,
"unemploy": 7982 
},
{
 "date": 4108,
"pce":         1909.1,
"pop": 229403,
"psavert":           10.3,
"uempmed":            7.4,
"unemploy": 7869 
},
{
 "date": 4138,
"pce":         1918.2,
"pop": 229575,
"psavert":           10.4,
"uempmed":            6.9,
"unemploy": 8174 
},
{
 "date": 4169,
"pce":         1938.5,
"pop": 229761,
"psavert":           10.2,
"uempmed":            6.6,
"unemploy": 8098 
},
{
 "date": 4199,
"pce":         1945.7,
"pop": 229966,
"psavert":           11.7,
"uempmed":            7.1,
"unemploy": 7863 
},
{
 "date": 4230,
"pce":         1969.8,
"pop": 230187,
"psavert":           11.4,
"uempmed":            7.2,
"unemploy": 8036 
},
{
 "date": 4261,
"pce":         1968.2,
"pop": 230412,
"psavert":           11.9,
"uempmed":            6.8,
"unemploy": 8230 
},
{
 "date": 4291,
"pce":         1966.2,
"pop": 230641,
"psavert":           12.5,
"uempmed":            6.8,
"unemploy": 8646 
},
{
 "date": 4322,
"pce":         1972.4,
"pop": 230822,
"psavert":           12.7,
"uempmed":            6.9,
"unemploy": 9029 
},
{
 "date": 4352,
"pce":         1989.9,
"pop": 230989,
"psavert":             12,
"uempmed":            6.9,
"unemploy": 9267 
},
{
 "date": 4383,
"pce":         1997.4,
"pop": 231157,
"psavert":           12.2,
"uempmed":            7.1,
"unemploy": 9397 
},
{
 "date": 4414,
"pce":         2021.4,
"pop": 231313,
"psavert":           11.6,
"uempmed":            7.5,
"unemploy": 9705 
},
{
 "date": 4442,
"pce":         2024.4,
"pop": 231470,
"psavert":           11.7,
"uempmed":            7.7,
"unemploy": 9895 
},
{
 "date": 4473,
"pce":         2027.2,
"pop": 231645,
"psavert":           12.4,
"uempmed":            8.1,
"unemploy": 10244 
},
{
 "date": 4503,
"pce":         2045.9,
"pop": 231809,
"psavert":           11.7,
"uempmed":            8.5,
"unemploy": 10335 
},
{
 "date": 4534,
"pce":         2050.2,
"pop": 231992,
"psavert":           11.6,
"uempmed":            9.5,
"unemploy": 10538 
},
{
 "date": 4564,
"pce":         2075.1,
"pop": 232188,
"psavert":             12,
"uempmed":            8.5,
"unemploy": 10849 
},
{
 "date": 4595,
"pce":         2083.7,
"pop": 232392,
"psavert":           11.9,
"uempmed":            8.7,
"unemploy": 10881 
},
{
 "date": 4626,
"pce":         2108.9,
"pop": 232599,
"psavert":           11.1,
"uempmed":            9.5,
"unemploy": 11217 
},
{
 "date": 4656,
"pce":         2130.7,
"pop": 232816,
"psavert":           10.7,
"uempmed":            9.7,
"unemploy": 11529 
},
{
 "date": 4687,
"pce":         2154.7,
"pop": 232993,
"psavert":           10.3,
"uempmed":             10,
"unemploy": 11938 
},
{
 "date": 4717,
"pce":         2167.4,
"pop": 233160,
"psavert":           10.3,
"uempmed":           10.2,
"unemploy": 12051 
},
{
 "date": 4748,
"pce":         2180.1,
"pop": 233322,
"psavert":           10.4,
"uempmed":           11.1,
"unemploy": 11534 
},
{
 "date": 4779,
"pce":         2183.1,
"pop": 233473,
"psavert":           10.5,
"uempmed":            9.8,
"unemploy": 11545 
},
{
 "date": 4807,
"pce":         2208.6,
"pop": 233613,
"psavert":             10,
"uempmed":           10.4,
"unemploy": 11408 
},
{
 "date": 4838,
"pce":         2231.8,
"pop": 233781,
"psavert":            9.6,
"uempmed":           10.9,
"unemploy": 11268 
},
{
 "date": 4868,
"pce":           2251,
"pop": 233922,
"psavert":            9.4,
"uempmed":           12.3,
"unemploy": 11154 
},
{
 "date": 4899,
"pce":         2280.8,
"pop": 234118,
"psavert":            8.5,
"uempmed":           11.3,
"unemploy": 11246 
},
{
 "date": 4929,
"pce":           2309,
"pop": 234307,
"psavert":              9,
"uempmed":           10.1,
"unemploy": 10548 
},
{
 "date": 4960,
"pce":         2324.8,
"pop": 234501,
"psavert":            8.7,
"uempmed":            9.3,
"unemploy": 10623 
},
{
 "date": 4991,
"pce":         2339.1,
"pop": 234701,
"psavert":              9,
"uempmed":            9.3,
"unemploy": 10282 
},
{
 "date": 5021,
"pce":         2361.8,
"pop": 234907,
"psavert":            9.1,
"uempmed":            9.4,
"unemploy": 9887 
},
{
 "date": 5052,
"pce":         2370.4,
"pop": 235078,
"psavert":            9.7,
"uempmed":            9.3,
"unemploy": 9499 
},
{
 "date": 5082,
"pce":         2397.9,
"pop": 235235,
"psavert":            9.5,
"uempmed":            8.7,
"unemploy": 9331 
},
{
 "date": 5113,
"pce":         2423.8,
"pop": 235385,
"psavert":            9.4,
"uempmed":            9.1,
"unemploy": 9008 
},
{
 "date": 5144,
"pce":         2408.1,
"pop": 235527,
"psavert":           11.1,
"uempmed":            8.3,
"unemploy": 8791 
},
{
 "date": 5173,
"pce":         2436.4,
"pop": 235675,
"psavert":           10.9,
"uempmed":            8.3,
"unemploy": 8746 
},
{
 "date": 5204,
"pce":         2462.6,
"pop": 235839,
"psavert":           10.9,
"uempmed":            8.2,
"unemploy": 8762 
},
{
 "date": 5234,
"pce":         2479.8,
"pop": 235993,
"psavert":           10.5,
"uempmed":            9.1,
"unemploy": 8456 
},
{
 "date": 5265,
"pce":         2501.2,
"pop": 236160,
"psavert":           10.5,
"uempmed":            7.5,
"unemploy": 8226 
},
{
 "date": 5295,
"pce":         2500.5,
"pop": 236348,
"psavert":             11,
"uempmed":            7.5,
"unemploy": 8537 
},
{
 "date": 5326,
"pce":         2518.4,
"pop": 236549,
"psavert":           11.2,
"uempmed":            7.3,
"unemploy": 8519 
},
{
 "date": 5357,
"pce":         2540.3,
"pop": 236760,
"psavert":           11.2,
"uempmed":            7.6,
"unemploy": 8367 
},
{
 "date": 5387,
"pce":         2538.2,
"pop": 236976,
"psavert":           11.2,
"uempmed":            7.2,
"unemploy": 8381 
},
{
 "date": 5418,
"pce":         2578.6,
"pop": 237159,
"psavert":           10.3,
"uempmed":            7.2,
"unemploy": 8198 
},
{
 "date": 5448,
"pce":           2590,
"pop": 237316,
"psavert":           10.6,
"uempmed":            7.3,
"unemploy": 8358 
},
{
 "date": 5479,
"pce":         2626.3,
"pop": 237468,
"psavert":            9.7,
"uempmed":            6.8,
"unemploy": 8423 
},
{
 "date": 5510,
"pce":         2648.6,
"pop": 237602,
"psavert":            8.5,
"uempmed":            7.1,
"unemploy": 8321 
},
{
 "date": 5538,
"pce":         2656.8,
"pop": 237732,
"psavert":            8.1,
"uempmed":            7.1,
"unemploy": 8339 
},
{
 "date": 5569,
"pce":         2668.4,
"pop": 237900,
"psavert":            9.4,
"uempmed":            6.9,
"unemploy": 8395 
},
{
 "date": 5599,
"pce":         2705.9,
"pop": 238074,
"psavert":           10.5,
"uempmed":            6.9,
"unemploy": 8302 
},
{
 "date": 5630,
"pce":         2699.3,
"pop": 238270,
"psavert":              9,
"uempmed":            6.6,
"unemploy": 8460 
},
{
 "date": 5660,
"pce":         2725.9,
"pop": 238466,
"psavert":            8.5,
"uempmed":            6.9,
"unemploy": 8513 
},
{
 "date": 5691,
"pce":         2762.7,
"pop": 238679,
"psavert":            7.5,
"uempmed":            7.1,
"unemploy": 8196 
},
{
 "date": 5722,
"pce":         2805.6,
"pop": 238898,
"psavert":            6.7,
"uempmed":            6.9,
"unemploy": 8248 
},
{
 "date": 5752,
"pce":         2767.1,
"pop": 239113,
"psavert":            8.5,
"uempmed":            7.1,
"unemploy": 8298 
},
{
 "date": 5783,
"pce":         2782.7,
"pop": 239307,
"psavert":            8.4,
"uempmed":              7,
"unemploy": 8128 
},
{
 "date": 5813,
"pce":         2822.8,
"pop": 239477,
"psavert":              8,
"uempmed":            6.8,
"unemploy": 8138 
},
{
 "date": 5844,
"pce":         2838.3,
"pop": 239638,
"psavert":              8,
"uempmed":            6.7,
"unemploy": 7795 
},
{
 "date": 5875,
"pce":         2831.2,
"pop": 239788,
"psavert":            8.7,
"uempmed":            6.9,
"unemploy": 8402 
},
{
 "date": 5903,
"pce":         2834.7,
"pop": 239928,
"psavert":            9.3,
"uempmed":            6.8,
"unemploy": 8383 
},
{
 "date": 5934,
"pce":         2846.5,
"pop": 240094,
"psavert":            9.1,
"uempmed":            6.7,
"unemploy": 8364 
},
{
 "date": 5964,
"pce":           2869,
"pop": 240271,
"psavert":            8.6,
"uempmed":            6.8,
"unemploy": 8439 
},
{
 "date": 5995,
"pce":         2873.5,
"pop": 240459,
"psavert":            8.8,
"uempmed":              7,
"unemploy": 8508 
},
{
 "date": 6025,
"pce":         2893.4,
"pop": 240651,
"psavert":            8.7,
"uempmed":            6.9,
"unemploy": 8319 
},
{
 "date": 6056,
"pce":         2911.1,
"pop": 240854,
"psavert":            8.4,
"uempmed":            7.1,
"unemploy": 8135 
},
{
 "date": 6087,
"pce":         2984.6,
"pop": 241068,
"psavert":            6.6,
"uempmed":            7.4,
"unemploy": 8310 
},
{
 "date": 6117,
"pce":         2945.9,
"pop": 241274,
"psavert":            7.8,
"uempmed":              7,
"unemploy": 8243 
},
{
 "date": 6148,
"pce":         2941.7,
"pop": 241467,
"psavert":            8.2,
"uempmed":            7.1,
"unemploy": 8159 
},
{
 "date": 6178,
"pce":         3010.8,
"pop": 241620,
"psavert":            6.4,
"uempmed":            7.1,
"unemploy": 7883 
},
{
 "date": 6209,
"pce":         2949.9,
"pop": 241784,
"psavert":            9.1,
"uempmed":            6.9,
"unemploy": 7892 
},
{
 "date": 6240,
"pce":         3016.5,
"pop": 241930,
"psavert":            7.9,
"uempmed":            6.6,
"unemploy": 7865 
},
{
 "date": 6268,
"pce":         3028.4,
"pop": 242079,
"psavert":            7.9,
"uempmed":            6.6,
"unemploy": 7862 
},
{
 "date": 6299,
"pce":         3054.1,
"pop": 242252,
"psavert":            3.8,
"uempmed":            7.1,
"unemploy": 7542 
},
{
 "date": 6329,
"pce":         3063.9,
"pop": 242423,
"psavert":            7.5,
"uempmed":            6.6,
"unemploy": 7574 
},
{
 "date": 6360,
"pce":         3088.4,
"pop": 242608,
"psavert":              7,
"uempmed":            6.5,
"unemploy": 7398 
},
{
 "date": 6390,
"pce":         3110.7,
"pop": 242804,
"psavert":            6.8,
"uempmed":            6.5,
"unemploy": 7268 
},
{
 "date": 6421,
"pce":           3147,
"pop": 243012,
"psavert":            6.5,
"uempmed":            6.4,
"unemploy": 7261 
},
{
 "date": 6452,
"pce":         3142.9,
"pop": 243223,
"psavert":              7,
"uempmed":              6,
"unemploy": 7102 
},
{
 "date": 6482,
"pce":         3151.1,
"pop": 243446,
"psavert":            7.6,
"uempmed":            6.3,
"unemploy": 7227 
},
{
 "date": 6513,
"pce":         3160.9,
"pop": 243639,
"psavert":            7.9,
"uempmed":            6.2,
"unemploy": 7035 
},
{
 "date": 6543,
"pce":         3190.9,
"pop": 243809,
"psavert":              8,
"uempmed":              6,
"unemploy": 6936 
},
{
 "date": 6574,
"pce":         3230.7,
"pop": 243981,
"psavert":            7.5,
"uempmed":            6.2,
"unemploy": 6953 
},
{
 "date": 6605,
"pce":         3238.5,
"pop": 244131,
"psavert":            7.9,
"uempmed":            6.3,
"unemploy": 6929 
},
{
 "date": 6634,
"pce":         3277.8,
"pop": 244279,
"psavert":            7.5,
"uempmed":            6.4,
"unemploy": 6876 
},
{
 "date": 6665,
"pce":         3280.4,
"pop": 244445,
"psavert":            8.1,
"uempmed":            5.9,
"unemploy": 6601 
},
{
 "date": 6695,
"pce":           3311,
"pop": 244610,
"psavert":            7.7,
"uempmed":            5.9,
"unemploy": 6779 
},
{
 "date": 6726,
"pce":         3335.6,
"pop": 244806,
"psavert":            7.8,
"uempmed":            5.8,
"unemploy": 6546 
},
{
 "date": 6756,
"pce":         3359.3,
"pop": 245021,
"psavert":            7.9,
"uempmed":            6.1,
"unemploy": 6605 
},
{
 "date": 6787,
"pce":         3384.3,
"pop": 245240,
"psavert":            7.8,
"uempmed":            5.9,
"unemploy": 6843 
},
{
 "date": 6818,
"pce":         3391.4,
"pop": 245464,
"psavert":            8.2,
"uempmed":            5.7,
"unemploy": 6604 
},
{
 "date": 6848,
"pce":         3430.4,
"pop": 245693,
"psavert":              8,
"uempmed":            5.6,
"unemploy": 6568 
},
{
 "date": 6879,
"pce":           3447,
"pop": 245884,
"psavert":            7.7,
"uempmed":            5.7,
"unemploy": 6537 
},
{
 "date": 6909,
"pce":         3476.3,
"pop": 246056,
"psavert":            7.7,
"uempmed":            5.9,
"unemploy": 6518 
},
{
 "date": 6940,
"pce":         3499.9,
"pop": 246224,
"psavert":              8,
"uempmed":            5.6,
"unemploy": 6682 
},
{
 "date": 6971,
"pce":         3503.9,
"pop": 246378,
"psavert":            8.4,
"uempmed":            5.4,
"unemploy": 6359 
},
{
 "date": 6999,
"pce":         3514.5,
"pop": 246530,
"psavert":            8.9,
"uempmed":            5.4,
"unemploy": 6205 
},
{
 "date": 7030,
"pce":         3558.6,
"pop": 246721,
"psavert":            7.9,
"uempmed":            5.4,
"unemploy": 6468 
},
{
 "date": 7060,
"pce":         3567.5,
"pop": 246906,
"psavert":            7.6,
"uempmed":            5.3,
"unemploy": 6375 
},
{
 "date": 7091,
"pce":         3582.4,
"pop": 247114,
"psavert":            7.6,
"uempmed":            5.4,
"unemploy": 6577 
},
{
 "date": 7121,
"pce":         3601.7,
"pop": 247342,
"psavert":            7.7,
"uempmed":            5.6,
"unemploy": 6495 
},
{
 "date": 7152,
"pce":         3636.8,
"pop": 247573,
"psavert":            7.1,
"uempmed":              5,
"unemploy": 6511 
},
{
 "date": 7183,
"pce":         3638.1,
"pop": 247816,
"psavert":            7.5,
"uempmed":            4.9,
"unemploy": 6590 
},
{
 "date": 7213,
"pce":           3650,
"pop": 248067,
"psavert":            7.9,
"uempmed":            4.9,
"unemploy": 6630 
},
{
 "date": 7244,
"pce":         3659.7,
"pop": 248281,
"psavert":              8,
"uempmed":            4.8,
"unemploy": 6725 
},
{
 "date": 7274,
"pce":         3700.7,
"pop": 248479,
"psavert":            7.2,
"uempmed":            4.9,
"unemploy": 6667 
},
{
 "date": 7305,
"pce":         3747.2,
"pop": 248659,
"psavert":            7.4,
"uempmed":            5.1,
"unemploy": 6752 
},
{
 "date": 7336,
"pce":         3744.8,
"pop": 248827,
"psavert":            8.1,
"uempmed":            5.3,
"unemploy": 6651 
},
{
 "date": 7364,
"pce":         3771.5,
"pop": 249012,
"psavert":            7.8,
"uempmed":            5.1,
"unemploy": 6598 
},
{
 "date": 7395,
"pce":         3786.7,
"pop": 249306,
"psavert":            8.2,
"uempmed":            4.8,
"unemploy": 6797 
},
{
 "date": 7425,
"pce":         3792.5,
"pop": 249565,
"psavert":              8,
"uempmed":            5.2,
"unemploy": 6742 
},
{
 "date": 7456,
"pce":         3821.3,
"pop": 249849,
"psavert":            7.9,
"uempmed":            5.2,
"unemploy": 6590 
},
{
 "date": 7486,
"pce":         3838.5,
"pop": 250132,
"psavert":              8,
"uempmed":            5.4,
"unemploy": 6922 
},
{
 "date": 7517,
"pce":           3865,
"pop": 250439,
"psavert":            7.5,
"uempmed":            5.4,
"unemploy": 7188 
},
{
 "date": 7548,
"pce":         3886.7,
"pop": 250751,
"psavert":            7.5,
"uempmed":            5.6,
"unemploy": 7368 
},
{
 "date": 7578,
"pce":         3887.1,
"pop": 251057,
"psavert":            7.3,
"uempmed":            5.8,
"unemploy": 7459 
},
{
 "date": 7609,
"pce":         3888.4,
"pop": 251346,
"psavert":            7.4,
"uempmed":            5.7,
"unemploy": 7764 
},
{
 "date": 7639,
"pce":         3877.8,
"pop": 251626,
"psavert":            8.2,
"uempmed":            5.9,
"unemploy": 7901 
},
{
 "date": 7670,
"pce":         3857.6,
"pop": 251889,
"psavert":            8.7,
"uempmed":              6,
"unemploy": 8015 
},
{
 "date": 7701,
"pce":         3883.3,
"pop": 252135,
"psavert":            8.3,
"uempmed":            6.2,
"unemploy": 8265 
},
{
 "date": 7729,
"pce":         3929.7,
"pop": 252372,
"psavert":            7.4,
"uempmed":            6.7,
"unemploy": 8586 
},
{
 "date": 7760,
"pce":         3923.9,
"pop": 252643,
"psavert":              8,
"uempmed":            6.6,
"unemploy": 8439 
},
{
 "date": 7790,
"pce":           3950,
"pop": 252913,
"psavert":            7.7,
"uempmed":            6.4,
"unemploy": 8736 
},
{
 "date": 7821,
"pce":         3957.1,
"pop": 253207,
"psavert":            8.2,
"uempmed":            6.9,
"unemploy": 8692 
},
{
 "date": 7851,
"pce":         3982.4,
"pop": 253493,
"psavert":            7.6,
"uempmed":              7,
"unemploy": 8586 
},
{
 "date": 7882,
"pce":         3985.4,
"pop": 253807,
"psavert":            7.9,
"uempmed":            7.3,
"unemploy": 8666 
},
{
 "date": 7913,
"pce":         4001.2,
"pop": 254126,
"psavert":            8.1,
"uempmed":            6.8,
"unemploy": 8722 
},
{
 "date": 7943,
"pce":         3992.9,
"pop": 254435,
"psavert":            8.6,
"uempmed":            7.2,
"unemploy": 8842 
},
{
 "date": 7974,
"pce":         4020.6,
"pop": 254718,
"psavert":            8.4,
"uempmed":            7.5,
"unemploy": 8931 
},
{
 "date": 8004,
"pce":         4037.7,
"pop": 254964,
"psavert":            9.1,
"uempmed":            7.8,
"unemploy": 9198 
},
{
 "date": 8035,
"pce":         4101.9,
"pop": 255214,
"psavert":            8.8,
"uempmed":            8.1,
"unemploy": 9283 
},
{
 "date": 8066,
"pce":         4116.8,
"pop": 255448,
"psavert":            9.2,
"uempmed":            8.2,
"unemploy": 9454 
},
{
 "date": 8095,
"pce":         4134.3,
"pop": 255703,
"psavert":            9.2,
"uempmed":            8.3,
"unemploy": 9460 
},
{
 "date": 8126,
"pce":           4149,
"pop": 255992,
"psavert":            9.4,
"uempmed":            8.5,
"unemploy": 9415 
},
{
 "date": 8156,
"pce":         4176.1,
"pop": 256285,
"psavert":            9.4,
"uempmed":            8.8,
"unemploy": 9744 
},
{
 "date": 8187,
"pce":           4195,
"pop": 256589,
"psavert":            9.5,
"uempmed":            8.7,
"unemploy": 10040 
},
{
 "date": 8217,
"pce":           4223,
"pop": 256894,
"psavert":            9.1,
"uempmed":            8.6,
"unemploy": 9850 
},
{
 "date": 8248,
"pce":         4239.3,
"pop": 257232,
"psavert":            9.2,
"uempmed":            8.8,
"unemploy": 9787 
},
{
 "date": 8279,
"pce":         4273.9,
"pop": 257548,
"psavert":            8.2,
"uempmed":            8.6,
"unemploy": 9781 
},
{
 "date": 8309,
"pce":         4303.5,
"pop": 257861,
"psavert":            7.4,
"uempmed":              9,
"unemploy": 9398 
},
{
 "date": 8340,
"pce":         4319.5,
"pop": 258147,
"psavert":            7.3,
"uempmed":              9,
"unemploy": 9565 
},
{
 "date": 8370,
"pce":         4355.6,
"pop": 258413,
"psavert":            9.9,
"uempmed":            9.3,
"unemploy": 9557 
},
{
 "date": 8401,
"pce":         4359.7,
"pop": 258679,
"psavert":            8.3,
"uempmed":            8.6,
"unemploy": 9325 
},
{
 "date": 8432,
"pce":         4374.3,
"pop": 258919,
"psavert":            8.4,
"uempmed":            8.5,
"unemploy": 9183 
},
{
 "date": 8460,
"pce":         4371.4,
"pop": 259152,
"psavert":            8.3,
"uempmed":            8.5,
"unemploy": 9056 
},
{
 "date": 8491,
"pce":         4412.4,
"pop": 259414,
"psavert":            8.2,
"uempmed":            8.4,
"unemploy": 9110 
},
{
 "date": 8521,
"pce":         4441.3,
"pop": 259680,
"psavert":            7.7,
"uempmed":            8.1,
"unemploy": 9149 
},
{
 "date": 8552,
"pce":         4458.8,
"pop": 259963,
"psavert":            7.2,
"uempmed":            8.3,
"unemploy": 9121 
},
{
 "date": 8582,
"pce":         4487.7,
"pop": 260255,
"psavert":              7,
"uempmed":            8.2,
"unemploy": 8930 
},
{
 "date": 8613,
"pce":         4499.9,
"pop": 260566,
"psavert":            7.1,
"uempmed":            8.2,
"unemploy": 8763 
},
{
 "date": 8644,
"pce":         4530.5,
"pop": 260867,
"psavert":            6.3,
"uempmed":            8.3,
"unemploy": 8714 
},
{
 "date": 8674,
"pce":           4552,
"pop": 261163,
"psavert":            5.6,
"uempmed":              8,
"unemploy": 8750 
},
{
 "date": 8705,
"pce":         4573.4,
"pop": 261425,
"psavert":            5.6,
"uempmed":            8.3,
"unemploy": 8542 
},
{
 "date": 8735,
"pce":         4590.7,
"pop": 261674,
"psavert":            8.4,
"uempmed":            8.3,
"unemploy": 8477 
},
{
 "date": 8766,
"pce":         4604.8,
"pop": 261919,
"psavert":            6.4,
"uempmed":            8.6,
"unemploy": 8630 
},
{
 "date": 8797,
"pce":         4652.3,
"pop": 262123,
"psavert":            5.9,
"uempmed":            9.2,
"unemploy": 8583 
},
{
 "date": 8825,
"pce":         4665.4,
"pop": 262352,
"psavert":            6.2,
"uempmed":            9.3,
"unemploy": 8470 
},
{
 "date": 8856,
"pce":         4690.7,
"pop": 262631,
"psavert":            5.8,
"uempmed":            9.1,
"unemploy": 8331 
},
{
 "date": 8886,
"pce":         4689.2,
"pop": 262877,
"psavert":            7.1,
"uempmed":            9.2,
"unemploy": 7915 
},
{
 "date": 8917,
"pce":         4728.8,
"pop": 263152,
"psavert":            6.3,
"uempmed":            9.3,
"unemploy": 7927 
},
{
 "date": 8947,
"pce":         4740.8,
"pop": 263436,
"psavert":            6.4,
"uempmed":              9,
"unemploy": 7946 
},
{
 "date": 8978,
"pce":           4783,
"pop": 263724,
"psavert":            5.9,
"uempmed":            8.9,
"unemploy": 7933 
},
{
 "date": 9009,
"pce":         4795.5,
"pop": 264017,
"psavert":            6.2,
"uempmed":            9.2,
"unemploy": 7734 
},
{
 "date": 9039,
"pce":         4833.3,
"pop": 264301,
"psavert":            6.5,
"uempmed":             10,
"unemploy": 7632 
},
{
 "date": 9070,
"pce":           4846,
"pop": 264559,
"psavert":            6.4,
"uempmed":              9,
"unemploy": 7375 
},
{
 "date": 9100,
"pce":         4862.3,
"pop": 264804,
"psavert":            6.5,
"uempmed":            8.7,
"unemploy": 7230 
},
{
 "date": 9131,
"pce":         4871.9,
"pop": 265044,
"psavert":            6.9,
"uempmed":              8,
"unemploy": 7375 
},
{
 "date": 9162,
"pce":         4871.7,
"pop": 265270,
"psavert":            7.3,
"uempmed":            8.1,
"unemploy": 7187 
},
{
 "date": 9190,
"pce":         4906.5,
"pop": 265495,
"psavert":              7,
"uempmed":            8.3,
"unemploy": 7153 
},
{
 "date": 9221,
"pce":         4911.5,
"pop": 265755,
"psavert":            6.3,
"uempmed":            8.3,
"unemploy": 7645 
},
{
 "date": 9251,
"pce":         4954.4,
"pop": 265998,
"psavert":            6.5,
"uempmed":            9.1,
"unemploy": 7430 
},
{
 "date": 9282,
"pce":           4999,
"pop": 266270,
"psavert":            6.1,
"uempmed":            7.9,
"unemploy": 7427 
},
{
 "date": 9312,
"pce":         4991.8,
"pop": 266557,
"psavert":            6.4,
"uempmed":            8.5,
"unemploy": 7527 
},
{
 "date": 9343,
"pce":         5027.1,
"pop": 266843,
"psavert":            6.1,
"uempmed":            8.3,
"unemploy": 7484 
},
{
 "date": 9374,
"pce":         5042.5,
"pop": 267152,
"psavert":            6.1,
"uempmed":            7.9,
"unemploy": 7478 
},
{
 "date": 9404,
"pce":         5035.9,
"pop": 267456,
"psavert":            6.5,
"uempmed":            8.2,
"unemploy": 7328 
},
{
 "date": 9435,
"pce":         5077.8,
"pop": 267715,
"psavert":              6,
"uempmed":              8,
"unemploy": 7426 
},
{
 "date": 9465,
"pce":         5120.1,
"pop": 267943,
"psavert":            5.5,
"uempmed":            8.3,
"unemploy": 7423 
},
{
 "date": 9496,
"pce":         5108.9,
"pop": 268151,
"psavert":            6.1,
"uempmed":            8.3,
"unemploy": 7491 
},
{
 "date": 9527,
"pce":         5156.1,
"pop": 268364,
"psavert":            6.1,
"uempmed":            7.8,
"unemploy": 7313 
},
{
 "date": 9556,
"pce":         5196.4,
"pop": 268595,
"psavert":              6,
"uempmed":            8.3,
"unemploy": 7318 
},
{
 "date": 9587,
"pce":         5231.6,
"pop": 268853,
"psavert":              5,
"uempmed":            8.6,
"unemploy": 7415 
},
{
 "date": 9617,
"pce":         5247.2,
"pop": 269108,
"psavert":            6.1,
"uempmed":            8.6,
"unemploy": 7423 
},
{
 "date": 9648,
"pce":         5253.7,
"pop": 269386,
"psavert":            6.5,
"uempmed":            8.3,
"unemploy": 7095 
},
{
 "date": 9678,
"pce":         5275.8,
"pop": 269667,
"psavert":            6.1,
"uempmed":            8.3,
"unemploy": 7337 
},
{
 "date": 9709,
"pce":           5299,
"pop": 269976,
"psavert":            5.9,
"uempmed":            8.4,
"unemploy": 6882 
},
{
 "date": 9740,
"pce":           5320,
"pop": 270284,
"psavert":              6,
"uempmed":            8.5,
"unemploy": 6979 
},
{
 "date": 9770,
"pce":         5351.5,
"pop": 270581,
"psavert":            5.8,
"uempmed":            8.3,
"unemploy": 7031 
},
{
 "date": 9801,
"pce":           5375,
"pop": 270878,
"psavert":            5.8,
"uempmed":            7.7,
"unemploy": 7236 
},
{
 "date": 9831,
"pce":         5401.7,
"pop": 271125,
"psavert":            5.7,
"uempmed":            7.8,
"unemploy": 7253 
},
{
 "date": 9862,
"pce":         5434.9,
"pop": 271360,
"psavert":            5.6,
"uempmed":            7.8,
"unemploy": 7158 
},
{
 "date": 9893,
"pce":         5457.7,
"pop": 271585,
"psavert":            5.7,
"uempmed":            8.1,
"unemploy": 7102 
},
{
 "date": 9921,
"pce":         5477.6,
"pop": 271821,
"psavert":            5.8,
"uempmed":            7.9,
"unemploy": 7000 
},
{
 "date": 9952,
"pce":         5482.8,
"pop": 272083,
"psavert":            5.9,
"uempmed":            8.3,
"unemploy": 6873 
},
{
 "date": 9982,
"pce":         5484.3,
"pop": 272342,
"psavert":            6.2,
"uempmed":              8,
"unemploy": 6655 
},
{
 "date": 10013,
"pce":         5518.2,
"pop": 272622,
"psavert":              6,
"uempmed":              8,
"unemploy": 6799 
},
{
 "date": 10043,
"pce":           5573,
"pop": 272912,
"psavert":            5.5,
"uempmed":            8.3,
"unemploy": 6655 
},
{
 "date": 10074,
"pce":         5611.8,
"pop": 273237,
"psavert":            5.4,
"uempmed":            7.8,
"unemploy": 6608 
},
{
 "date": 10105,
"pce":         5625.6,
"pop": 273553,
"psavert":            5.6,
"uempmed":            8.2,
"unemploy": 6656 
},
{
 "date": 10135,
"pce":         5661.2,
"pop": 273852,
"psavert":            5.6,
"uempmed":            7.7,
"unemploy": 6454 
},
{
 "date": 10166,
"pce":         5685.2,
"pop": 274126,
"psavert":            5.8,
"uempmed":            7.6,
"unemploy": 6308 
},
{
 "date": 10196,
"pce":         5716.4,
"pop": 274372,
"psavert":            5.8,
"uempmed":            7.5,
"unemploy": 6476 
},
{
 "date": 10227,
"pce":         5714.4,
"pop": 274626,
"psavert":            6.7,
"uempmed":            7.4,
"unemploy": 6368 
},
{
 "date": 10258,
"pce":         5748.4,
"pop": 274838,
"psavert":            6.8,
"uempmed":              7,
"unemploy": 6306 
},
{
 "date": 10286,
"pce":           5775,
"pop": 275047,
"psavert":            6.9,
"uempmed":            6.8,
"unemploy": 6422 
},
{
 "date": 10317,
"pce":         5812.9,
"pop": 275304,
"psavert":            6.6,
"uempmed":            6.7,
"unemploy": 5941 
},
{
 "date": 10347,
"pce":         5863.3,
"pop": 275564,
"psavert":            6.3,
"uempmed":              6,
"unemploy": 6047 
},
{
 "date": 10378,
"pce":         5897.2,
"pop": 275836,
"psavert":            6.2,
"uempmed":            6.9,
"unemploy": 6212 
},
{
 "date": 10408,
"pce":         5915.6,
"pop": 276115,
"psavert":            6.3,
"uempmed":            6.7,
"unemploy": 6259 
},
{
 "date": 10439,
"pce":           5951,
"pop": 276418,
"psavert":            6.2,
"uempmed":            6.8,
"unemploy": 6179 
},
{
 "date": 10470,
"pce":         5991.8,
"pop": 276714,
"psavert":            5.8,
"uempmed":            6.7,
"unemploy": 6300 
},
{
 "date": 10500,
"pce":         6025.8,
"pop": 277003,
"psavert":            5.6,
"uempmed":            5.8,
"unemploy": 6280 
},
{
 "date": 10531,
"pce":         6042.7,
"pop": 277277,
"psavert":            5.7,
"uempmed":            6.6,
"unemploy": 6100 
},
{
 "date": 10561,
"pce":         6098.2,
"pop": 277526,
"psavert":            5.2,
"uempmed":            6.8,
"unemploy": 6032 
},
{
 "date": 10592,
"pce":         6099.1,
"pop": 277790,
"psavert":            5.7,
"uempmed":            6.9,
"unemploy": 5976 
},
{
 "date": 10623,
"pce":         6128.2,
"pop": 277992,
"psavert":            5.6,
"uempmed":            6.8,
"unemploy": 6111 
},
{
 "date": 10651,
"pce":         6159.7,
"pop": 278198,
"psavert":            5.3,
"uempmed":            6.8,
"unemploy": 5783 
},
{
 "date": 10682,
"pce":         6223.6,
"pop": 278451,
"psavert":            4.5,
"uempmed":            6.2,
"unemploy": 6004 
},
{
 "date": 10712,
"pce":         6253.4,
"pop": 278717,
"psavert":            4.3,
"uempmed":            6.5,
"unemploy": 5796 
},
{
 "date": 10743,
"pce":         6281.9,
"pop": 279001,
"psavert":            4.2,
"uempmed":            6.3,
"unemploy": 5951 
},
{
 "date": 10773,
"pce":         6309.5,
"pop": 279295,
"psavert":            4.1,
"uempmed":            5.8,
"unemploy": 6025 
},
{
 "date": 10804,
"pce":         6354.8,
"pop": 279602,
"psavert":              4,
"uempmed":            6.5,
"unemploy": 5838 
},
{
 "date": 10835,
"pce":         6407.4,
"pop": 279903,
"psavert":            3.5,
"uempmed":              6,
"unemploy": 5915 
},
{
 "date": 10865,
"pce":         6431.2,
"pop": 280203,
"psavert":            3.9,
"uempmed":            6.1,
"unemploy": 5778 
},
{
 "date": 10896,
"pce":         6467.2,
"pop": 280471,
"psavert":            4.1,
"uempmed":            6.2,
"unemploy": 5716 
},
{
 "date": 10926,
"pce":         6568.2,
"pop": 280716,
"psavert":            3.7,
"uempmed":            5.8,
"unemploy": 5653 
},
{
 "date": 10957,
"pce":         6564.7,
"pop": 280976,
"psavert":            4.7,
"uempmed":            5.8,
"unemploy": 5708 
},
{
 "date": 10988,
"pce":         6648.7,
"pop": 281190,
"psavert":            4.2,
"uempmed":            6.1,
"unemploy": 5858 
},
{
 "date": 11017,
"pce":         6714.8,
"pop": 281409,
"psavert":            3.9,
"uempmed":              6,
"unemploy": 5733 
},
{
 "date": 11048,
"pce":           6701,
"pop": 281653,
"psavert":            4.4,
"uempmed":            6.1,
"unemploy": 5481 
},
{
 "date": 11078,
"pce":         6737.2,
"pop": 281877,
"psavert":            4.2,
"uempmed":            5.8,
"unemploy": 5758 
},
{
 "date": 11109,
"pce":         6773.6,
"pop": 282126,
"psavert":            4.2,
"uempmed":            5.7,
"unemploy": 5651 
},
{
 "date": 11139,
"pce":         6793.7,
"pop": 282385,
"psavert":            4.6,
"uempmed":              6,
"unemploy": 5747 
},
{
 "date": 11170,
"pce":         6828.7,
"pop": 282653,
"psavert":            4.6,
"uempmed":            6.3,
"unemploy": 5853 
},
{
 "date": 11201,
"pce":         6913.1,
"pop": 282932,
"psavert":            3.8,
"uempmed":            5.2,
"unemploy": 5625 
},
{
 "date": 11231,
"pce":         6919.6,
"pop": 283201,
"psavert":              4,
"uempmed":            6.1,
"unemploy": 5534 
},
{
 "date": 11262,
"pce":         6934.5,
"pop": 283453,
"psavert":            3.8,
"uempmed":            6.1,
"unemploy": 5639 
},
{
 "date": 11292,
"pce":         6979.1,
"pop": 283696,
"psavert":            3.5,
"uempmed":              6,
"unemploy": 5634 
},
{
 "date": 11323,
"pce":         7009.8,
"pop": 283920,
"psavert":              4,
"uempmed":            5.8,
"unemploy": 6023 
},
{
 "date": 11354,
"pce":         7029.3,
"pop": 284137,
"psavert":            4.1,
"uempmed":            6.1,
"unemploy": 6089 
},
{
 "date": 11382,
"pce":         7022.1,
"pop": 284350,
"psavert":            4.5,
"uempmed":            6.6,
"unemploy": 6141 
},
{
 "date": 11413,
"pce":         7036.2,
"pop": 284581,
"psavert":            4.3,
"uempmed":            5.9,
"unemploy": 6271 
},
{
 "date": 11443,
"pce":         7083.1,
"pop": 284810,
"psavert":            3.7,
"uempmed":            6.3,
"unemploy": 6226 
},
{
 "date": 11474,
"pce":         7097.1,
"pop": 285062,
"psavert":            3.7,
"uempmed":              6,
"unemploy": 6484 
},
{
 "date": 11504,
"pce":         7109.2,
"pop": 285309,
"psavert":              5,
"uempmed":            6.8,
"unemploy": 6583 
},
{
 "date": 11535,
"pce":         7146.1,
"pop": 285570,
"psavert":            6.1,
"uempmed":            6.9,
"unemploy": 7042 
},
{
 "date": 11566,
"pce":         7054.8,
"pop": 285843,
"psavert":            6.3,
"uempmed":            7.2,
"unemploy": 7142 
},
{
 "date": 11596,
"pce":         7250.2,
"pop": 286098,
"psavert":            2.7,
"uempmed":            7.3,
"unemploy": 7694 
},
{
 "date": 11627,
"pce":         7209.6,
"pop": 286341,
"psavert":            3.4,
"uempmed":            7.7,
"unemploy": 8003 
},
{
 "date": 11657,
"pce":           7190,
"pop": 286570,
"psavert":            3.8,
"uempmed":            8.2,
"unemploy": 8258 
},
{
 "date": 11688,
"pce":         7217.7,
"pop": 286788,
"psavert":            5.6,
"uempmed":            8.4,
"unemploy": 8182 
},
{
 "date": 11719,
"pce":         7259.7,
"pop": 286994,
"psavert":            5.3,
"uempmed":            8.3,
"unemploy": 8215 
},
{
 "date": 11747,
"pce":         7276.7,
"pop": 287190,
"psavert":            5.3,
"uempmed":            8.4,
"unemploy": 8304 
},
{
 "date": 11778,
"pce":         7345.6,
"pop": 287397,
"psavert":            5.1,
"uempmed":            8.9,
"unemploy": 8599 
},
{
 "date": 11808,
"pce":         7321.8,
"pop": 287623,
"psavert":            5.6,
"uempmed":            9.5,
"unemploy": 8399 
},
{
 "date": 11839,
"pce":         7366.1,
"pop": 287864,
"psavert":            5.4,
"uempmed":             11,
"unemploy": 8393 
},
{
 "date": 11869,
"pce":         7424.2,
"pop": 288105,
"psavert":            4.6,
"uempmed":            8.9,
"unemploy": 8390 
},
{
 "date": 11900,
"pce":           7449,
"pop": 288360,
"psavert":            4.4,
"uempmed":              9,
"unemploy": 8304 
},
{
 "date": 11931,
"pce":         7426.1,
"pop": 288618,
"psavert":            4.9,
"uempmed":            9.5,
"unemploy": 8251 
},
{
 "date": 11961,
"pce":         7469.3,
"pop": 288870,
"psavert":            4.7,
"uempmed":            9.6,
"unemploy": 8307 
},
{
 "date": 11992,
"pce":         7499.8,
"pop": 289106,
"psavert":            4.7,
"uempmed":            9.3,
"unemploy": 8520 
},
{
 "date": 12022,
"pce":         7552.6,
"pop": 289313,
"psavert":            4.5,
"uempmed":            9.6,
"unemploy": 8640 
},
{
 "date": 12053,
"pce":         7579.5,
"pop": 289518,
"psavert":            4.5,
"uempmed":            9.6,
"unemploy": 8520 
},
{
 "date": 12084,
"pce":         7573.6,
"pop": 289714,
"psavert":            4.7,
"uempmed":            9.5,
"unemploy": 8618 
},
{
 "date": 12112,
"pce":         7627.5,
"pop": 289911,
"psavert":            4.6,
"uempmed":            9.7,
"unemploy": 8588 
},
{
 "date": 12143,
"pce":         7661.7,
"pop": 290125,
"psavert":            4.6,
"uempmed":           10.2,
"unemploy": 8842 
},
{
 "date": 12173,
"pce":         7669.2,
"pop": 290346,
"psavert":            5.1,
"uempmed":            9.9,
"unemploy": 8957 
},
{
 "date": 12204,
"pce":         7722.9,
"pop": 290584,
"psavert":            4.9,
"uempmed":           11.5,
"unemploy": 9266 
},
{
 "date": 12234,
"pce":         7783.8,
"pop": 290820,
"psavert":            5.5,
"uempmed":           10.3,
"unemploy": 9011 
},
{
 "date": 12265,
"pce":         7878.9,
"pop": 291072,
"psavert":            5.3,
"uempmed":           10.1,
"unemploy": 8896 
},
{
 "date": 12296,
"pce":           7874,
"pop": 291321,
"psavert":            4.5,
"uempmed":           10.2,
"unemploy": 8921 
},
{
 "date": 12326,
"pce":         7890.6,
"pop": 291574,
"psavert":            4.6,
"uempmed":           10.4,
"unemploy": 8732 
},
{
 "date": 12357,
"pce":         7950.4,
"pop": 291807,
"psavert":            4.7,
"uempmed":           10.3,
"unemploy": 8576 
},
{
 "date": 12387,
"pce":         7974.3,
"pop": 292008,
"psavert":            4.8,
"uempmed":           10.4,
"unemploy": 8317 
},
{
 "date": 12418,
"pce":         8037.3,
"pop": 292192,
"psavert":            4.5,
"uempmed":           10.6,
"unemploy": 8370 
},
{
 "date": 12449,
"pce":         8072.1,
"pop": 292368,
"psavert":            4.5,
"uempmed":           10.2,
"unemploy": 8167 
},
{
 "date": 12478,
"pce":           8121,
"pop": 292561,
"psavert":            4.5,
"uempmed":           10.2,
"unemploy": 8491 
},
{
 "date": 12509,
"pce":         8141.6,
"pop": 292779,
"psavert":            4.7,
"uempmed":            9.5,
"unemploy": 8170 
},
{
 "date": 12539,
"pce":         8212.9,
"pop": 292997,
"psavert":            4.7,
"uempmed":            9.9,
"unemploy": 8212 
},
{
 "date": 12570,
"pce":         8204.6,
"pop": 293223,
"psavert":              5,
"uempmed":             11,
"unemploy": 8286 
},
{
 "date": 12600,
"pce":         8270.7,
"pop": 293463,
"psavert":            4.5,
"uempmed":            8.9,
"unemploy": 8136 
},
{
 "date": 12631,
"pce":         8294.4,
"pop": 293719,
"psavert":            4.6,
"uempmed":            9.2,
"unemploy": 7990 
},
{
 "date": 12662,
"pce":           8373,
"pop": 293971,
"psavert":            3.9,
"uempmed":            9.6,
"unemploy": 7927 
},
{
 "date": 12692,
"pce":         8417.9,
"pop": 294230,
"psavert":            3.8,
"uempmed":            9.5,
"unemploy": 8061 
},
{
 "date": 12723,
"pce":         8458.4,
"pop": 294466,
"psavert":            3.4,
"uempmed":            9.7,
"unemploy": 7932 
},
{
 "date": 12753,
"pce":         8516.5,
"pop": 294694,
"psavert":            6.3,
"uempmed":            9.5,
"unemploy": 7934 
},
{
 "date": 12784,
"pce":         8521.2,
"pop": 294914,
"psavert":            2.9,
"uempmed":            9.4,
"unemploy": 7784 
},
{
 "date": 12815,
"pce":         8575.7,
"pop": 295105,
"psavert":            2.7,
"uempmed":            9.2,
"unemploy": 7980 
},
{
 "date": 12843,
"pce":         8622.5,
"pop": 295287,
"psavert":            2.7,
"uempmed":            9.3,
"unemploy": 7737 
},
{
 "date": 12874,
"pce":         8715.9,
"pop": 295490,
"psavert":            2.1,
"uempmed":              9,
"unemploy": 7672 
},
{
 "date": 12904,
"pce":         8680.6,
"pop": 295704,
"psavert":              3,
"uempmed":            9.1,
"unemploy": 7651 
},
{
 "date": 12935,
"pce":         8775.3,
"pop": 295936,
"psavert":            2.2,
"uempmed":              9,
"unemploy": 7524 
},
{
 "date": 12965,
"pce":         8867.9,
"pop": 296186,
"psavert":            1.9,
"uempmed":            8.8,
"unemploy": 7406 
},
{
 "date": 12996,
"pce":         8872.6,
"pop": 296440,
"psavert":            2.4,
"uempmed":            9.2,
"unemploy": 7345 
},
{
 "date": 13027,
"pce":         8923.6,
"pop": 296707,
"psavert":            2.3,
"uempmed":            8.4,
"unemploy": 7553 
},
{
 "date": 13057,
"pce":         8959.6,
"pop": 296972,
"psavert":            2.6,
"uempmed":            8.6,
"unemploy": 7453 
},
{
 "date": 13088,
"pce":         8987.7,
"pop": 297207,
"psavert":            2.7,
"uempmed":            8.5,
"unemploy": 7566 
},
{
 "date": 13118,
"pce":         9026.8,
"pop": 297431,
"psavert":            2.8,
"uempmed":            8.7,
"unemploy": 7279 
},
{
 "date": 13149,
"pce":         9100.1,
"pop": 297647,
"psavert":            3.7,
"uempmed":            8.6,
"unemploy": 7064 
},
{
 "date": 13180,
"pce":         9134.7,
"pop": 297854,
"psavert":            3.8,
"uempmed":            9.1,
"unemploy": 7184 
},
{
 "date": 13208,
"pce":         9168.1,
"pop": 298060,
"psavert":            3.7,
"uempmed":            8.7,
"unemploy": 7072 
},
{
 "date": 13239,
"pce":         9223.3,
"pop": 298281,
"psavert":            3.4,
"uempmed":            8.4,
"unemploy": 7120 
},
{
 "date": 13269,
"pce":         9254.1,
"pop": 298496,
"psavert":            3.2,
"uempmed":            8.5,
"unemploy": 6980 
},
{
 "date": 13300,
"pce":         9283.8,
"pop": 298739,
"psavert":            3.4,
"uempmed":            7.3,
"unemploy": 7001 
},
{
 "date": 13330,
"pce":         9360.4,
"pop": 298996,
"psavert":            2.9,
"uempmed":              8,
"unemploy": 7175 
},
{
 "date": 13361,
"pce":         9368.6,
"pop": 299263,
"psavert":              3,
"uempmed":            8.4,
"unemploy": 7091 
},
{
 "date": 13392,
"pce":         9393.9,
"pop": 299554,
"psavert":              3,
"uempmed":              8,
"unemploy": 6847 
},
{
 "date": 13422,
"pce":         9413.3,
"pop": 299835,
"psavert":            3.1,
"uempmed":            7.9,
"unemploy": 6727 
},
{
 "date": 13453,
"pce":         9431.2,
"pop": 300094,
"psavert":            3.2,
"uempmed":            8.3,
"unemploy": 6872 
},
{
 "date": 13483,
"pce":         9516.5,
"pop": 300340,
"psavert":              3,
"uempmed":            7.5,
"unemploy": 6762 
},
{
 "date": 13514,
"pce":         9553.1,
"pop": 300574,
"psavert":              3,
"uempmed":            8.3,
"unemploy": 7116 
},
{
 "date": 13545,
"pce":         9590.8,
"pop": 300802,
"psavert":            3.3,
"uempmed":            8.5,
"unemploy": 6927 
},
{
 "date": 13573,
"pce":         9631.6,
"pop": 301021,
"psavert":            3.6,
"uempmed":            9.1,
"unemploy": 6731 
},
{
 "date": 13604,
"pce":         9670.6,
"pop": 301254,
"psavert":            3.2,
"uempmed":            8.6,
"unemploy": 6850 
},
{
 "date": 13634,
"pce":         9708.9,
"pop": 301483,
"psavert":              3,
"uempmed":            8.2,
"unemploy": 6766 
},
{
 "date": 13665,
"pce":         9723.3,
"pop": 301739,
"psavert":            2.8,
"uempmed":            7.7,
"unemploy": 6979 
},
{
 "date": 13695,
"pce":         9759.6,
"pop": 302004,
"psavert":            2.8,
"uempmed":            8.7,
"unemploy": 7149 
},
{
 "date": 13726,
"pce":         9800.6,
"pop": 302267,
"psavert":            2.6,
"uempmed":            8.8,
"unemploy": 7067 
},
{
 "date": 13757,
"pce":         9837.5,
"pop": 302546,
"psavert":            2.8,
"uempmed":            8.7,
"unemploy": 7170 
},
{
 "date": 13787,
"pce":         9853.9,
"pop": 302807,
"psavert":            2.8,
"uempmed":            8.4,
"unemploy": 7237 
},
{
 "date": 13818,
"pce":         9928.6,
"pop": 303054,
"psavert":            2.5,
"uempmed":            8.6,
"unemploy": 7240 
},
{
 "date": 13848,
"pce":         9947.6,
"pop": 303287,
"psavert":              3,
"uempmed":            8.4,
"unemploy": 7645 
},
{
 "date": 13879,
"pce":         9963.2,
"pop": 303506,
"psavert":            3.4,
"uempmed":              9,
"unemploy": 7685 
},
{
 "date": 13910,
"pce":         9955.7,
"pop": 303711,
"psavert":            3.9,
"uempmed":            8.7,
"unemploy": 7497 
},
{
 "date": 13939,
"pce":        10004.2,
"pop": 303907,
"psavert":              4,
"uempmed":            8.7,
"unemploy": 7822 
},
{
 "date": 13970,
"pce":        10044.6,
"pop": 304117,
"psavert":            3.5,
"uempmed":            9.4,
"unemploy": 7637 
},
{
 "date": 14000,
"pce":        10093.3,
"pop": 304323,
"psavert":            7.9,
"uempmed":            7.9,
"unemploy": 8395 
},
{
 "date": 14031,
"pce":        10149.4,
"pop": 304556,
"psavert":            5.6,
"uempmed":              9,
"unemploy": 8575 
},
{
 "date": 14061,
"pce":        10151.1,
"pop": 304798,
"psavert":            4.4,
"uempmed":            9.7,
"unemploy": 8937 
},
{
 "date": 14092,
"pce":        10140.3,
"pop": 305045,
"psavert":            3.7,
"uempmed":            9.7,
"unemploy": 9438 
},
{
 "date": 14123,
"pce":        10083.2,
"pop": 305309,
"psavert":            4.4,
"uempmed":           10.2,
"unemploy": 9494 
},
{
 "date": 14153,
"pce":         9983.3,
"pop": 305554,
"psavert":            5.4,
"uempmed":           10.4,
"unemploy": 10074 
},
{
 "date": 14184,
"pce":         9851.2,
"pop": 305786,
"psavert":            6.3,
"uempmed":            9.8,
"unemploy": 10538 
},
{
 "date": 14214,
"pce":         9744.2,
"pop": 306004,
"psavert":            6.5,
"uempmed":           10.5,
"unemploy": 11286 
},
{
 "date": 14245,
"pce":         9792.1,
"pop": 306208,
"psavert":            6.5,
"uempmed":           10.7,
"unemploy": 12058 
},
{
 "date": 14276,
"pce":         9775.7,
"pop": 306402,
"psavert":            5.9,
"uempmed":           11.7,
"unemploy": 12898 
},
{
 "date": 14304,
"pce":         9742.9,
"pop": 306588,
"psavert":            6.1,
"uempmed":           12.3,
"unemploy": 13426 
},
{
 "date": 14335,
"pce":         9741.9,
"pop": 306787,
"psavert":            6.7,
"uempmed":           13.1,
"unemploy": 13853 
},
{
 "date": 14365,
"pce":         9759.7,
"pop": 306984,
"psavert":            8.1,
"uempmed":           14.2,
"unemploy": 14499 
},
{
 "date": 14396,
"pce":         9807.6,
"pop": 307206,
"psavert":            6.7,
"uempmed":           17.2,
"unemploy": 14707 
},
{
 "date": 14426,
"pce":         9835.2,
"pop": 307439,
"psavert":              6,
"uempmed":             16,
"unemploy": 14601 
},
{
 "date": 14457,
"pce":         9961.9,
"pop": 307685,
"psavert":            4.9,
"uempmed":           16.3,
"unemploy": 14814 
},
{
 "date": 14488,
"pce":         9875.4,
"pop": 307946,
"psavert":            5.9,
"uempmed":           17.8,
"unemploy": 15009 
},
{
 "date": 14518,
"pce":         9924.6,
"pop": 308189,
"psavert":            5.4,
"uempmed":           18.9,
"unemploy": 15352 
},
{
 "date": 14549,
"pce":         9946.1,
"pop": 308418,
"psavert":            5.7,
"uempmed":           19.8,
"unemploy": 15219 
},
{
 "date": 14579,
"pce":        10000.6,
"pop": 308633,
"psavert":            5.7,
"uempmed":           20.1,
"unemploy": 15098 
},
{
 "date": 14610,
"pce":        10003.4,
"pop": 308833,
"psavert":            5.6,
"uempmed":             20,
"unemploy": 15046 
},
{
 "date": 14641,
"pce":        10034.7,
"pop": 309027,
"psavert":            5.2,
"uempmed":           19.9,
"unemploy": 15113 
},
{
 "date": 14669,
"pce":        10095.5,
"pop": 309212,
"psavert":              5,
"uempmed":           20.4,
"unemploy": 15202 
},
{
 "date": 14700,
"pce":        10106.9,
"pop": 309191,
"psavert":            5.6,
"uempmed":           22.1,
"unemploy": 15325 
},
{
 "date": 14730,
"pce":        10140.2,
"pop": 309376,
"psavert":              6,
"uempmed":           22.3,
"unemploy": 14849 
},
{
 "date": 14761,
"pce":        10165.9,
"pop": 309562,
"psavert":            5.9,
"uempmed":           25.2,
"unemploy": 14474 
},
{
 "date": 14791,
"pce":        10184.3,
"pop": 309767,
"psavert":            5.9,
"uempmed":           22.3,
"unemploy": 14512 
},
{
 "date": 14822,
"pce":        10247.1,
"pop": 309989,
"psavert":            5.8,
"uempmed":             21,
"unemploy": 14648 
},
{
 "date": 14853,
"pce":        10268.9,
"pop": 310218,
"psavert":            5.6,
"uempmed":           20.3,
"unemploy": 14579 
},
{
 "date": 14883,
"pce":        10343.7,
"pop": 310451,
"psavert":            5.4,
"uempmed":           21.2,
"unemploy": 14516 
},
{
 "date": 14914,
"pce":        10399.8,
"pop": 310657,
"psavert":            5.3,
"uempmed":             21,
"unemploy": 15081 
},
{
 "date": 14944,
"pce":        10436.1,
"pop": 310853,
"psavert":            5.9,
"uempmed":           21.9,
"unemploy": 14348 
},
{
 "date": 14975,
"pce":        10474.7,
"pop": 311042,
"psavert":            6.2,
"uempmed":           21.6,
"unemploy": 14046 
},
{
 "date": 15006,
"pce":        10512.4,
"pop": 311205,
"psavert":            6.4,
"uempmed":           21.1,
"unemploy": 13828 
},
{
 "date": 15034,
"pce":        10583.5,
"pop": 311367,
"psavert":              6,
"uempmed":           21.5,
"unemploy": 13728 
},
{
 "date": 15065,
"pce":        10624.6,
"pop": 311548,
"psavert":            5.9,
"uempmed":           20.9,
"unemploy": 13956 
},
{
 "date": 15095,
"pce":        10653.1,
"pop": 311729,
"psavert":            5.9,
"uempmed":           21.6,
"unemploy": 13853 
},
{
 "date": 15126,
"pce":        10676.4,
"pop": 311923,
"psavert":            6.1,
"uempmed":           22.3,
"unemploy": 13958 
},
{
 "date": 15156,
"pce":        10727.1,
"pop": 312139,
"psavert":            6.3,
"uempmed":             22,
"unemploy": 13756 
},
{
 "date": 15187,
"pce":        10745.6,
"pop": 312355,
"psavert":            6.2,
"uempmed":           22.4,
"unemploy": 13806 
},
{
 "date": 15218,
"pce":        10790.6,
"pop": 312587,
"psavert":            5.7,
"uempmed":             22,
"unemploy": 13929 
},
{
 "date": 15248,
"pce":        10827.6,
"pop": 312810,
"psavert":            5.5,
"uempmed":           20.5,
"unemploy": 13599 
},
{
 "date": 15279,
"pce":        10828.7,
"pop": 313003,
"psavert":            5.6,
"uempmed":           20.9,
"unemploy": 13309 
},
{
 "date": 15309,
"pce":        10827.3,
"pop": 313191,
"psavert":            6.4,
"uempmed":           20.5,
"unemploy": 13071 
},
{
 "date": 15340,
"pce":        10905.5,
"pop": 313373,
"psavert":            6.6,
"uempmed":             21,
"unemploy": 12812 
},
{
 "date": 15371,
"pce":        10979.2,
"pop": 313537,
"psavert":            6.7,
"uempmed":           19.8,
"unemploy": 12828 
},
{
 "date": 15400,
"pce":        10994.3,
"pop": 313705,
"psavert":            6.9,
"uempmed":           19.2,
"unemploy": 12696 
},
{
 "date": 15431,
"pce":        11030.2,
"pop": 313881,
"psavert":              7,
"uempmed":           19.1,
"unemploy": 12636 
},
{
 "date": 15461,
"pce":          11029,
"pop": 314052,
"psavert":              7,
"uempmed":           19.9,
"unemploy": 12668 
},
{
 "date": 15492,
"pce":        11032.5,
"pop": 314247,
"psavert":            7.1,
"uempmed":           20.1,
"unemploy": 12688 
},
{
 "date": 15522,
"pce":        11074.8,
"pop": 314449,
"psavert":            6.6,
"uempmed":           17.5,
"unemploy": 12657 
},
{
 "date": 15553,
"pce":        11104.8,
"pop": 314673,
"psavert":            6.4,
"uempmed":           18.5,
"unemploy": 12449 
},
{
 "date": 15584,
"pce":        11179.6,
"pop": 314909,
"psavert":            6.5,
"uempmed":           18.8,
"unemploy": 12106 
},
{
 "date": 15614,
"pce":        11199.9,
"pop": 315129,
"psavert":            7.1,
"uempmed":           19.7,
"unemploy": 12141 
},
{
 "date": 15645,
"pce":        11222.8,
"pop": 315341,
"psavert":            8.2,
"uempmed":           18.5,
"unemploy": 12026 
},
{
 "date": 15675,
"pce":        11245.2,
"pop": 315532,
"psavert":           10.5,
"uempmed":           17.6,
"unemploy": 12272 
},
{
 "date": 15706,
"pce":        11303.2,
"pop": 315701,
"psavert":            4.5,
"uempmed":           16.2,
"unemploy": 12497 
},
{
 "date": 15737,
"pce":        11371.4,
"pop": 315869,
"psavert":            4.7,
"uempmed":           17.5,
"unemploy": 11967 
},
{
 "date": 15765,
"pce":        11378.8,
"pop": 316041,
"psavert":            4.9,
"uempmed":           17.7,
"unemploy": 11653 
},
{
 "date": 15796,
"pce":        11373.3,
"pop": 316220,
"psavert":            5.1,
"uempmed":           17.1,
"unemploy": 11735 
},
{
 "date": 15826,
"pce":        11407.1,
"pop": 316395,
"psavert":            5.2,
"uempmed":             17,
"unemploy": 11671 
},
{
 "date": 15857,
"pce":        11462.4,
"pop": 316594,
"psavert":            5.3,
"uempmed":           16.6,
"unemploy": 11736 
},
{
 "date": 15887,
"pce":        11484.7,
"pop": 316799,
"psavert":            5.1,
"uempmed":           16.3,
"unemploy": 11357 
},
{
 "date": 15918,
"pce":        11511.6,
"pop": 317019,
"psavert":            5.3,
"uempmed":           16.8,
"unemploy": 11241 
},
{
 "date": 15949,
"pce":        11559.6,
"pop": 317253,
"psavert":            5.2,
"uempmed":           16.5,
"unemploy": 11251 
},
{
 "date": 15979,
"pce":        11602.1,
"pop": 317470,
"psavert":            4.7,
"uempmed":           16.1,
"unemploy": 11161 
},
{
 "date": 16010,
"pce":        11671.5,
"pop": 317679,
"psavert":            4.3,
"uempmed":             17,
"unemploy": 10814 
},
{
 "date": 16040,
"pce":        11686.3,
"pop": 317867,
"psavert":            4.1,
"uempmed":             17,
"unemploy": 10376 
},
{
 "date": 16071,
"pce":        11663.9,
"pop": 318032,
"psavert":            4.9,
"uempmed":           15.9,
"unemploy": 10280 
},
{
 "date": 16102,
"pce":        11714.4,
"pop": 318200,
"psavert":              5,
"uempmed":           16.2,
"unemploy": 10387 
},
{
 "date": 16130,
"pce":        11807.1,
"pop": 318373,
"psavert":            4.8,
"uempmed":           15.9,
"unemploy": 10384 
},
{
 "date": 16161,
"pce":        11825.2,
"pop": 318552,
"psavert":              5,
"uempmed":           15.6,
"unemploy": 9696 
},
{
 "date": 16191,
"pce":        11864.3,
"pop": 318728,
"psavert":            5.1,
"uempmed":           14.5,
"unemploy": 9761 
},
{
 "date": 16222,
"pce":        11922.6,
"pop": 318927,
"psavert":            5.1,
"uempmed":           13.2,
"unemploy": 9453 
},
{
 "date": 16252,
"pce":        11944.4,
"pop": 319133,
"psavert":            5.1,
"uempmed":           13.5,
"unemploy": 9648 
},
{
 "date": 16283,
"pce":          12017,
"pop": 319354,
"psavert":            4.7,
"uempmed":           13.3,
"unemploy": 9568 
},
{
 "date": 16314,
"pce":        12044.6,
"pop": 319588,
"psavert":            4.6,
"uempmed":           13.3,
"unemploy": 9237 
},
{
 "date": 16344,
"pce":        12096.4,
"pop": 319804,
"psavert":            4.6,
"uempmed":           13.5,
"unemploy": 8983 
},
{
 "date": 16375,
"pce":        12142.2,
"pop": 320013,
"psavert":            4.5,
"uempmed":           12.8,
"unemploy": 9071 
},
{
 "date": 16405,
"pce":          12122,
"pop": 320201,
"psavert":              5,
"uempmed":           12.6,
"unemploy": 8688 
},
{
 "date": 16436,
"pce":        12080.8,
"pop": 320367,
"psavert":            5.5,
"uempmed":           13.4,
"unemploy": 8979 
},
{
 "date": 16467,
"pce":        12095.9,
"pop": 320534,
"psavert":            5.7,
"uempmed":           13.1,
"unemploy": 8705 
},
{
 "date": 16495,
"pce":        12161.5,
"pop": 320707,
"psavert":            5.2,
"uempmed":           12.2,
"unemploy": 8575 
},
{
 "date": 16526,
"pce":        12158.9,
"pop": 320887,
"psavert":            5.6,
"uempmed":           11.7,
"unemploy": 8549 
} 
]
  
      if(!(opts.type==="pieChart" || opts.type==="sparklinePlus" || opts.type==="bulletChart")) {
        var data = d3.nest()
          .key(function(d){
            //return opts.group === undefined ? 'main' : d[opts.group]
            //instead of main would think a better default is opts.x
            return opts.group === undefined ? opts.y : d[opts.group];
          })
          .entries(data);
      }
      
      if (opts.disabled != undefined){
        data.map(function(d, i){
          d.disabled = opts.disabled[i]
        })
      }
      
      nv.addGraph(function() {
        var chart = nv.models[opts.type]()
          .width(opts.width)
          .height(opts.height)
          
        if (opts.type != "bulletChart"){
          chart
            .x(function(d) { return d[opts.x] })
            .y(function(d) { return d[opts.y] })
        }
          
         
        chart
  .xTickFormat(function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));})
          
        

        
        
        
      
       d3.select("#" + opts.id)
        .append('svg')
        .datum(data)
        .transition().duration(500)
        .call(chart);

       nv.utils.windowResize(chart.update);
       return chart;
      });
    };
</script>

--- .class #id

### rCharts: highcharts


```r
p3 <- hPlot(Pulse ~ Height, data = MASS::survey, type = "bubble", title = "Zoom demo",
            subtitle = "bubble chart", size = "Age", group = "Exer")
p3$chart(zoomType = "xy"); p3$exporting(enabled = T); p3$print('chart3')
```


<div id = 'chart3' class = 'rChart highcharts'></div>
<script type='text/javascript'>
    (function($){
        $(function () {
            var chart = new Highcharts.Chart({
 "dom": "chart3",
"width":            800,
"height":            400,
"credits": {
 "href": null,
"text": null 
},
"exporting": {
 "enabled": true 
},
"title": {
 "text": "Zoom demo" 
},
"yAxis": [
 {
 "title": {
 "text": "Pulse" 
} 
} 
],
"series": [
 {
 "data": [
 [
         154.94,
71,
        17.167 
],
[
          156.2,
80,
          28.5 
],
[
            157,
74,
        35.833 
],
[
            157,
74,
            18 
],
[
            157,
89,
        19.333 
],
[
         157.48,
68,
         17.75 
],
[
         160.02,
80,
          18.5 
],
[
         162.56,
60,
        17.417 
],
[
            164,
64,
        18.583 
],
[
            164,
64,
        20.167 
],
[
            164,
84,
        23.083 
],
[
            165,
48,
        18.667 
],
[
            165,
80,
        18.167 
],
[
            165,
92,
            20 
],
[
          165.1,
87,
        17.083 
],
[
          166.4,
72,
         39.75 
],
[
          166.5,
60,
         23.25 
],
[
            167,
70,
        20.333 
],
[
         167.64,
40,
        17.417 
],
[
         167.64,
70,
        17.333 
],
[
         167.64,
72,
        17.333 
],
[
         167.64,
90,
        17.167 
],
[
            168,
83,
        17.083 
],
[
          168.5,
85,
         17.75 
],
[
          169.2,
60,
        29.083 
],
[
            170,
64,
        17.583 
],
[
            170,
68,
        20.667 
],
[
            170,
70,
        23.833 
],
[
            170,
75,
         18.75 
],
[
            170,
104,
         17.25 
],
[
         170.18,
75,
        21.167 
],
[
         170.18,
80,
            17 
],
[
            172,
68,
        19.167 
],
[
            172,
68,
        23.417 
],
[
            172,
73,
          21.5 
],
[
            172,
92,
          17.5 
],
[
         172.72,
65,
         17.25 
],
[
         172.72,
68,
        17.667 
],
[
         172.72,
69,
        70.417 
],
[
         172.72,
76,
        36.583 
],
[
            173,
62,
        20.333 
],
[
            173,
76,
        23.583 
],
[
            174,
48,
        21.333 
],
[
            175,
72,
        20.167 
],
[
            175,
76,
        24.167 
],
[
         175.26,
62,
        18.083 
],
[
         175.26,
66,
            21 
],
[
         175.26,
68,
        19.083 
],
[
          176.5,
76,
        20.167 
],
[
            177,
76,
         18.25 
],
[
            177,
78,
        17.917 
],
[
          177.8,
62,
        17.667 
],
[
            178,
70,
          17.5 
],
[
          178.5,
66,
        18.083 
],
[
            179,
56,
        17.417 
],
[
            179,
60,
        21.583 
],
[
            179,
65,
        22.833 
],
[
            180,
59,
        17.417 
],
[
            180,
64,
        18.583 
],
[
            180,
78,
          17.5 
],
[
            180,
84,
        18.917 
],
[
         180.34,
64,
        17.833 
],
[
         180.34,
70,
        17.417 
],
[
         180.34,
72,
        18.167 
],
[
         180.34,
72,
        17.333 
],
[
            182,
65,
            20 
],
[
          182.5,
72,
        17.917 
],
[
         182.88,
72,
        19.333 
],
[
         182.88,
83,
        18.833 
],
[
            184,
100,
        20.083 
],
[
            185,
60,
        17.917 
],
[
            185,
68,
        17.417 
],
[
            185,
71,
        19.333 
],
[
            185,
88,
        19.333 
],
[
         185.42,
60,
        32.667 
],
[
            187,
66,
        20.333 
],
[
            187,
84,
        17.917 
],
[
         187.96,
64,
            23 
],
[
         187.96,
86,
            20 
],
[
            188,
75,
        18.917 
],
[
            190,
66,
            18 
],
[
            190,
68,
          17.5 
],
[
            190,
68,
        19.417 
],
[
          190.5,
72,
        17.917 
],
[
            195,
76,
          25.5 
],
[
            196,
63,
        20.083 
],
[
            200,
55,
          18.5 
] 
],
"name": "Freq",
"type": "bubble",
"marker": {
 "radius":              3 
} 
},
{
 "data": [
 [
         157.48,
70,
        17.167 
],
[
            158,
70,
          20.5 
],
[
            160,
86,
        20.167 
],
[
            165,
50,
         30.75 
],
[
            165,
65,
          18.5 
],
[
            165,
97,
          19.5 
],
[
            167,
68,
        18.667 
],
[
            167,
80,
        18.417 
],
[
            170,
60,
         17.75 
],
[
            170,
96,
        19.417 
],
[
            176,
68,
        18.917 
],
[
          177.8,
104,
        17.583 
],
[
         180.34,
68,
        43.833 
],
[
          190.5,
80,
        18.167 
] 
],
"name": "None",
"type": "bubble",
"marker": {
 "radius":              3 
} 
},
{
 "data": [
 [
            152,
90,
        18.333 
],
[
          152.4,
92,
          23.5 
],
[
          153.5,
76,
        17.417 
],
[
         154.94,
72,
        17.083 
],
[
            155,
66,
          17.5 
],
[
            159,
70,
        22.917 
],
[
            160,
74,
        17.167 
],
[
            160,
84,
        18.583 
],
[
            160,
88,
        16.917 
],
[
         160.02,
65,
         32.75 
],
[
         160.02,
72,
         17.25 
],
[
          162.5,
79,
         17.25 
],
[
         162.56,
70,
            18 
],
[
         162.56,
70,
        17.167 
],
[
         162.56,
88,
        18.167 
],
[
            163,
79,
        24.667 
],
[
            163,
80,
        17.667 
],
[
            163,
83,
         17.25 
],
[
            164,
80,
          17.5 
],
[
            165,
35,
        23.667 
],
[
            165,
65,
        20.417 
],
[
            165,
70,
        19.667 
],
[
            165,
76,
        23.583 
],
[
            165,
76,
          26.5 
],
[
            165,
88,
         17.75 
],
[
          165.1,
68,
        17.083 
],
[
          165.1,
85,
        17.667 
],
[
            167,
61,
         19.25 
],
[
            167,
76,
         17.25 
],
[
            167,
79,
         19.25 
],
[
            167,
90,
        22.333 
],
[
         167.64,
74,
         44.25 
],
[
            168,
60,
        18.417 
],
[
            168,
72,
        21.167 
],
[
            168,
81,
          18.5 
],
[
          168.9,
68,
        17.083 
],
[
            169,
80,
        18.167 
],
[
            170,
70,
        18.167 
],
[
            170,
80,
         18.25 
],
[
            170,
80,
        19.167 
],
[
         170.18,
78,
        18.333 
],
[
         170.18,
80,
        18.417 
],
[
            171,
68,
        17.667 
],
[
            171,
100,
        18.917 
],
[
            172,
60,
        28.583 
],
[
         172.72,
64,
            21 
],
[
         172.72,
90,
          17.5 
],
[
            173,
92,
         18.25 
],
[
            175,
72,
            19 
],
[
            175,
84,
          17.5 
],
[
            175,
90,
         18.75 
],
[
         175.26,
85,
        18.417 
],
[
          176.5,
80,
          17.5 
],
[
            178,
60,
         18.75 
],
[
          179.1,
80,
        18.667 
],
[
          179.1,
92,
        18.917 
],
[
            180,
60,
        27.333 
],
[
            180,
70,
        17.167 
],
[
            180,
96,
            19 
],
[
         180.34,
67,
         17.75 
],
[
         182.88,
74,
        18.333 
],
[
         182.88,
80,
        18.667 
],
[
            183,
75,
        19.667 
],
[
            183,
90,
        17.167 
],
[
            184,
62,
         18.25 
],
[
            185,
75,
            19 
],
[
            185,
80,
          35.5 
],
[
            189,
90,
        19.167 
],
[
          191.8,
72,
          17.5 
],
[
         193.04,
83,
        18.917 
] 
],
"name": "Some",
"type": "bubble",
"marker": {
 "radius":              3 
} 
} 
],
"xAxis": [
 {
 "title": {
 "text": "Height" 
} 
} 
],
"subtitle": {
 "text": "bubble chart" 
},
"chart": {
 "zoomType": "xy",
"renderTo": "chart3" 
},
"id": "chart3" 
});
        });
    })(jQuery);
</script>

--- .class #id

## Reporting with `knitr`

- use `rmarkdown` syntax
- generate charts from data, e.g. using `ggplot`
- include key figures in narrative, e.g. descriptive statistics
- convert to various output formats (Word, PDF, HTML)

<p style="text-align:right"><img src="assets/img/knit-logo.png" alt="knitr logo"/></p>

--- .class #id

### Reporting with `knitr`

<p style="text-align:left"><img src="assets/img/knitr_minimal_result.png" alt="knitr output"/></p>

--- .class #id

### Reporting with `knitr`


```r
# A Minimal Example for Markdown
This is a minimal example of using **knitr** to produce an _HTML_ page from _Markdown_.
## R code chunks
Now we write some code chunks in this markdown file:
'''{r computing}
x <- 1+1 # a simple calculator
set.seed(123)
rnorm(5)  # boring random numbers
'''
We can also produce plots:
'''{r graphics}
par(mar = c(4, 4, .1, .1))
with(mtcars, {
  plot(mpg~hp, pch=20, col='darkgray')
  lines(lowess(hp, mpg))
})
'''
```

--- .class #id

## Report Templates

`brew`: generate input files for `knitr`

apply function to country vector


```r
create.report <- function(x, prepend = "report_icio_tiva_") {
  Rmd.file <- file.path(path.Rmd, paste0(prepend, x, ".Rmd"))
  rmd.file <- file.path(path.rmd, paste0(prepend, x, ".rmd")) # .md doesn't convert hash tags
  brew(file = file.path(path, "report_icio_tiva.brew"), output = Rmd.file)
  knit(input = Rmd.file, output = rmd.file)
  out.file <- paste0(prepend, x, ".rmd")
  return(out.file)
}
coulist <- c("AUT", "DEU", "ESP", "IRL", "USA")
results <- sapply(as.character(coulist), create.report)
```

--- .class #id

### `brew` template example


```r
'''{r preamble, echo = FALSE}
cou <<- '<%= x %>'
country <- as.character(namereg$country[match(cou, namereg$cou)])
natnlty <- as.character(namereg$coupron[match(cou, namereg$cou)])
customtext <- cntext[,colnames(cntext)==cou]
'''
# Trade in Value-added: 'r country'
## EXGRDVA\_EX {#exgrdvaex}
### Domestic value added content of gross exports, 'r year', %
'''{r fig1, fig.path="figures/report_icio_tiva/<%= x %>/", fig.height=5, fig.width=10,
      echo=FALSE, message=FALSE}
	source(file.path(path, "code", "figure1.R"))
'''
'r country' domestic value-added content of its exports is, at 'r .perc1'%, 'r .rel1'
the OECD average in 'r year'.
'r if(!is.na(customtext[1])) customtext[1]'
```

--- .class #id

## Reporting Tools and Platforms

[ReporteRs](http://davidgohel.github.io/ReporteRs/index.html): create MS doc and ppt with editable graphics and formatted tables

<p style="text-align:left"><img src="assets/img/MSWord_logo.png" alt="Word logo"/></p>

[rapporter](http://rapporter.net) / [pander](http://rapporter.github.io/pander/): ruby on rails online platform for report generation

<p style="text-align:left"><img src="assets/img/rapporter_logo.png" alt="rapporter logo"/></p>

[jekyll](http://jekyllrb.com/): ruby on rails framework to generate static homepages from `rmd` files (i.e. evaluate R code with `knitr`, see http://10.101.26.220, http://r-pkgs.had.co.nz/ or http://adv-r.had.co.nz)

--- .class #id

## References

- ggplot2 http://ggplot2.org/
- R for Data Science http://r4ds.had.co.nz/
- ggplot2 extensions https://www.ggplot2-exts.org/
- static plotting showcase http://yutannihilation.github.io/allYourFigureAreBelongToUs/
- interactive plotting showcase http://gallery.htmlwidgets.org/
- CRAN Task View: Graphics http://cran.r-project.org/web/views/Graphics.html
- Shiny R Graph Catalog http://shinyapps.stat.ubc.ca/r-graph-catalog/
