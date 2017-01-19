## source(file.path(dbpath, "GitHub", "as_seminar_report", "create_presentation_slidify.R"))
	
## require(devtools)
## ## Windows:
## require(httr)
## set_config(config(ssl.verifypeer = 0L))
## ## Linux: make sure no proxy is active
## Sys.getenv("http_proxy")
## ## GitHub repos
## devtools::install_github("ramnathv/slidify")
## devtools::install_github("ramnathv/slidifyLibraries")
## devtools::install_github("ramnathv/rCharts")
require(slidify)
require(slidifyLibraries)

## ## additional
## install.packages("ggvis")
## install.packages("googleVis")
## install.packages("gridBase")
## install.packages("knitcitations")
## install.packages("maps")
## install.packages("RGraphics")
## install.packages("oz")
## install.packages("RODBC")
## install_github("yihui/runr")
## install_github('ramnathv/slidify', ref = github_pull(425))

## library(rCharts)
## devtools::install_github('ramnathv/rCharts')

## path <- file.path(dbpath, "GitHub", "slidify", "as_seminar_report")
path <- file.path("~", "src", "R", "as-seminar", "as-seminar-report")
setwd(path)

## author("as_seminar_reports")
## library(ggthemr)
## slidify(inputFile = file.path("~/Dropbox", "GitHub", "slidify", "as_seminar_report", "index.Rmd"))
## unlink(".cache/*")
slidify("index.Rmd")
  ## publish(user = "bowerth", repo = "as_seminar_report", host = "github")
