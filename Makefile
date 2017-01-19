## Makefile created by 'dryworkflow' at Mon May  9 16:01:11 2016

.PHONY: all
all: index.html
# all: R-Bootcamp.html get-data.html 3-data-structures.html 4-data-types.html tidy-data.html dplyr-data.html 7-quickplots.html 8-ggplot.html # \
R-Bootcamp-students.html get-data-students.html 3-data-structures-students.html 4-data-types-students.html tidy-data-students.html dplyr-data-students.html 7-quickplots-students.html 8-ggplot-students.html

index.html: index.Rmd
	R CMD BATCH --no-save create-presentation-slidify.R

# # teacher
# R-Bootcamp.html: ${@:.html=.Rmd} R-Bootcamp.Rmd
# get-data.html: ${@:.html=.Rmd} get-data.Rmd
# 3-data-structures.html: ${@:.html=.Rmd} 3-data-structures.Rmd
# 4-data-types.html: ${@:.html=.Rmd} 4-data-types.Rmd
# tidy-data.html: ${@:.html=.Rmd} tidy-data.Rmd
# dplyr-data.html: ${@:.html=.Rmd} dplyr-data.Rmd
# 7-quickplots.html: ${@:.html=.Rmd} 7-quickplots.Rmd
# 8-ggplot.html: ${@:.html=.Rmd} 8-ggplot.Rmd

# # students
# R-Bootcamp-students.html: ${@:.html=.Rmd} R-Bootcamp-students.Rmd
# get-data-students.html: ${@:.html=.Rmd} get-data-students.Rmd
# 3-data-structures-students.html: ${@:.html=.Rmd} 3-data-structures-students.Rmd
# 4-data-types-students.html: ${@:.html=.Rmd} 4-data-types-students.Rmd
# tidy-data-students.html: ${@:.html=.Rmd} tidy-data-students.Rmd
# dplyr-data-students.html: ${@:.html=.Rmd} dplyr-data-students.Rmd
# 7-quickplots-students.html: ${@:.html=.Rmd} 7-quickplots-students.Rmd
# 8-ggplot-students.html: ${@:.html=.Rmd} 8-ggplot-students.Rmd

# report_imputeLoss_finalPredictData_csv.html: ${@:.html=.Rmd} ../work/summary_imputeLoss_finalPredictData_csv.Rout

# include ~/lib/common.mk
