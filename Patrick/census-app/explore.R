counties <- readRDS("census-app/data/counties.rds")
head(counties)

# The percent_map function in helpers.R takes five arguments:
#   
#   Argument	Input
# var	a column vector from the counties.rds dataset
# color	any character string you see in the output of colors()
# legend.title	A character string to use as the title of the plot's legend
# max	A parameter for controlling shade range (defaults to 100)
# min	A parameter for controlling shade range (defaults to 0)


library(maps)
library(mapproj)
source("census-app/helpers.R")
counties <- readRDS("census-app/data/counties.rds")
percent_map(counties$white, "darkgreen", "% white")