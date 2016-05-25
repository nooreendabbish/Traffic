##########################################
## Name: readSAS.R
## Author: CC
## Modified: 
## Last Modified: 18JAN2016 
##########################################

## Set up location for year of 2003 ##
getwd()
setwd("C:/Users/Chen Chen/Dropbox/GSS Project/DrowsyDrivers/Data/Raw/GES03")

## Import data from SAS ##
library(sas7bdat)
person <- read.sas7bdat("person.sas7bdat")

## Save as R data ##
setwd("C:/Users/Chen Chen/Dropbox/GSS Project/DrowsyDrivers/Data/Raw/GES03/R data")
save(person,file="person.Rda")
load("person.Rda")
