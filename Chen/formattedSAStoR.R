try <- Hmisc::sas.get("C:/Users/Chen Chen/Dropbox/GSS Project/DrowsyDrivers/Data/Raw/GES13", 
                      mem="vehicle", format.library="C:/Users/Chen Chen/Dropbox/GSS Project/DrowsyDrivers/Data/Raw/GES13", 
                      sasprog ="C:/Program Files/SASHome/x86/SASFoundation/9.3/SAS" )
library(haven)
setwd("C:/Users/Chen Chen/Dropbox/GSS Project/DrowsyDrivers/Data/Raw/GES13")
sas <- haven::read_sas('Vehicle.sas7bdat', "formats.sas7bcat")

vehicle <- read.csv("vehicle.csv")