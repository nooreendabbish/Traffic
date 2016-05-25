 ###################
## Load Packages ##
###################
library(survey)
library(sas7bdat)


#################################
###### Chapter 2 p21 ############
#################################
data(api)
srs_design <- svydesign(id=~1, weights=~pw, data=apisrs)
svytotal(~enroll, srs_design)
svymean(~enroll, srs_design)

#########################
##### GES data 2014 #####
#########################
setwd("C:/Users/Chen Chen/Dropbox/gss project/DrowsyDrivers/Data/Anal/SAs")
## Information about accidents ##
case <- read.sas7bdat("casecnt.sas7bdat")
acc_design <- svydesign(id=~1, weights=~WEIGHT, data=case)
svytotal(~CASECNT, acc_design)
#svyby(~CASECNT, ~DRIMPAIR+REGION, acc_design, svytotal)

## Nastya's deisgn ##
load("C:/Users/Chen Chen/Dropbox/gss project/DrowsyDrivers/Data/Raw/GES14/R/accident2014.rda")
acc_design <- svydesign(id = ~ PSU + PJ + CASENUM, 
                        weights = ~ WEIGHT,
                        strata =  ~ PSUSTRAT + STRATUM,
                        nest = TRUE,
                        data=accident2014)
svytotal(~CASENUM, acc_design)

##########################################
###### Understanding the design ##########
##########################################
sum(accident2014$WEIGHT)
length(unique(accident2014$PSUSTRAT))
length(unique(accident2014$PSU))
length(unique(accident2014$STRATUM))

## Updated Survey Design ##
accident2014$PJSTRAT <- rep(1, dim(accident2014)[1])
design2014 <- svydesign(id = ~ PSU + PJ + CASENUM, 
                        weights = ~ WEIGHT,
                        strata =  ~ PSUSTRAT + PJSTRAT + STRATUM,
                        lonely.psu=getOption("remove"),
                        data=accident2014)
svytotal(~CASENUM, design2014)
svymean(~VE_TOTAL, design2014, na.rm=TRUE)
