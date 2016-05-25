library(survey)
library(MASS)
library(Maeswrap)

load("GES2014.rda")

##Create a subset of the data to look at only drivers 
##Including all lines gives more weight to vehicles with more passengers in our drowsyness regressions, which is not what we want
##We are also only interested in the day at the moment
GES2014.drivers <- subset(GES2014, PER_TYP == 1)
GES2014.drivers.day <- subset(GES2014.drivers, HOUR_IM %in% 7:23)
##CHECK WHERE HEIBERGER MADE HIS CUT TO DEFINE "NIGHTTIME"; USE THE SAME PERIOD
GES2014.drivers.day$PJSTRAT <- 1

GES2014.drivers.day$DROWSY <- factor(ifelse(GES2014.drivers.day$DRIMPAIR == 2, 1, 
                                     ifelse(GES2014.drivers.day$DRIMPAIR %in% c(95, 98, 99), NA, 0) ))

##Define young as 17 to 23, per the NHTSA drowsyness fact sheet
GES2014.drivers.day$DRYOUNG <- factor(ifelse(GES2014.drivers.day$AGE_IM %in% 17:23, 1, 0))

GES2014.drivers.day$SEASON <- factor(ifelse(GES2014.drivers.day$MONTH %in% c(12, 1, 2), 1,
                                     ifelse(GES2014.drivers.day$MONTH %in% 3:5, 2,
                                            ifelse(GES2014.drivers.day$MONTH %in% 6:8, 3, 4)  )))
##Ultimately used SUMMER, we are now looking into better cutpoint
GES2014.drivers.day$SUMMER <- factor(ifelse(GES2014.drivers.day$MONTH %in% 6:8, 1, 0))
GES2014.drivers.day$WEEKEND <- factor(ifelse(GES2014.drivers.day$WKDY_IM %in% c(1, 7), 1, 0))

##Change unknown highway status 9 to NA for proper handling
GES2014.drivers.day$INT_HWY[GES2014.drivers.day$INT_HWY == 9] <- NA
GES2014.drivers.day$INT_HWY <- factor(GES2014.drivers.day$INT_HWY)

##Recode speeding-related variable to be dichotomous or NA
GES2014.drivers.day$SPEEDREL <- factor(ifelse(GES2014.drivers.day$SPEEDREL == 0, 0,
                                       ifelse(GES2014.drivers.day$SPEEDREL %in% 1:5, 1, NA) ))

GES2014.drivers.day.design <- svydesign(id = ~ PSU + PJ + CASENUM,
                                data = GES2014.drivers.day,
                                weight = ~ WEIGHT,
                                strata = ~ PSUSTRAT + PJSTRAT + STRATUM) # Create the survey design object for use in model fitting
save(GES2014.drivers.day, file = "GES2014.drivers.day.rda")
save(GES2014.drivers.day.design, file = "GES2014.drivers.day.design.rda")


load("GES2014.drivers.day.rda")
load("GES2014.drivers.day.design.rda")

options(survey.lonely.psu="adjust")
drowsy.day.weekend <- svyglm(DROWSY ~ WEEKEND,
                             family = binomial(link = logit), 
                             design = GES2014.drivers.day.design)
summary(drowsy.day.weekend)

options(survey.lonely.psu="adjust")
drowsy.day.season <- svyglm(DROWSY ~ SEASON,
                            family = binomial(link = logit), 
                            design = GES2014.drivers.day.design)
summary(drowsy.day.season)

options(survey.lonely.psu="adjust")
drowsy.day.alchl <- svyglm(DROWSY ~ ALCHL_IM,
                           family = binomial(link = logit), 
                           design = GES2014.drivers.day.design)
summary(drowsy.day.alchl)

options(survey.lonely.psu="adjust")
drowsy.day.int.hwy <- svyglm(DROWSY ~ INT_HWY,
                             family = binomial(link = logit), 
                             design = GES2014.drivers.day.design)
summary(drowsy.day.int.hwy)

options(survey.lonely.psu="adjust")
drowsy.day.sex <- svyglm(DROWSY ~ SEX_IM,
                              family = binomial(link = logit), 
                              design = GES2014.drivers.day.design)
summary(drowsy.day.sex)

options(survey.lonely.psu="adjust")
drowsy.day.young <- svyglm(DROWSY ~ DRYOUNG,
                               family = binomial(link = logit), 
                               design = GES2014.drivers.day.design)
summary(drowsy.day.young)

options(survey.lonely.psu="adjust")
drowsy.day.speedrel <- svyglm(DROWSY ~ SPEEDREL,
                           family = binomial(link = logit), 
                           design = GES2014.drivers.day.design)
summary(drowsy.day.speedrel)

# options(survey.lonely.psu="adjust")
# drowsy.day.all <- svyglm(DROWSY ~ WEEKEND + SEASON + ALCHL_IM + 
#                                   INT_HWY + SEX_IM + DRYOUNG + SPEEDREL,
#                          family = binomial(link = logit), 
#                          design = GES2014.drivers.day.design)
# summary(drowsy.day.all) 

#stepAIC(drowsy.day.all)


##Collect all glm objects in the environment into a list for easy/automated comparison
predictors <- c("WEEKEND", "SUMMER", "ALCHL_IM", "INT_HWY", "SEX_IM", "DRYOUNG", 
                "SPEEDREL")
response <- "DROWSY"

fwd.stepwise.AIC <- function(predictors, response)
{
  fmlas <- paste0(response, " ~ ", predictors)
  length.save <- length(predictors)
  # AIC.save <- rep(0, length.save)
  # effect <- rep("", length.save)

  for (i in 1:length.save)
  {
    Models <- lapply(fmlas, function(x) svyglm(x,
                                               family = binomial(link = logit), 
                                               design = GES2014.drivers.day.design))
    # AICs <- lapply( Models, function(x) unlist(AIC(x)) )
    deviances <- lapply(Models, "[", "deviance")
    names(deviances) <- lapply(lapply(lapply( Models, 
                                              function(x) attr(terms(x), "term.labels") ), 
                                      "[", 1:i), 
                               function(x) paste(x, sep="", collapse="*"))
    # names(AICs) <- lapply(lapply(lapply( Models, 
    #                                      function(x) attr(terms(x), "term.labels") ), 
    #                              "[", 1:i), 
    #                       function(x) paste(x, sep="", collapse="*"))
    effect[i] <- substr(names(which.min(unlist(deviances))), 1, nchar(names(which.min(unlist(deviances))))-9)
    AIC.save[i] <- AIC(Models[[which.min(unlist(deviances))]])[2]
    pred.add <- paste(rev(strsplit(gsub( "[*].*$", "", paste(rev(strsplit(effect, NULL)[[1]]), sep="", collapse="")), NULL)[[1]]), sep="", collapse="")
    predictors <- predictors[predictors != pred.add]
    #AIC.save[i] <- AIC[[effect[i]]][2]
    print(effect[i])
    fmlas <- paste0(response, " ~ ", effect[i], "*", predictors)
  }
  names(AIC.save) <- effect
  AIC.save
}

obj <- fwd.stepwise.AIC(predictors, response)
obj









##5.23.16
drowsy.day.almost.best.AIC <- svyglm(DROWSY ~ SEX_IM*SPEEDREL*SUMMER*DRYOUNG,
                                     family = binomial(link = logit), 
                                     design = GES2014.drivers.day.design)
scale <- riskScaleSimp(drowsy.day.almost.best.AIC)
groups <- riskGroups(GES2014.drivers.day, scale, drowsy.day.almost.best.AIC)