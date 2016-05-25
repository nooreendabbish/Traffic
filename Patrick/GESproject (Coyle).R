library(sas7bdat)
library(foreign)
library(vcd)
library(survey)
library(latticeExtra)
#library(LogisticDx)

# accident.2014 <- read.sas7bdat("C:/Users/Patrick/Documents/Grad School/GES Project/GES2014zip/accident.sas7bdat")
# names(accident.2014)
# 
# accident <- read.sas7bdat("C:/Users/Patrick/Documents/Grad School/GES Project/name_of_analysis_file.sas7bdat")
# accident.hist <- hist(accident$HOUR[accident$HOUR <= 24], breaks=23)
# plot(accident.hist)
# 
# impair.2003 <- read.sas7bdat("C:/Users/Patrick/Documents/Grad School/GES Project/Raw/GES03/impair.sas7bdat")
# impair.2003.drowsy <- subset(impair.2003, MIMPAIR == 2)
# impair.2003.nondrowsy <- subset(impair.2003, MIMPAIR != 2)
# 
# impair.2003.PSU.hist <- hist(impair.2003$PSU)








##Where the magic begins

# drimpair.2014 <- read.sas7bdat("C:/Users/Patrick/Documents/Grad School/GES Project/GES2014zip/drimpair.sas7bdat")
# 
# names(drimpair.2014)
# drimpair2014.design <- svydesign(id = ~ PSU + PJ + CASENUM,
#                                  data = drimpair2014,
#                                  weight = ~ WEIGHT,
#                                  strata = ~ PSUSTRAT)
# for (i in 1:length(drimpair.2014$DRIMPAIR))
# {
#   if (drimpair.2014[i, 8] != 2)
#   {
#     drimpair.2014[i, 8] <- "Not asleep/fatigued"
#   }
# }
# mosaic(DRIMPAIR ~ PSUSTRAT, 
#        data = drimpair.2014, 
#        gp = shading_Friendly(lty = 1, eps = NULL))
# mosaic(PSUSTRAT ~ DRIMPAIR, 
#        data = drimpair.2014, 
#        gp = shading_Friendly(lty = 1, eps = NULL))
# ##Conduct overall chi-square test of independence!
# ##See if format file in SAS tells us what the PSUSTRATS are! Can we group so we're just examining urban/rural??
# chisq.test(table(drimpair.2014$PSUSTRAT, drimpair.2014$DRIMPAIR))
##X-squared = 114.18, df = 13, p-value < 2.2e-16
##There is evidence of association between DRIMPAIR and PSUSTRAT, i.e. there is evidence that the probability of fatigue (conditional on an accident occuring) differs based on PSUSTRAT category











##Create .rda files from .sas7bdat files
# drimpair2014 <- read.sas7bdat("C:/Users/Patrick/Documents/Grad School/GES Project/GES2014zip/drimpair.sas7bdat")
# write.table(drimpair2014, "drimpair2014.txt", sep="\t") 
# drimpair2014 <- read.table("drimpair2014.txt")
# save(drimpair2014, file="drimpair2014.rda")
# unlink("drimpair2014.txt") ##Delete the .txt file
# filenames <- list.files(pattern="*.sas7bdat", full.names=TRUE)
# file <- NA
# name <- NA
# GES2014 <- NA
# for (i in c(5,12,14))
# {
#   file <- read.sas7bdat(filenames[i])
#   name <- paste(filenames[i], ".txt")
#   write.table(file, name, sep="\t")
#   file <- read.table(name)
#   GES2014 <- merge(GES2014, file)
#   # unlink("drimpair2014.txt") ##Delete the .txt file
# }
# GES2014 <- lapply(filenames, read.sas7bdat)
# write.table(drimpair2014, "drimpair2014.txt", sep="\t") 
# drimpair2014 <- read.table("drimpair2014.txt")
# save(drimpair2014, file="drimpair2014.rda")
# unlink("drimpair2014.txt") ##Delete the .txt file

# #Create our data file for analysis
# load("drimpair2014.rda")
# load("person2014.rda")
# GES2014 <- merge(drimpair2014, person2014) #This function does not replicate duplicate columns. Very helpful.
load("GES2014.rda")

#GES2014$FATIGUED <- ifelse(GES2014$DRIMPAIR == 2, 1, 0) #Not sure if this is necessary or if we can just use I(DRIMPAIR==2) in all our functions

options(survey.lonely.psu="adjust") # Allow for conservative adjustment of variance estimation due to there existing a PSU with only one stratum in our data set.
GES2014.design <- svydesign(id = ~ PSU + PJ + CASENUM,
                            data = GES2014,
                            weight = ~ WEIGHT,
                            strata = ~ PSUSTRAT + STRATUM) # Create the survey design object for use in model fitting



# # http://www.nhtsa.gov/Driving+Safety/Drowsy+Driving
# # NHSTA states multiple at-risk groups, so we will try to replicate these observations using logistic regression
# # Risk assesment for young males ages 17-23
# GES2014$FATIGUED <- ifelse(GES2014$DRIMPAIR == 2, 1, 0)
# GES2014$YOUNGMALE <- ifelse(GES2014$AGE >= 17 & GES2014$AGE <= 23 & GES2014$SEX == 1, 1, 0)
# # svydesign() has an option for finite population correction (FPC); do we need this??
# # To fit the GLM, we conduct logistic regression on I(DRIMPAIR = 2), a Bernoulli response
# GES2014.glm.youngmale <- svyglm(FATIGUED ~ YOUNGMALE, family=binomial(link=logit), design=GES2014.design)
# GES2014.glm.region <- svyglm(FATIGUED ~ factor(REGION), family=binomial(link=logit), design=GES2014.design)
# GES2014.glm.youngmale.quasi <- svyglm(FATIGUED ~ YOUNGMALE, family=quasibinomial, design=GES2014.design)
# GES2014.glm.region.quasi <- svyglm(FATIGUED ~ factor(REGION), family=quasibinomial, design=GES2014.design)
# coef(GES2014.glm.youngmale)
# coef(GES2014.glm.region)
# coef(GES2014.glm.youngmale.quasi)
# coef(GES2014.glm.region.quasi)

# min.model = svyglm(FATIGUED ~ 1, family=binomial(link=logit), design=GES2014.design)
# biggest <- formula(svyglm(FATIGUED~., family=binomial(link=logit), design=GES2014.design))
# biggest


##Conduct forward stepwise regression on the entire data frame
##Per Lumley, this is not a valid approach, because the variable selection criterion employed by step() is AIC, which involves maximum likelihood estimation. 
##Complex surveys do not have a direct likelihood measurement, so we need a more dedicated approach.
# fwd.model <- step(min.model, direction='forward', scope=biggest, steps=8)
# names(fwd.model)

# # ##svcoplot() example
# data(api)
# dclus2<-svydesign(id=~dnum+snum,  weights=~pw,
#                   data=apiclus2, fpc=~fpc1+fpc2)
# svycoplot(api00~api99|sch.wide*comp.imp, design=dclus2, style="hexbin")
# svycoplot(api00~api99|sch.wide*comp.imp, design=dclus2, style="hexbin", hexscale="absolute")
# svycoplot(api00~api99|sch.wide, design=dclus2, style="trans")
# svycoplot(api00~meals|stype,design=dclus2,
#           style="transparent",
#           basecol=function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)],
#           alpha=c(0,1))

##Check all unique values of the vectors

##Plot correlation between time and age
##NOTE: We use AGE_IM and HOUR_IM because they exclude missing values
##We should read the manual to understand their imputation process
# png("GES.2.25%03d.png")
useOuterStrips(svycoplot(AGE_IM~HOUR_IM, design=GES2014.design, style="hexbin", hexscale="absolute"))
useOuterStrips(svycoplot(AGE_IM~HOUR_IM|I(DRIMPAIR==2), design=GES2014.design, style="hexbin", hexscale="absolute"))
useOuterStrips(svycoplot(AGE_IM~HOUR_IM|I(DRIMPAIR==2)*SEX_IM, design=GES2014.design, style="hexbin", hexscale="absolute"))
useOuterStrips(svycoplot(AGE_IM~HOUR_IM, design=GES2014.design, style="trans"))
useOuterStrips(svycoplot(AGE_IM~HOUR_IM|I(DRIMPAIR==2), design=GES2014.design, style="trans"))
useOuterStrips(svycoplot(AGE_IM~HOUR_IM|I(DRIMPAIR==2)*SEX_IM, design=GES2014.design, style="trans"))

svyplot(AGE_IM~HOUR_IM, design=GES2014.design, style="hex")
svyplot(AGE_IM~HOUR_IM, design=GES2014.design, style="trans")
##dev.off()

glm1 <- svyglm(I(DRIMPAIR==2) ~ AGE_IM, family=binomial(link=logit), design=GES2014.design)
glm2 <- svyglm(I(DRIMPAIR==2) ~ SEX_IM, family=binomial(link=logit), design=GES2014.design)
glm3 <- svyglm(I(DRIMPAIR==2) ~ HOUR_IM, family=binomial(link=logit), design=GES2014.design)
glm4 <- svyglm(I(DRIMPAIR==2) ~ MODEL, family=binomial(link=logit), design=GES2014.design)
# # summary(glm1)
# # summary(glm2)
# # summary(glm3)
# # summary(glm4)
glm.interact1 <- svyglm(I(DRIMPAIR==2) ~ AGE_IM*SEX_IM, family=binomial(link=logit), design=GES2014.design)
glm.interact2 <- svyglm(I(DRIMPAIR==2) ~ AGE_IM*HOUR_IM, family=binomial(link=logit), design=GES2014.design)
glm.interact3 <- svyglm(I(DRIMPAIR==2) ~ SEX_IM*HOUR_IM, family=binomial(link=logit), design=GES2014.design)
glm.interact4 <- svyglm(I(DRIMPAIR==2) ~ AGE_IM*SEX_IM*HOUR_IM, family=binomial(link=logit), design=GES2014.design)

# summary(glm.interact1)
# summary(glm.interact2)
# summary(glm.interact3)
# summary(glm.interact4)

plot(c(glm1$deviance, 
       glm2$deviance, 
       glm3$deviance,
       glm4$deviance,
       glm.interact1$deviance,
       glm.interact2$deviance,
       glm.interact3$deviance,
       glm.interact4$deviance),
     type="h",
     ylab="Deviance",
     xaxt = "n",
     main="Deviance Comparison")
axis(1, at=1:8, labels=abbreviate(c("AGE", "SEX", "HOUR", "MODEL", "AS", "AH", "SH", "ASH"), minlength=2), tick=TRUE)
plot(c(glm1$aic, 
       glm2$aic, 
       glm3$aic,
       glm4$aic,
       glm.interact1$aic,
       glm.interact2$aic,
       glm.interact3$aic,
       glm.interact4$aic),
     type="h",
     ylab="AIC",
     xaxt = "n",
     main="AIC Comparison")
axis(1, at=1:8, labels=c("AGE", "SEX", "HOUR", "MODEL", "AS", "AH", "SH", "ASH"), tick=TRUE)
plot(c(glm1$null.deviance, 
       glm2$null.deviance, 
       glm3$null.deviance,
       glm4$null.deviance,
       glm.interact1$null.deviance,
       glm.interact2$null.deviance,
       glm.interact3$null.deviance,
       glm.interact4$null.deviance),
     type="h",
     ylab="Null Deviance",
     xaxt = "n",
     main="Null Deviance Comparison")
axis(1, at=1:8, labels=c("AGE", "SEX", "HOUR", "MODEL", "AS", "AH", "SH", "ASH"), tick=TRUE)
dev.off()

regTermTest(glm1, ~AGE_IM)
regTermTest(glm2, ~SEX_IM)
regTermTest(glm3, ~HOUR_IM)
regTermTest(glm4, ~MODEL)
regTermTest(glm.interact1, ~AGE_IM)
regTermTest(glm.interact2, ~AGE_IM)
regTermTest(glm.interact4, ~AGE_IM)

##The survey package has built-in functions for regression diagnostics. Let's check it out.
##glm.interact4 looks like a good model. It finds age, time and sex to be significant and it gives a low AIC/residual deviance
par(mfrow=c(2,2))
plot(glm.interact4)
title("A*S*H Residual diagnostics (svyglm)", outer=TRUE, line=-2)

##Fit regular GLM and compare diagnostic plots
glm.interact4.noweights <- glm(I(DRIMPAIR==2) ~ AGE_IM*SEX_IM*HOUR_IM, family=binomial(link=logit), data=GES2014)
par(mfrow=c(2,2))
plot(glm.interact4.noweights)
title("A*S*H Residual diagnostics (w/o survey weights)", outer=TRUE, line=-2)

plot(glm.interact4)



load("GES2014.rda")
load("GES2014.design.rda")

GES2014.drivers <- subset(GES2014, PER_TYP == 1)
dim(GES2014.drivers)
dim(GES2014)
GES2014.driver.design <- svydesign(id = ~ PSU + PJ + CASENUM,
                            data = GES2014.drivers,
                            weight = ~ WEIGHT,
                            strata = ~ PSUSTRAT + STRATUM) # Create the survey design object for use in model fitting
save(GES2014.driver.design, file="GES2014.driver.design.rda")

options(survey.lonely.psu="adjust")
fatal.drowsy.glm <- svyglm(I(INJSEV_IM==4)~I(DRIMPAIR==2), 
                           family=binomial(link=logit), 
                           design=GES2014.driver.design)
summary(fatal.drowsy.glm)

fatal.drowsy.cond.sex <- svyglm(I(INJSEV_IM==4)~I(DRIMPAIR==2)|I(SEX_IM==1), 
                                family=binomial(link=logit), 
                                design=GES2014.driver.design)
summary(fatal.drowsy.cond.sex)

fatal.drowsy.sex.inter <- svyglm(I(INJSEV_IM==4)~I(DRIMPAIR==2)*as.factor(SEX_IM), 
                                 family=binomial(link=logit), 
                                 design=GES2014.driver.design)
summary(fatal.drowsy.sex.inter)

fatal.drowsy.cond.night <- svyglm(I(INJSEV_IM==4)~I(DRIMPAIR==2)*I(HOUR_IM %in% 0:6),
                            family=binomial(link=logit), 
                            design=GES2014.driver.design)
summary(fatal.drowsy.cond.night)

fatal.drowsy.cond.night.sex.inter <- svyglm(I(INJSEV_IM==4)~I(DRIMPAIR==2)|SEX_IM:I(HOUR_IM %in% 0:6),
                                  family=binomial(link=logit), 
                                  design=GES2014.driver.design)
summary(fatal.drowsy.cond.night.sex.inter)

fatal.drowsy.cond.night.age.inter <- svyglm(I(INJSEV_IM==4)~I(DRIMPAIR==2)*I(AGE_IM %in% 15:19)*I(HOUR_IM %in% 0:6),
                                            family=binomial(link=logit), 
                                            design=GES2014.driver.design)
summary(fatal.drowsy.cond.night.age.inter)






##5/5/2016
##The conventional logic would state that drowsyness is primarily attributable to it being nighttime.
##So let's condition on it being daytime and see what happens.
load("GES2014.rda")
GES2014$DROWSY <- ifelse(GES2014$DRIMPAIR==2, 1, 0)
GES2014$FATAL <- ifelse(GES2014$INJSEV_IM==4, 1, 0)
GES2014$SEX_IM <- ifelse(GES2014$SEX_IM==2, 1, 0)
GES2014$YOUNG <- ifelse(GES2014$AGE_IM %in% 15:19, 1, 0)
GES2014$DAY <- ifelse(GES2014$HOUR_IM %in% 7:23, 1, 0)
GES2014.factors.drivers <- subset(GES2014, PER_TYP == 1)
save(GES2014.factors.drivers, file="GES2014.factors.drivers.rda")
sort(names(GES2014.factors.drivers))
GES2014.factor.driver.design <- svydesign(id = ~ PSU + PJ + CASENUM,
                                          data = GES2014.factors.drivers,
                                          weight = ~ WEIGHT,
                                          strata = ~ PSUSTRAT + STRATUM) # Create the survey design object for use in model fitting
save(GES2014.factor.driver.design, file="GES2014.factor.driver.design.rda")

options(survey.lonely.psu="adjust")
fatal.drowsy.cond.day <- svyglm(FATAL~DROWSY|DAY,
                                family=binomial(link=logit), 
                                design=GES2014.factor.driver.design)
summary(fatal.drowsy.cond.day)

options(survey.lonely.psu="adjust")
fatal.drowsy.sex.cond.day <- svyglm(FATAL~(DROWSY|DAY)*(SEX_IM|DAY),
                                        family=binomial(link=logit), 
                                        design=GES2014.factor.driver.design)
summary(fatal.drowsy.sex.cond.day)

options(survey.lonely.psu="adjust")
fatal.drowsy.young.cond.day <- svyglm(FATAL~(DROWSY|DAY)*(YOUNG|DAY),
                                    family=binomial(link=logit), 
                                    design=GES2014.factor.driver.design)
summary(fatal.drowsy.young.cond.day)

options(survey.lonely.psu="adjust")
fatal.drowsy.sex.young.cond.day <- svyglm(FATAL~(DROWSY|DAY)*(SEX_IM|DAY)*(YOUNG|DAY),
                                        family=binomial(link=logit), 
                                        design=GES2014.factor.driver.design)
summary(fatal.drowsy.sex.young.cond.day)

##DROWSY:SEX_IM interaction appears to be wildly non-predictive, so let's remove it and leave the rest
##There are others that are not "statistically significant," but may be informative and pedagogical
options(survey.lonely.psu="adjust")
fatal.drowsy.sex.young.cond.day <- svyglm(FATAL~(DROWSY|DAY)+(SEX_IM|DAY)+(YOUNG|DAY)+((DROWSY:YOUNG)|DAY)+((SEX_IM:YOUNG)|DAY)+((DROWSY:SEX_IM:YOUNG)|DAY),
                                          family=binomial(link=logit), 
                                          design=GES2014.factor.driver.design)
summary(fatal.drowsy.sex.young.cond.day)

##Problem: I get the following error: 
# In DROWSY:YOUNG :
# numerical expression has 91899 elements: only the first used
##For some reason, GLM is reading : as requesting a sequence of integers instead of an interaction

##Here is an attempt at a workaround
options(survey.lonely.psu="adjust")
fatal.exp.question.cond.day <- svyglm(FATAL~ ( (DROWSY*SEX_IM*YOUNG)|DAY ) + ( (DROWSY*SEX_IM)|DAY ) + ( (DROWSY*YOUNG)|DAY ) + (DROWSY|DAY),
                                          family=binomial(link=logit), 
                                          design=GES2014.factor.driver.design)
summary(fatal.exp.question.cond.day)

##The problem with our old code is that I() is an indicator function on it's own, but within the lm() function it means the following
##  as is: include a new variable consisting of these variables multiplied;
# I(x^2) means include this variable squared, etc. In other words
# I( ) isolates the mathematic operations inside it.

##I get the following errors when I try to coerce the variables of interest to factors
# Error in model.matrix.default(mt, mf, contrasts) : 
#   variable 1 has no levels
# In addition: Warning messages:
#   1: In Ops.factor(DROWSY, SEX_IM) : '*' not meaningful for factors
# 2: In Ops.factor(DROWSY * SEX_IM, YOUNG) : '*' not meaningful for factors
# 3: In Ops.factor(DROWSY * SEX_IM * YOUNG, DAY) :
#   '|' not meaningful for factors


##5/5/16
options(survey.lonely.psu="adjust")
fatal.dropped <- svyglm(FATAL~(DROWSY|DAY)+(SEX_IM|DAY)+(YOUNG|DAY)+((DROWSY*YOUNG)|DAY)+((SEX_IM*YOUNG)|DAY)+((DROWSY*SEX_IM*YOUNG)|DAY),
                                          family=binomial(link=logit), 
                                          design=GES2014.factor.driver.design)
summary(fatal.dropped)


#5/6/16
#If we include interactions, then we must include the main effects as well
#Read An Exegesis on Linear Models
load("GES2014.factor.driver.design.rda")
options(survey.lonely.psu="adjust")
fatal.full <- svyglm(FATAL~(DROWSY|DAY)*(SEX_IM|DAY)*(YOUNG|DAY),
                        family=binomial(link=logit), 
                        design=GES2014.factor.driver.design)
summary(fatal.full)




##5/12/16
##PER_TYP indicates the type of person injured in that given row. So we want to consider fatality potential for all injured people. Don't subset yet.
##Let's condition on day up-front in order to make our generic function for risk groups work without parsing
load("GES2014.rda")
GES2014$PJSTRAT <- 1
GES2014$DROWSY <- ifelse(GES2014$DRIMPAIR==2, 1, 0)
GES2014$FATAL <- ifelse(GES2014$INJSEV_IM==4, 1, 0)
GES2014$SEX_IM <- ifelse(GES2014$SEX_IM==2, 1, 0)
GES2014$YOUNG <- ifelse(GES2014$AGE_IM %in% 15:19, 1, 0)
GES2014.day <- subset(GES2014, HOUR_IM %in% 7:23)
GES2014.day.design <- svydesign(id = ~ PSU + PJ + CASENUM,
                                data = GES2014.day,
                                weight = ~ WEIGHT,
                                strata = ~ PSUSTRAT + PJSTRAT + STRATUM) # Create the survey design object for use in model fitting
save(GES2014.day.design, file = "GES2014.day.design.rda")
options(survey.lonely.psu="adjust")
fatal.day.full <- svyglm(FATAL ~ DROWSY * SEX_IM * YOUNG,
                     family = binomial(link = logit), 
                     design = GES2014.day.design)
summary(fatal.day.full)







##Coding will be more complex
##Want to know if entire crash involved a drowsy person, a man, a young person, or 2-3/3 of these
##NHTSA reports that drivers aged 21-29 are the high-risk group wrt age, NOT teenagers per se
##http://www.nhtsa.gov/Driving+Safety/Drowsy+Driving
load("GES2014.rda")
GES2014.drivers <- subset(GES2014, PER_TYP == 1)
GES2014.drivers$DRYOUNG <- ifelse(GES2014.drivers$AGE_IM %in% 17:23, 1, 0)











#GES2014$DROWSY <- GES2014$MALE <- GES2014$YOUNG <- GES2014$DROWSY_YOUNG <- GES2014$DROWSY_MALE <- GES2014$DROWSY_YOUNG_MALE <- GES2014$YOUNG_MALE <- 0
# counter <- 0
# for (case in unique(GES2014.drivers$CASENUM))
# {
#   print(case)
#   sub1 <- subset(GES2014.drivers, CASENUM == case)
#   for (veh in unique(sub$VEH_NO))
#   {
#     print(veh)
#     sub2 <- subset(sub1, VEH_NO == veh)
#     if (nrow(sub2) != 1)
#     {
#       print(nrow(sub2))
#     }
#     #GES2014.sel <- subset(GES2014.drivers, CASENUM == case & VEH_NO == veh)
#     if (sub2$AGE_IM %in% 17:23)
#     {
#       #print(GES2014.sel2$AGE_IM)
#       GES2014.drivers$DRYOUNG[GES2014.drivers$CASENUM == case & GES2014.drivers$VEH_NO == veh] <- 1
#       #print(GES2014$YOUNG_VEH[GES2014$CASENUM == case && GES2014$VEH_NO == veh])
#     }
#   }
# }
    # GES2014.sel2 <- subset(GES2014.sel, VEH_NO == veh)
    # if (nrow(GES2014.sel2) != 1)
    #   print(nrow(GES2014.sel2))
    # if (GES2014.sel2$SEX_IM == 1)
    # {
    #   #print(GES2014.sel2$SEX_IM)
    #   GES2014$MALE_VEH[GES2014$CASENUM == case & GES2014$VEH_NO == veh] <- 1
    #   #print(GES2014$MALE_VEH[GES2014$CASENUM == case && GES2014$VEH_NO == veh])
    # }
#   }
# }
##NOTE: There are 5 cases where 2 drivers are listed, and 1 case where no drivers are listed. LET IT GO.


          # if (GES2014.sel2$AGE_IM[GES2014.sel2$PER_NO  == per] %in% 17:23)
          # {
          #   if (GES2014.sel2$DRIMPAIR[GES2014.sel2$PER_NO  == per] == 2)
          #   {
          #     GES2014$DROWSY_YOUNG_MALE[GES2014$CASENUM == case] <- 1
          #     ##Intention: if the accident involves at least one drowsy young male, then we mark all injuries from the accident as dorwsy-YOUNG-male-related
          #     cond <- 1
          #     break
          #   }
          #   else
          #   {
          #     GES2014$YOUNG_MALE[GES2014$CASENUM == case] <- 1
          #     ##Intention: if the accident involves at least one nondrowsy young male, then we mark all injuries from the accident as drowsy-YOUNG-male-related
          #     cond <- 1
          #     break
          #   }
          # }
          # else
          # {
          #   if (GES2014.sel2$DRIMPAIR[GES2014.sel2$PER_NO  == per] == 2)
          #   {
          #     GES2014$DROWSY_MALE[GES2014$CASENUM == case] <- 1
          #     ##Intention: if the accident involves at least one drowsy young male, then we mark all injuries from the accident as dorwsy-YOUNG-male-related
          #     cond <- 1
          #     break
          #   }
          #   else
          #   {
          #     GES2014$MALE[GES2014$CASENUM == case] <- 1
          #     ##Intention: if the accident involves at least one nondrowsy young male, then we mark all injuries from the accident as drowsy-YOUNG-male-related
          #     cond <- 1
          #     break
          #   }
          # }
        # else
        # {
        #   if (GES2014.sel2$AGE_IM[GES2014.sel2$PER_NO  == per] %in% 17:23)
        #   {
        #     if (GES2014.sel2$DRIMPAIR[GES2014.sel2$PER_NO  == per] == 2)
        #     {
        #       GES2014$DROWSY_YOUNG[GES2014$CASENUM == case] <- 1
        #       ##Intention: if the accident involves at least one drowsy young male, then we mark all injuries from the accident as dorwsy-YOUNG-male-related
        #       cond <- 1
        #       break
        #     }
        #     else
        #     {
        #       GES2014$YOUNG[GES2014$CASENUM == case] <- 1
        #       ##Intention: if the accident involves at least one nondrowsy young male, then we mark all injuries from the accident as drowsy-YOUNG-male-related
        #       cond <- 1
        #       break
        #     }
        #   }
        #   else
        #   {
        #     if (GES2014.sel2$DRIMPAIR[GES2014.sel2$PER_NO == per] == 2)
        #     {
        #       GES2014$DROWSY[GES2014$CASENUM == case] <- 1
        #       ##Intention: if the accident involves at least one drowsy young male, then we mark all injuries from the accident as dorwsy-YOUNG-male-related
        #       cond <- 1
        #       break
        #     }
        #   }
        # }
      }
    }
    # if (cond == 1)
    # {
    #   break
    #   ##This is a workaround to do a "double break" out of the veh AND per group. 
    #   ##We want ONLY a single variable to be equal to one for each accident
    # }
  }
  # counter <- counter + 1
  # print(counter)
}
##47: In if (GES2014.sel2$SEX_IM[GES2014.sel2$PER_NO == per] ==  ... :
# the condition has length > 1 and only the first element will be used
# 48: In if (GES2014.sel2$AGE_IM[GES2014.sel2$PER_NO == per] %in%  ... :
# the condition has length > 1 and only the first element will be used
# 49: In if (GES2014.sel2$DRIMPAIR[GES2014.sel2$PER_NO == per] ==  ... :
# the condition has length > 1 and only the first element will be used
# 50: In if (GES2014.sel2$PER_TYP[GES2014.sel2$PER_NO == per] ==  ... :
# the condition has length > 1 and only the first element will be used
# 
GES2014$PJSTRAT <- 1
GES2014$FATAL <- ifelse(GES2014$INJSEV_IM==4, 1, 0)
GES2014.aug <- GES2014
save(GES2014.aug, file = "GES2014.aug.rda")
GES2014.day <- subset(GES2014.aug, HOUR_IM %in% 7:23)
GES2014.day$SEASON <- ifelse(GES2014.day$MONTH %in% c(12, 1, 2), 1,
                             ifelse(GES2014.day$MONTH %in% 3:5, 2,
                                    ifelse(GES2014.day$MONTH %in% 6:8, 3, 4)  ))
GES2014.day$WEEKEND <- ifelse(GES2014.day$WKDY_IM %in% c(1, 7), 1, 0)
GES2014.day <- subset(GES2014.day, INT_HWY != 9)
GES2014.day$ALCHL_IM_DICH <- ifelse(GES2014.day$ALCHL_IM == 1, 1, 0)
# GES2014.day$DROWSY_ANY <- ifelse(GES2014.day$DROWSY == 1 
#                                  | GES2014.day$DROWSY_YOUNG == 1 
#                                  | GES2014.day$DROWSY_MALE == 1
#                                  | GES2014.day$DROWSY_YOUNG_MALE == 1,
#                                  1, 0)
GES2014.day.design <- svydesign(id = ~ PSU + PJ + CASENUM,
                                data = GES2014.day,
                                weight = ~ WEIGHT,
                                strata = ~ PSUSTRAT + PJSTRAT + STRATUM) # Create the survey design object for use in model fitting
save(GES2014.day, file = "GES2014.day.rda")
save(GES2014.day.design, file = "GES2014.day.design.rda")

#load("GES2014.day.design.rda")
options(survey.lonely.psu="adjust")
fatal.day.full <- svyglm(FATAL ~ DROWSY + MALE + YOUNG + DROWSY_YOUNG + DROWSY_MALE + YOUNG_MALE + DROWSY_YOUNG_MALE,
                         family = binomial(link = logit), 
                         design = GES2014.day.design)
summary(fatal.day.full)

risk <- riskScaleSimp(fatal.day.full)
groups <- riskGroups(GES2014.day, risk)
GES2014.day$RISK <- groups
save(GES2014.day, file="GES2014.day.rda")

library(ggplot2)
load("GES2014.day.design.rda")
load("GES2014.day.rda")

par(mfrow=c(1,2))
fatal.counts <- table(GES2014.day$FATAL)
barplot(fatal.counts, main="Daytime count: non-fatal vs. fatal",
        xlab="Class") 
drowsy.counts <- table(GES2014.day$DROWSY)
barplot(drowsy.counts, main="Daytime count: non-drowsy vs. drowsy",
        xlab="Class") 
##114610 accidents btwn 7 AM and 11 PM
##126220 accidents in total


options(survey.lonely.psu="adjust")
drowsy.day.weekend <- svyglm(DROWSY_ANY ~ WEEKEND,
                         family = binomial(link = logit), 
                         design = GES2014.day.design)
summary(drowsy.day.weekend)

options(survey.lonely.psu="adjust")
drowsy.day.season <- svyglm(DROWSY_ANY ~ SEASON,
                             family = binomial(link = logit), 
                             design = GES2014.day.design)
summary(drowsy.day.season)

options(survey.lonely.psu="adjust")
drowsy.day.alchl <- svyglm(DROWSY_ANY ~ ALCHL_IM_DICH,
                            family = binomial(link = logit), 
                            design = GES2014.day.design)
summary(drowsy.day.alchl)

options(survey.lonely.psu="adjust")
drowsy.day.int.hwy <- svyglm(DROWSY_ANY ~ INT_HWY,
                           family = binomial(link = logit), 
                           design = GES2014.day.design)
summary(drowsy.day.int.hwy)

options(survey.lonely.psu="adjust")
drowsy.day.male.veh <- svyglm(DROWSY_ANY ~ MALE_VEH,
                           family = binomial(link = logit), 
                           design = GES2014.day.design)
summary(drowsy.day.male.veh)

options(survey.lonely.psu="adjust")
drowsy.day.young.veh <- svyglm(DROWSY_ANY ~ YOUNG_VEH,
                           family = binomial(link = logit), 
                           design = GES2014.day.design)
summary(drowsy.day.young.veh)

##Problem: There can be a young man in one car, and a drowsy old woman in another, but we only assign a single category to the accident, based on the ordering here
##We should use count variables, so we can have the situation described above, or TWO drwosy young men (where the variable then equals two)


##Problem: NHTSA reports that drivers aged 21-29 are the high-risk group for nodding off wrt age, NOT teenagers per se
##http://www.nhtsa.gov/Driving+Safety/Drowsy+Driving
##How can we check for this while respecting the main effect-interaction inclusion rule in An Exegesis on Linear Models?