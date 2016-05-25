library(survey)

load("GES2014.drivers.day.rda")
load("GES2014.drivers.day.design.rda")
options(survey.lonely.psu="adjust")

predictors <- c("WEEKEND", "SUMMER", "ALCHL_IM", "INT_HWY", "SEX_IM", "DRYOUNG", 
                "SPEEDREL")
response <- "DROWSY"

fwd.stepwise.AIC <- function (predictors, response)
{
  fmlas <- paste0(response, " ~ ", predictors)
  length.save <- length(predictors)
  AIC.save <- rep(0, length.save)
  effect <- rep("", length.save)
  
  for (i in 1:length.save)
  {
    Models <- lapply(fmlas, function(x) svyglm(x,
                                               family = quasibinomial(link = logit), 
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

fwd.stepwise.AIC(predictors, response)