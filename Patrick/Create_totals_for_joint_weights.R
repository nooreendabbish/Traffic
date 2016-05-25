##Code for creating a "model matrix" of all of the quantities needed to calculate joint weights (inverse of joint inclusion probability)
geostrat.ind <- 14
psu.ind <- 60
accstrat.ind <- 6
##Each component of pj.ind represents the number of PJ

##Create totals for PSU-geostrat level ("list of length 1," so not coded as a list)
psu.geostrat.totals <- matrix(0, ncol=geostrat.ind, nrow=2, rownames=c("pop", "sample"))

##sorta-recursion fact: the dimension of the i'th PJ-per-PSU matrix (i=1,...,14) is equal to the 
##value at the i'th position of the psus.per.geostrat matrix
##Make a list!
pj.psu.totals <- vector("list", geostrat.ind)
##You should NAME the items in the list so you can reference them with $
sampled.psus.per.geostrat <- rep(0,geostrat.ind)
for (geostrat in 1:geostrat.ind)
  pj.psu.totals[[geostrat]] <-
    matrix(0, ncol=sampled.psus.per.geostrat[geostrat], nrow=2, rownames=c("pop", "sample"))
##How do we lapply a list to create zero-vectors of length P_j (referencing a separate vector of the same length as this list)

##Create totals for PARS-per-accident.strat level
sampled.pjs.per.psu <- rep(0,psu.ind)
par.accstrat.totals <- vector("list", psu.ind)
for (psu in 1:psu.ind)
{
  par.accstrat.totals[[psu]] <- vector("list", pj.ind[psu])
  for (pj in 1:pj.ind[psu])
    par.accstrat.totals[[psu]][[pj]] <- 
      matrix(0, ncol=accstrat.ind, nrow=2, rownames=c("pop", "sample"))
}





##Create matrix of joint weights
load("GES2014.rda")
##joint.weights <- matrix(0, nrow(GES2014)), nrow(GES2014))
##This matrix would be too big!! So we need to just calculate the weights as needed, based on the "short" lists defined above.

joint.weight <- function(obs1, obs2)
{
  
  #Case: two observations in different geostrats
  if (obs1$PSUSTRAT != obs2$PSUSTRAT)
  {
    joint.wt <- obs1$WEIGHT * obs2$WEIGHT
  }
  
  #Case: 2 observations in the same geostrat but different PSUs
  else if (obs1$PSU != obs2$PSU)
  {
    #Probability of selecting the two different PSUs from the same geostrat ("WITHOUT REPLACEMENT")
    psu.geostrat.wt1 <- (psu.geostrat.totals$pop[obs1$PSUSTRAT]                      ) / (psu.geostrat.totals$sample[obs1$PSUSTRAT]                      )
    psu.geostrat.wt2 <- (psu.geostrat.totals$pop[obs2$PSUSTRAT]-1                    ) / (psu.geostrat.totals$sample[obs2$PSUSTRAT]-1                    )
    
    #Probability of selecting the two different PJs from the two different PSUs ("WITH REPLACEMENT")
    pj.psu.wt1       <- (pj.psu.totals[[obs1$PSUSTRAT]]$pop[obs1$PSU]                ) / (pj.psu.totals[[obs1$PSUSTRAT]]$sample[obs1$PSU]                )
    pj.psu.wt2       <- (pj.psu.totals[[obs2$PSUSTRAT]]$pop[obs2$PSU]                ) / (pj.psu.totals[[obs2$PSUSTRAT]]$sample[obs2$PSU]                )
    ##Will need to recode PJs within each PSU to index from 1 to M_j for this to work! 
    
    #Probability of selecting the two different accident strata from the two different PJs ("WITH REPLACEMENT")
    #(treating accstrata of the same index but from different PJs as "different accstrata")
    par.accstrat.wt1 <- (par.accstrat.totals[[obs1$PSU]][[obs1$PJ]]$pop[obs1$STRATUM]) / (par.accstrat.totals[[obs1$PSU]][[obs1$PJ]]$sample[obs1$STRATUM])
    par.accstrat.wt2 <- (par.accstrat.totals[[obs2$PSU]][[obs2$PJ]]$pop[obs2$STRATUM]) / (par.accstrat.totals[[obs2$PSU]][[obs2$PJ]]$sample[obs2$STRATUM])
    
    joint.wt         <- psu.geostrat.wt1 * psu.geostrat.wt2 * pj.psu.wt1 * pj.psu.wt2 * par.accstrat.wt1 * par.accstrat.wt2
  }
  
  #Case: 2 observations in the same PSU but different PJs
  else if (obs1$PJ != obs2$PJ)
  {
    #Probability of selecting the PSU
    psu.geostrat.wt <- (psu.geostrat.totals$pop[obs1$PSUSTRAT]                       ) / (psu.geostrat.totals$sample[obs1$PSUSTRAT]                      )
    
    #Probability of selecting the two different PJs from the same PSU ("WITHOUT REPLACEMENT")
    pj.psu.wt1       <- (pj.psu.totals[[obs1$PSUSTRAT]]$pop[obs1$PSU]                ) / (pj.psu.totals[[obs1$PSUSTRAT]]$sample[obs1$PSU]                )
    pj.psu.wt2       <- (pj.psu.totals[[obs2$PSUSTRAT]]$pop[obs2$PSU]-1              ) / (pj.psu.totals[[obs2$PSUSTRAT]]$sample[obs2$PSU]-1              )
    
    #Probability of selecting the two different PARs from the two different accstrata ("WITH REPLACEMENT")
    #(treating accstrata of the same index but from different PJs as "different accstrata")
    par.accstrat.wt1 <- (par.accstrat.totals[[obs1$PSU]][[obs1$PJ]]$pop[obs1$STRATUM]) / (par.accstrat.totals[[obs1$PSU]][[obs1$PJ]]$sample[obs1$STRATUM])
    par.accstrat.wt2 <- (par.accstrat.totals[[obs2$PSU]][[obs2$PJ]]$pop[obs2$STRATUM]) / (par.accstrat.totals[[obs2$PSU]][[obs2$PJ]]$sample[obs2$STRATUM])
    
    joint.wt         <- psu.geostrat.wt * pj.psu.wt1 * pj.psu.wt2 * par.accstrat.wt1 * par.accstrat.wt2
  }
  
  #Case: 2 observations in the same PJ but different accstrats
  else if (obs1$STRATUM != obs2$STRATUM)
  {
    #Probability of selecting the PSU
    psu.geostrat.wt <- (psu.geostrat.totals$pop[obs1$PSUSTRAT]                       ) / (psu.geostrat.totals$sample[obs1$PSUSTRAT]                      )
    
    #Probability of selecting the PJ
    pj.psu.wt       <- (pj.psu.totals[[obs1$PSUSTRAT]]$pop[obs1$PSU]                 ) / (pj.psu.totals[[obs1$PSUSTRAT]]$sample[obs1$PSU]                )
    
    #Probability of selecting the two different PARs from the two different accstrata ("WITH REPLACEMENT")
    #(treating accstrata of the different index but from the same PJ as "different accstrata")
    par.accstrat.wt1 <- (par.accstrat.totals[[obs1$PSU]][[obs1$PJ]]$pop[obs1$STRATUM]) / (par.accstrat.totals[[obs1$PSU]][[obs1$PJ]]$sample[obs1$STRATUM])
    par.accstrat.wt2 <- (par.accstrat.totals[[obs2$PSU]][[obs2$PJ]]$pop[obs2$STRATUM]) / (par.accstrat.totals[[obs2$PSU]][[obs2$PJ]]$sample[obs2$STRATUM])
    
    joint.wt         <- psu.geostrat.wt * pj.psu.wt * par.accstrat.wt1 * par.accstrat.wt2
  }
  
  #Case: 2 observations in the same accstrat
  else
  {
    #Probability of selecting the PSU
    psu.geostrat.wt <- (psu.geostrat.totals$pop[obs1$PSUSTRAT]                         ) / (psu.geostrat.totals$sample[obs1$PSUSTRAT]                        )
    
    #Probability of selecting the PJ
    pj.psu.wt       <- (pj.psu.totals[[obs1$PSUSTRAT]]$pop[obs1$PSU]                   ) / (pj.psu.totals[[obs1$PSUSTRAT]]$sample[obs1$PSU]                  )
    
    #Probability of selecting the two different PARs from the same accstratum ("WITHOUT REPLACEMENT")
    #(treating ONLY accstrata of the same index AND from the same PJ as "the same accstratum")
    par.accstrat.wt1 <- (par.accstrat.totals[[obs1$PSU]][[obs1$PJ]]$pop[obs1$STRATUM]  ) / (par.accstrat.totals[[obs1$PSU]][[obs1$PJ]]$sample[obs1$STRATUM]  )
    par.accstrat.wt2 <- (par.accstrat.totals[[obs2$PSU]][[obs2$PJ]]$pop[obs2$STRATUM]-1) / (par.accstrat.totals[[obs2$PSU]][[obs2$PJ]]$sample[obs2$STRATUM]-1)
    
    joint.wt         <- psu.geostrat.wt * pj.psu.wt * par.accstrat.wt1 * par.accstrat.wt2
  }
}








