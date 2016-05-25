#load("GES2014.aug.rda")
GES2014.aug <- groups

GES2014.aug$NORM_WEIGHT_DICH <- ifelse(GES2014.aug[, 2] == 0, 
                                      GES2014.aug$WEIGHT/sum(GES2014.aug$WEIGHT[GES2014.aug[, 2] == 0]),
                                      GES2014.aug$WEIGHT/sum(GES2014.aug$WEIGHT[GES2014.aug[, 2] == 1]))
group.num <- length(unique(GES2014.aug$RISK))
sens <- spec <- sens_unweighted <- spec_unweighted <- rep(0, group.num)
for (i in 1:group.num)
{
  GES2014.aug.sub <- subset(GES2014.aug, RISK <= i)
  sens[i] <- sum(GES2014.aug.sub$NORM_WEIGHT_DICH[GES2014.aug.sub[, 2] == 1])
  spec[i] <- sum(GES2014.aug.sub$NORM_WEIGHT_DICH[GES2014.aug.sub[, 2] == 0])
  sens_unweighted[i] <- length(GES2014.aug.sub$NORM_WEIGHT_DICH[GES2014.aug.sub[, 2] == 1])/length(GES2014.aug$WEIGHT[GES2014.aug[, 2] == 1])
  spec_unweighted[i] <- length(GES2014.aug.sub$NORM_WEIGHT_DICH[GES2014.aug.sub[, 2] == 0])/length(GES2014.aug$WEIGHT[GES2014.aug[, 2] == 0])
  # for (row in 1:nrow(GES2014.aug))
  # {
  #   if (GES2014.aug[row, 2] == 1)
  #   {
  #     sens[i] <- sens[i] + GES2014.aug$NORM_WEIGHT_DICH[row]
  #     sens_unweighted[i] <- sens_unweighted[i] + 1/length(GES2014.aug$WEIGHT[GES2014.aug[, 2] == 1])
  #   }
  #   else
  #   {
  #     spec[i] <- spec[i] + GES2014.aug$NORM_WEIGHT_DICH[row]
  #     spec_unweighted[i] <- spec_unweighted[i] + 1/length(GES2014.aug$WEIGHT[GES2014.aug[, 2] == 0])
  #   }
  # }
  print(i)
}


par(pty="s")
##attach(sens_spec_comp)
plot(sens, spec, pch=19, cex=1.5,
     main = "ROC, DROWSY ~ SEX_IM*SPEEDREL*SUMMER*DRYOUNG (best wrt AIC)")
points(sens_unweighted, spec_unweighted, pch=19, cex=1.5, col="red")
abline(a=0, b=1, col="grey80")

sens_spec_comp <- data.frame(sens = sens,
                             spec = spec,
                             sens_unweighted = sens_unweighted,
                             spec_unweighted = spec_unweighted)
save(sens_spec_comp, file="sens_spec_comp.rda")
##Make this a function