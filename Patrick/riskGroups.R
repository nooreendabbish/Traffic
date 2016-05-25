##Label each row of GES2014 wrt ordinal risk group

riskGroups <- function(data.obj, risk.scale.mat, logistic.glm.obj)
{
  risk.scale.trunc <- risk.scale.mat[, 3:length(attr(terms(logistic.glm.obj), "dataClasses"))]
  ##newnames <- match(old.names), colnames.in.new.line.6)
  factor.mat <- data.obj[, colnames(data.obj) %in% colnames(risk.scale.trunc)]
  factor.mat <- factor.mat[, match(colnames(risk.scale.trunc), colnames(factor.mat))[1:ncol(factor.mat)]]
  factor.mat <- cbind(data.obj$WEIGHT,
                      data.obj[as.character(attributes(terms(logistic.glm.obj))$variables[[2]])],
                      factor.mat)
  names(factor.mat)[names(factor.mat)=="data.obj$WEIGHT"] <- "WEIGHT"
  #factor.mat <- na.omit(factor.mat)
  factor.mat <- na.exclude(factor.mat)
  groups <- rep(0, nrow(factor.mat))
  for (row in 1:nrow(factor.mat))
  {
    for (row.comp in nrow(risk.scale.trunc):1)
    {
      if (all(factor.mat[row, 3:ncol(factor.mat)]==risk.scale.trunc[row.comp, ]))
      {
        groups[row] <- row.comp
        break
      }
    }
    if (row %% 1000 == 0)
      print(row)
  }
  ##Consider using outer() here instead (Define a function that breaks when equal rows are identified)
  factor.mat$RISK <- groups
  factor.mat
}

# groups <- riskGroups(GES2014.aug, risk)
# GES2014.aug$RISK <- groups
# save(GES2014.aug, file="GES2014.aug.rda")