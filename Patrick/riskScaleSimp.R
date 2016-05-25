riskScaleSimp <- function(logistic.glm.obj)
{
  newData <- unique(model.matrix(logistic.glm.obj))
  betas <- coef(logistic.glm.obj)
  risk.mat <- data.frame(risk = newData %*% betas, newData)
  #risk.mat <- data.frame(risk.classes, newData)
  colnames(risk.mat) <- c("risk", "(Intercept)",
                          attr(terms(logistic.glm.obj), "term.labels"),
                          attr(terms(logistic.glm.obj), "response"))
  risk.mat[order(risk.mat[,"risk"]), ]
}
