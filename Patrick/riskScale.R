##Function for creating ordinal "risk scale" from regression output

riskScale <- function(logistic.glm.obj)
{
  ##Initialize
  betas <- coef(logistic.glm.obj)
  vec <- vector("numeric")
  names <- vector("character")
  start <- end <- 1
  subvec.length <- 0

  ##Fill the vector with summed risk values
  for (comb.size in 1:length(betas[-1]))
  {
    subvec.length <- choose((length(betas[-1])), comb.size)
    end <- start + subvec.length - 1
    vec[start:end] <- betas[1] + combn(betas[-1], comb.size, sum)
    names[start:end] <- combn(betas[-1], comb.size, function(x) paste0(names(x), sep=", ", collapse=""))
    start <- end + 1
  }
  return(data.frame(order = 1:length(vec),
                    odds = sort(vec),
                    names = names[order(vec)]))
}

##ADJUST THE CODE TO INCLUDE THE BASELINE (JUST INTERCEPT!!!!)