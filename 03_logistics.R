##' Logistic model
##'
##' @param theta  parameter vector
##' @param x      vector of x values
##' @return vector of model predictions
pred_logistic <- function(theta, x){
  z <- exp(theta[3]+theta[4]*x)
  Ey <- theta[1]+theta[2]*z/(1+z) 
}

##' Fit logistic model
##' 
##' @param dat  dataframe of day of year (doy), gcc_mean, gcc_std
##' @param par  vector of initial parameter guess
##' @return  output from numerical optimization
fit_logistic <- function(dat, par){
  
  fit <- nls(gcc_mean ~ pred_logistic(theta = theta, x = doy), data = dat, start = list(theta = par))
  
  fit$m$getPars()
  
}

```