generate_data = function(n, p) {
  covariates = matrix(rnorm(n*p, mean = 0, sd =1),n,p)
  responses = rnorm(n,mean = 0, sd=1)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses,cutoff) {
  v = c()
  for(i in 1:nrow(covariates)){
    if(summary(lm(responses~covariates[,i]))$coefficients[,4][2]<=cutoff){
      v = c(v, i)
    }
  }
  if(is.null(v)) {
    return(c())
  }
  else {
    return(summary(lm(responses~covariates[,v]))$coefficients[,4])
  }
}







