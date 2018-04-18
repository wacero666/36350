generate_data = function(n, p) {
  covariates = matrix(rnorm(n*p, mean = 0, sd =1),n,p)
  responses = rnorm(n,mean = 0, sd=1)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses,cutoff) {
  v = c()
  for(i in 1:ncol(covariates)){
    a = summary(lm(responses~covariates[,i]))$coefficients[,4]
    if(a[2] <=cutoff){
      v = c(v, a[-1])
    }
  }
  if(is.null(v)) {
    return(c())
  }
  else {
    return (v)
  }
}

run_simulation = function(n_trials, n, p, cutoff) {
  k = c()
  for (ni in n){
    for(pi in p){
      data = generate_data(ni, pi)
      k = c(k,model_select(data$covariates, data$responses, cutoff))
    }
  }
 
  hist(k)
}





