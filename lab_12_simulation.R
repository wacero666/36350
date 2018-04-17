generate_data(n, p) {
  covariates = matrix(rnorm(n*p, mean = 0, sd =1),n,p)
  responses = rnorm(n,mean = 0, sd=1)
  return(list(covariates = covariates, responses = responses))
}