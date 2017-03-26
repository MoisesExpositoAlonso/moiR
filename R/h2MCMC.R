h2MCMC=function(mcmcglmmobject, randomname=colnames(mcmcglmmobject$VCV)){


allvariance=apply(mcmcglmmobject$VCV, 1, sum)
posterior<-mcmcglmmobject$VCV[,randomname]/ allvariance

HPDinterval(posterior,0.95)
posterior.mode(posterior)

}
