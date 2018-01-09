makeprior<-function(randomfact=1,levels=3){

  start<-"list(R = list(V = 1e-07, nu = -2),G = list("
  end<-"))"
  themeat<-paste(paste0("G",1:randomfact, "=list(V=diag(",levels,"), n=",levels,")"),collapse=',')

  prior<-eval(expr = parse(text= paste(start,themeat,end)))

  return(prior)

}

