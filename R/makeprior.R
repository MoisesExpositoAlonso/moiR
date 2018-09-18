makeprior<-function(randomfact=1,levels=3){

  start<-"list(R = list(V = 1e-07, nu = -2),G = list("
  end<-"))"
  themeat<-paste(paste0("G",1:randomfact, "=list(V=diag(",levels,"), n=",levels,")"),collapse=',')

  prior<-eval(expr = parse(text= paste(start,themeat,end)))

  return(prior)

}


# makeprior<-function(randomfact=1,levels=3, Rv=1e-7, Rnu=-2, Gv=1e-10, Gnu=-1){
# #
# # prior2b = list(R = list(V = 1, fix = 1), G = list(G1 = list(V = 1e-10,nu = -1)))
# # prior2b
# #
# # prior<-  list(R=list(V=diag(2)*(0.002/1.002),nu=1.002),
# #               G=list(G1=list(V=diag(2)*(0.002/1.002),nu=1.002),
# #                      G2=list(V=diag(2)*(0.002/1.002),nu=1.002)) )  # Non informative prior
#   start<-paste("list(R = list(V = ",Rv,", nu =",Rnu,"),G = list(")
#   end<-"))"
#   makeg<-function(levels, value){
#     x=diag(levels) ; diag(x) <-value
#     return(x)
#   }
#   themeat<-paste(paste0("G",1:randomfact, "=list(V=makeg(",levels,",",Gv,"), nu=",Gnu,")"),collapse=',')
#
#   prior<-eval(expr = parse(text= paste(start,themeat,end)))
#   prior$G
#   return(prior)
#
# }
# makeprior(1,1)
