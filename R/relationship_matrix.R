#' Calculate a genetic relationship matrix
#'
#' @param mat A matrix of nrows individuals andncol SNPs. The notation must be allele counts, 0, 1, 2 or NA if missing. No column of individual names.
#'
#' @export
#'

# relationship_matrix<-function(mat){
# #   Ajk<- (1/N) * sum [ ( xij-2pi) (xij - 2pi) / 2pi(1-pi) ]
# # A jk = genetic relationship between individuals j & k
# # x ij = number of copies of reference allele for the i th SNP of the j th individual
# # p i = the frequency of the reference allele
# # N number of SNPs
# # ind number of individuals
#
# N=dim(mat)[2]
# ind=dim(mat)[1]
#
#
# # p= apply(mat,2,FUN=function(x) table(x == "2")["TRUE"]) / apply(mat,2,FUN = function(x) table(is.na(x) )["FALSE"] )
# p= apply(mat,2,FUN=function(x) table(x == "2")["TRUE"]) / ind
#
# Amat<-sapply(1:ind, function(j)
#   sapply(1:ind, function (k)
#       if (j!=k)
#         {
#         xij<-as.numeric(mat[c(j),])
#         xik<-as.numeric(mat[c(k),])
#         form<-( ((xij-(2*p))*(xik-(2*p))) / ((2*p)*(1-p)) )
#         # Ajk<-sum(form,na.rm=T)/sum(!is.na(form))
#         Ajk<-sum(form,na.rm=T)/N
#         return(Ajk)
#         }
#       else return(2)
#     )
#   )
#
# return(Amat)
#
# }

GRM<-function(mat){
# Yang et al 2010 Nature Genetics

# Ajk<- (1/N) * sum [ ( xij-2pi) (xij - 2pi) / 2pi(1-pi) ]
# if j=k, then: Ajj<- (1/N) * sum [ ( xij^2 -(1 + 2pi)xij + 2pi^2 ) / 2pi(1-pi) ]
# A jk = genetic relationship between individuals j & k
# x ij = number of copies of reference allele for the i th SNP of the j th individual
# p i = the frequency of the reference allele
# N number of SNPs
# ind number of individuals

N=dim(mat)[2]
ind=dim(mat)[1]


p= apply(mat,2,FUN=function(x) table(x == "2")["TRUE"]) / apply(mat,2,FUN = function(x) table(is.na(x) )["FALSE"] )

selected<-which(p > 0.01 & p< 0.99)

message(paste("using only alleles with MAF > 1%, in total:", length(selected) ) )

p<-p[selected]


Amat<-sapply(1:ind, function(j)
      sapply(1: ind, function (k)
      if (j!=k) {
        xij<-as.numeric(mat[c(j),selected])
        xik<-as.numeric(mat[c(k),selected])
        form<-( ((xij-(2*p))*(xik-(2*p))) / ((2*p)*(1-p)) )
        Ajk<-sum(form,na.rm=T)/table(!is.na(form))["TRUE"]
      return(Ajk)
      }

      else if (j==k) {
      #   xij<-as.numeric(mat[c(j),selected])
      #   form<-( xij^2 -(1 + 2*pi)*xij + 2*pi^2 ) / 2*pi*(1-pi)
      #   Ajj<-sum(form,na.rm=T)/ table(!is.na(form))["TRUE"]
      # return(Ajj)
        return(2) # work around for Arabidopsis, where everybody are homozygous
      }
))


colnames(Amat)=rownames(Amat)=rownames(mat)
return(Amat)

}

solveAmat=function(Amat){
  Ainv<-as(solve(Amat), "dgCMatrix")
  return(Ainv)
}


# GRM.2<-function(mat){
# Z=mat
# Z[is.na(Z)]=1
# Zoriginal=Z
#
#
# N=dim(Z)[2]
# ind=dim(Z)[1]
#
#
#
# p= apply(Z,2,FUN=function(x) sum(x)) / apply(Z,2,FUN=function(x) length(x)*2)
# k= 2 * sum(p * (1-p) )
#
# centering= rbind(0-2*p,
#                  1-2*p,
#                  2-2*p
#                  )
# Z.cent= apply(,function(x) return(centering[Z[,x],x]) )
# Z.cent=do.call(cbind,Z.cent)
#
# Amat1=( Z %*% t(Z) )/ k
# Amat2=( Z.cent %*% t(Z.cent) )/ k
#
# plot((Amat2) ~ (Amat1))
#
# http://nce.ads.uga.edu/wiki/lib/exe/fetch.php?media=uga_pregs.pdf
# }



#
#
# library(pedigree)
# # gblup
# M_nona<-M
# M_nona[is.na(M_nona)]=1 # I am going to treat the NAs as heterozygotes. The relationship matrix will be probably underestimated, but does not accept NAs.
#
# Amatnew= calcG(M_nona)
# plot(fn(Amatnew) ~ fn(normaldist))
#
# # dim(Amatnew)
# # hist(fn(Amatnew))
# #
# example(gblup)
# G <- calcG(M)
# # Ginv <- calcG(M,solve = TRUE)
#
# M[1:5,1:5]
# M_nona[1:5,1:5]
#
# diag(Amatnew)

