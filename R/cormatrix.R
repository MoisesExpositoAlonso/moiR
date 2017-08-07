#' Correlations of all pairs o variables.
#'
#' @param method the methods inherited from rcorr. Default "pearson". Other possibility is "spearman".
#' @param dat data frame or matrix
#' @param columns the columns in the data frame to do the pairwise correlations
#' @param digits digits for the output
#'
#' @return If a file name is indicated, an output pdf plot. Otherwise, prints to the graphic device.
#' @return A matrix with lower triangle R pearson correlations, and upper triangle the p-values
#'
#' @examples
#' library(MASS)
#' data(mtcars)
#' cormatrix_r_p(as.numeric(na.omit(mtcars)))
#'
#' @export
#'

cormatrix_r_p<-function(dat,columns=NULL,method="pearson", digits=3){

  # check columns to analyse
  if(is.null(columns)){columns=colnames(dat)}

  # check that dataset is good
  dat<-dat[,columns]
  dat<-apply(dat,2,fn)


  # get correlations
  res<-matrix(nrow = length(columns),
           ncol = length(columns))
  colnames(res) = rownames(res) = columns

  dm=expand.grid(columns, columns)
  dm=subset(dm,dm$Var1 != dm$Var2)

  for(i in 1:nrow(dm)){
        v1= fc(dm[i,1])
        v2= fc(dm[i,2])
        message(paste(v1,"<->",v2))

        tmpdat=data.frame(v1=fn(dat[,v1]), v2=fn(dat[,v2]))
        tmpdat=na.omit(tmpdat)

        if(nrow(tmpdat) == 0){
          message('Too many NAs')
          res[v1,v2] <- NA
          res[v2,v1] <- NA
        }else{
          tmp=cor.test( method=method,
          fn(dat[,v1]),
          fn(dat[,v2]))

          res[v1,v2] <- fn(round(tmp$estimate,digits=digits))
          res[v2,v1] <- fn(format(tmp$p.value, digits=digits))

        }
    }


return(res)
}



#' Plot a matrix
#' see https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
#' @param thecor matrix, ideally generated with cormatrix_r_p so r is in the upper triangle and p in the lower triangle
#'
#' @return
#' @export
#'
#' @examples

plot.cormatrix_r_p<-function(thecor, pvals=FALSE,...){

## Genearte two half matrices
# p values
pv=thecor
pv[upper.tri(pv)]<-NA
# if(stars==TRUE) pv[!is.na(pv)] <- starpvalue(pv[!is.na(pv)] )

# correlations
co=thecor
diag(co)<-1
co[lower.tri(co)]<-NA

## Plot

if(pvals ==TRUE){
corrplot::corrplot(co,p.mat=pv, na.label=' ',sig.level = .Machine$double.xmin,insig = "p-value",tl.col = "black")
}else{
corrplot::corrplot(co,p.mat=pv, na.label=' ',sig.level = 0.05,tl.col = "black")
}
}

# cormatrix_r_p.<-function(numericdatanona,columnpos=NULL,filename=NULL,method="pearson") {
#
# if(is.null(columnpos)){columnpos=1:dim(numericdatanona)}
#
# # check that dataset is good
# numericdatanona<-numericdatanona[,columnpos]
# numericdatanona<-na.omit(apply(numericdatanona,2,as.numeric) )
#
#
# # get correlations
# requie("Hmisc")
# rcorr(numericdatanona)
# rp<-rcorr(numericdatanona,method=method)
# attributes(rp)
# rp$r
# rp$r[ lower.tri(rp$r) ]
# dummy<-rp$r
# dummy[ lower.tri(dummy) ] = rp$P[lower.tri(rp$P) ]
#
# # perhaps write
# if(!is.null(filename)){
# write.csv(dummy,file =paste(filename,".csv",sep=""))
# }
#
# return(dummy)
#
# }
