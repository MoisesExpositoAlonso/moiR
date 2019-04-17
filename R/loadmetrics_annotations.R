load_metrics_n_annotations<-function(){
load("dataint/gmetrics.rda")
# head(gmetrics)
# metricscols<-c( "fst" ,"pi" ,"taj" ,"sweep_lr", "sweep_alpha") # perhaps it is unfair to use allele frequency as it will change?
metricscols<-c("maf", "fst" ,"pi" ,"taj" ,"sweep_lr", "sweep_alpha")

### Load data - an
# load('dataint/an.rda')
load('dataint/an01.rda')
head(an)
pryr::mem_used()
ancols<-c("intergenic", "intron", "UTR3", "UTR5" ,"exon", "synonymous", "nonsynonymous" ,"exon_noncoding")


}