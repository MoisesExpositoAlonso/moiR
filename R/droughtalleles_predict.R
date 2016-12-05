# The idea of this script is to check whether the geography or the environment predicts better the drought alleles


#######################################################################################################################################

# devtools::load_all(pkg="~/ebio/abt6/mexposito/moiR/")
source("distribution_model_functions.R")

#### Read the main table ####

# source("read_accmaster_plusflowering.R")
# accmaster<-read.table("../762_accmaster_env_str_drough.tsv",sep="\t",header=T)
accmaster<-read_accmaster_plus()
head(accmaster,3)


#### read alleles  ####

name="add"
accmaster_add<-read_alleles(name)
wheresnps<-grep("chr", colnames(accmaster_add))

accmaster_add[,wheresnps] [ accmaster_add[,wheresnps] ==1] <- 2


#######################################################################################################################################


