#' Quick scatter plot with points colored by a 3rd variable
#'
#' @param df data frame with the data to be plotted
#' @param chr.col the name of the column where the chromosome numer is found
#' @param pos.col the name of the column where the position of the SNP is found
#' @param stat.col the name of the column to be plotted
#' @param stat.type What kind of statistic is the stat.col: either p-value(p), empirical p-value(ep), or effect size(es).
#' @param chrpalette A vector of colors or single color
#' @param minp the minimum p value to be plotted
#' @param FDRisminp if true, it overwriedes minp, and uses FDR as minimum
#' @param resolution the resolution of base pairs to be plotted in the x axis legend
#' @param significants logical, whether significant (bonferroni) SNPs should be plotted in red
#' @param subset the number of SNPs to subset for the plotting. If -9 all SNPs are plotted
#' @param breaks how many axis breaks you want
#' @param alpha how much transparency you want
#' @return A scatter plot from base plot with points colored by the rank of a given variable
#'
#' @export
#'

ggmanhattan<-function(df,
                      chrpalette = c("black","darkgrey", "black", "darkgrey" ,"black"),
                      chr.col='chr' ,
                      pos.col='pos' ,
                      stat.col='stat' ,
                      stat.type = 'p' ,
                      minp=0.05,
                      FDRisminp=T,
                      resolution=1e6 ,
                      significants=F,
                      subset= (-9),
                      breaks=10 ,
                      alpha=0.8){
library(ggplot2)
library(cowplot)
################################################################################
# Internal checks of arguments
# if(is.na(df) & is.null(df) ){stop('Provide data!')}
if(class(df) != 'matrix' & class(df) != 'data.frame') {stop('Provide a matrix or data frame')}
  if(class(df)=='matrix'){ df=data.frame(df) }
if( any( !c(chr.col,pos.col,stat.col) %in% colnames(df) ) ) {
  stop('Provide the correct column names!')
}else{
  colnames(df)[which(colnames(df) == chr.col)] <- 'chr'
  colnames(df)[which(colnames(df) == pos.col)] <- 'pos'
  colnames(df)[which(colnames(df) == stat.col)] <- 'stat'
  }

if(!stat.type %in% c('p','ep','es')){stop('Provide a type of statistic to plot as "stat" argument')}

################################################################################

# save a copy of raw data
df_backup<-df

# Create the value to plot
if(stat.type=='p'){ df$values= -log10(df$stat)
}else{ df$values= df$stat}

# Create label for y axis
if(stat.type=='p'){ thelabel= "-log10 p-value"
}else if(stat.type=='ep'){ thelabel= "empirical p-value"
}else if(stat.type=='es'){ thelabel= "effect size"
}


# sort by chromosome and position
df<-arrange(df,chr,pos)

# calculate family error
if(stat.type=='p'){
thebonf=0.05/nrow(df_backup)

thefdr=fdr_level(df_backup$stat)
message(paste("the FDR level is: ",thefdr))
message(paste("the Bonferroni level is: ",thebonf))
}

# remove very low significant SNPs
if(stat.type=='p'){
  if(FDRisminp==T) minp=thefdr
  if(thefdr > 0.05){
    minp=0.05
  df<-dplyr::filter(df,stat< minp)
}
}


# define parameters for plotting
if(stat.type=='p' | stat.type=='ep' | stat.type=='es'){
  df$sizes= abs(df$values) / max( abs(df$values),na.rm=T )
}  # i can add other ways of size

numchr=length(unique(df$chr))
df$cumpos<-1:nrow(df)


# Make a nice x axis
minc=min(df$cumpos)
maxc=max(df$cumpos)
seqgenome=round(seq(minc,maxc,length.out = breaks),0)
seqlabel=round(df$pos[seqgenome]/resolution, digits = 0)

# Subset?
if(subset!=(-9)){
  message('Attempting subset')
  subs=sample(x = row.names(df),size = subset,prob = df$sizes)
  df<-df[subs,]
}

# Make plot
p<- ggplot(df,aes(y=values,x=cumpos,group=factor(chr),color=factor(chr),size=sizes) ) +
  geom_point(alpha=alpha,shape=19)+
  ylab(thelabel)+ xlab('Position (Mb)')+
  scale_x_continuous(breaks=seqgenome, labels= seqlabel)+
  scale_color_manual(values=chrpalette,guide=FALSE)+
  scale_size_continuous(range=c(0,2), guide=FALSE)


# Add significance levels
if(stat.type=='p'){
  message('Highlighting significant SNPs')
  # bonferroni
  p=p+geom_hline(yintercept= -log10(thebonf), size = 0.2,linetype=2)
  # FDR
  p=p+geom_hline(yintercept=-log10(thefdr), size = 0.2,linetype=3)
  # the  5%
  p=p+geom_hline(yintercept=-log10(0.05), size = 0.2,linetype=1)

  message('dashed = bonferroni; dotted = fdr; dotdash = 0.05')

if(significants==T){
  top=dplyr::filter(df,stat<thebonf)
  print(top)
  print(class(top))
  p<-p+geom_point(data=top,mapping=aes(y=values,x=cumpos),size=2+1,color='white')+
  geom_point(data=top,mapping=aes(y=values,x=cumpos),size=2+0.75,color='black')+
  geom_point(data=top,mapping=aes(y=values,x=cumpos),size=2+0.5,color='red')
}}


# Extras that could be implemented
# p<-p+ stat_smooth(aes(group=factor(chr)), col='black' )
# p<-p+ facet_wrap(~env,nrow=2)

return(p)
}

