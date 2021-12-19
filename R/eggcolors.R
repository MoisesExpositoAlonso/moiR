eggcolors<-function(n=12){
  egg<-
  c(
    "#683514",
    "#96703B",
    "#948A64",
    "#ACB096",
    "#A4C8C1",
    "#A6D9F2",
    "#D3EDFB",
    "#F7F7F7",
    "#D6D0D8",
    "#C8ACA0",
    # "#D2D0DE",
    "#DFA185",
    "#D17A56",
    "#9F2D21",
    "#43120E")
  colorRampPalette(egg)(n)
}

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
num=10
ggplot(data.frame(x=1:num)) +
  geom_point(aes(x=x, y=0,color=x) ,size=10)+
  scale_color_gradientn(colors = eggcolors(num))

