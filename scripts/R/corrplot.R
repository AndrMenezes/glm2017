rm(list = ls())
setwd('C:/Users/User/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho/scripts')
library(ggplot2)
library(reshape2)
###########################################
#### Ajuste do banco
corrmat <- as.matrix(read.delim('corrpar.txt')[,-1])
rownames(corrmat) <- colnames(corrmat)
head(corrmat)
#### Gráfico
corrpar.plot <- function(corrmat)
{
  corrmat[lower.tri(corrmat)]<- NA
  corrmat <- corrmat %>% melt(na.rm = T)
  ggplot(data = corrmat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "black")+
    scale_fill_gradient2(low ="gray100", mid = "gray50", high ="gray0", midpoint = 0,
                         limit = c(-1,1), name="") + theme_bw()+ 
    geom_text(aes(Var2, Var1, label = value), color = "white", size = 6) +
    scale_y_discrete(position = "right", labels = c(expression(beta[0]), expression(beta[1]), expression(beta[2]),
                                                    expression(beta[3]), expression(beta[4]), expression(phi))) +
    scale_x_discrete(labels = c(expression(beta[0]), expression(beta[1]), expression(beta[2]),
                                expression(beta[3]), expression(beta[4]), expression(phi))) +
    theme(axis.title.x = element_blank(), axis.text = element_text(size = 20, colour = "black"), 
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top", legend.text = element_text(size = 15))+
    guides(fill = guide_colorbar(barwidth = 10, barheight = 2)) -> corrplot
  return(corrplot)
}
###########################################
corrpar.plot(corrmat)
setwd(gsub(getwd(), pattern = "/scripts", replacement = ""))
ggsave(filename = 'corrplotbn.pdf', width = 8, height = 6, dpi = 300)
###########################################
