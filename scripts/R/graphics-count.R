rm(list = ls())
library(ggplot2)
library(dplyr)
library(foreign)
library(fitdistrplus)
setwd("/home/andrefelipe/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho")
source("/home/andrefelipe/Dropbox/IC_2017/Scripts/R/toLatex.R")
# setwd("C:/Users/User/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho")
dados <- read.dta("couart2.dta")
head(dados)
str(dados)
class(dados)

# Comportamento da variável resposta --------------------------------------
tab <- table(dados$art)
y <- as.numeric(tab)
x <- as.numeric(names(tab))

pdf(file = "freq-plot.pdf", width = 12, height = 7)
par(mar = c(3.1, 3.2, 0.9, 0.4), cex = 2)
plot(1:length(y), y,  lwd = 2, type = "h", xaxt = 'n', yaxt = 'n', 
     xlab = '', ylab = '', ylim = c(0, 300))
points(1:length(x), y+0.05, pch = 19, cex = 0.8)
axis(side = 1, at = 1:length(x), labels = x)
axis(side = 2, at = seq(0, 300, l = 5))
mtext(text = "N° de Publicações", side = 1, line = 2.1, cex = 2)
mtext(text = "Frequência Absoluta", side = 2, line = 2.2, cex = 2)
text(x = 1:length(x), y = y + 15, labels = y)
text(x = 13, y = 280, labels = paste("E(Y) =", round(mean(dados$art), 4)))
text(x = 13, y = 250, labels = paste("V(Y) =", round(var(dados$art), 4)))
graphics.off()

xx <- sort(dados$art)
Fn <- ecdf(xx)
bn <- coef(fitdist(data = dados[["art"]], dist = "nbinom", discrete = TRUE))

pdf(file = "cum-plot.pdf", width = 12, height = 7)
par(mar = c(3.1, 3.2, 0.9, 0.4), cex = 2)
plot(xx, Fn(xx), type = "S", xaxt = 'n', yaxt = 'n', 
     xlim = c(0, 20), ylim = c(0.25, 1),
     xlab = '', ylab = '')
lines(xx, ppois(xx, lambda = mean(xx)), col = "blue", lwd = 2)
lines(xx, pnbinom(xx, size = bn[1], mu = bn[2]), col = "red", lwd = 2)
legend("bottomright", legend = c("Empírica", "Poisson", "Binomial Negativa"), 
       col = c("black", "blue", "red"), lwd = 2, bty = "n", inset = 0.02)
axis(side = 1, at = seq(0, 20, l = 5))
axis(side = 2, at = seq(0.25, 1, l = 5), labels = FF(seq(0.25, 1, l = 5), 2))
mtext(text = "N° de Publicações", side = 1, line = 2.1, cex = 2)
mtext(text = "Probabilidade Acumulada", side = 2, line = 2.2, cex = 2)
graphics.off()




# Usando o ggplot2 --------------------------------------------------------
aux <- as.data.frame(table(dados$art))
aux$Freq <- as.numeric(aux$Freq)

theme_set(theme_bw())
ggplot(aux, mapping = aes(x = Var1, y = Freq)) +
  geom_point(size = 2) +
  geom_segment(aes(x = Var1, xend = Var1, y = 0, yend = Freq)) +
  geom_text(aes(label=Freq), vjust = -1) + 
  # scale_x_continuous(limits = c(0, 19), breaks = 0:19, minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, l = 5), minor_breaks = NULL) +
  labs(y = "Frequência Absoluta\n", x = "\nNº de Publicações") +
  theme(text = element_text(size = 12),
        axis.ticks.length = unit(0.3,"cm"),
        panel.border = element_rect(colour = "grey")) 
ggsave("lollipop.pdf", width = 8, height = 5)


bysex <- as.data.frame(table(dados$art, dados$fem))
# bysex$Var1 <- as.numeric(levels(bysex$Var1))
bysex$Freq <- as.numeric(bysex$Freq)
bysex$Freq <- with(bysex, ifelse(test = Freq == 0, NA, Freq))
bysex$Var2 <- with(bysex, ifelse(test = Var2 == "Men", "Homens", "Mulheres"))

ggplot(bysex, mapping = aes(x = Var1, y = Freq)) +
  facet_grid(~ Var2, scales = "fixed") + geom_point(size = 2) +
  geom_segment(aes(x = Var1, xend = Var1, y = 0, yend = Freq)) +
  geom_text(aes(label=Freq),hjust=-1) + 
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, l = 5), minor_breaks = NULL) +
  labs(y = "Frequência Absoluta\n", x = "\nNº de Publicações") +
  theme(text = element_text(size = 16),
        axis.ticks.length = unit(0.3,"cm"),
        panel.border = element_rect(color = "grey", fill = NA),
        strip.background = element_rect(color = "grey")) +
  coord_flip()
ggsave("bysex.pdf", width = 12, height = 6)

bysex <- as.data.frame(table(dados$art, dados$mar))
# bysex$Var1 <- as.numeric(levels(bysex$Var1))
bysex$Freq <- as.numeric(bysex$Freq)
bysex$Freq <- with(bysex, ifelse(test = Freq == 0, NA, Freq))
bysex$Var2 <- with(bysex, ifelse(test = Var2 == "Men", "Homens", "Mulheres"))

ggplot(bysex, mapping = aes(x = Var1, y = Freq)) +
  facet_grid(~ Var2, scales = "fixed") + geom_point(size = 2) +
  geom_segment(aes(x = Var1, xend = Var1, y = 0, yend = Freq)) +
  geom_text(aes(label=Freq),hjust=-1) + 
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, l = 5), minor_breaks = NULL) +
  labs(y = "Frequência Absoluta\n", x = "\nNº de Publicações") +
  theme(text = element_text(size = 14),
        axis.ticks.length = unit(0.3,"cm"),
        panel.border = element_rect(color = "grey", fill = NA),
        strip.background = element_rect(color = "grey")) +
  coord_flip()
ggsave("bysex.pdf", width = 12, height = 6)


bysex <- as.data.frame(table(dados$art, interaction(dados$fem, dados$mar)))
media     <- with(dados, tapply(art, interaction(fem, mar), mean))
variancia <- with(dados, tapply(art, interaction(fem, mar), var))
plot(variancia ~ media);abline(lsfit(media, variancia))

names(dados)
media     <- with(dados, tapply(art, ment, mean, na.rm = T))
variancia <- with(dados, tapply(art, ment, var, na.rm = T))
plot(variancia ~ media, xlim = c(0, 80), ylim = c(0, 80));abline(lsfit(media, variancia))
abline(a = 1, b = 1, lty = 2)


names(dados)
media     <- with(dados, tapply(art, phd, mean, na.rm = T))
variancia <- with(dados, tapply(art, phd, var, na.rm = T))
plot(variancia ~ media, xlim =  c(0, 50));abline(lsfit(media, variancia))


media     <- tapply(dados$art, dados$kid5, mean)
variancia <- tapply(dados$art, dados$kid5, var)
plot(variancia ~ media)

table(dados$kid5)
str(dados)











