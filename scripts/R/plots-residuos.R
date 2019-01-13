rm(list = ls())
setwd("C:/Users/User/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho/scripts")
library(tidyverse)
library(gridExtra)
library(hnp)
library(MASS)
library(plyr)
FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}
#######################################################################################
dados <- read.csv(file = "residuos.csv")
str(dados)
sort(names(dados))
setwd(gsub(getwd(), pattern = "/scripts", replacement = ""))
tema <- theme(text = element_text(size=20), panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(size = 1.2))
dados$fem <- relevel(dados$fem, "Women")
mod <- glm.nb(art ~ fem + mar + kid5 + ment, data = dados)
#######################################################################################
## Resíduos versus Preditor Linear (neta) --- avaliação dos resíduos
dados %>% ggplot(aes(x=xb, y=rpearson)) + 
  geom_point(size = 2) + tema + 
  scale_y_continuous(limits = c(-3, 7), breaks = seq(-3, 7, by = 1)) +
  theme(text = element_text(size=20)) +
  geom_hline(yintercept = c(-3, 3), col = "red", lty = 3, size = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  labs(x = expression(hat(eta)), y = "Resíduos de Pearson") 
# labs(x = "Valores Preditos", y = "Resíduos de Pearson") 
ggsave(filename = 'rpearson.pdf', device = "pdf", width = 10, height = 6, dpi = 300)

dados %>% ggplot(aes(x=xb, y=rdeviance)) + 
  geom_point(size = 2) + tema +
  scale_y_continuous(limits = c(-3, 4), breaks = seq(-4, 4, by = 1)) +
  geom_hline(yintercept = c(-3, 3), col = "red", lty = 3, size = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  labs(x = expression(hat(eta)), y = "Resíduos da Deviance") 
# labs(x = "Valores Preditos", y = "Resíduos da Deviance") 
ggsave(filename = 'rdeviance.pdf', device = "pdf", width = 10, height = 6, dpi = 300)

dados %>% ggplot(aes(x=xb, y=sdpearson)) + geom_point(size = 2) + tema +
  scale_y_continuous(limits = c(-3, 7), breaks = seq(-3, 7, by = 1)) +
  theme(text = element_text(size=20)) +
  geom_hline(yintercept = c(-3, 3), col = "red", lty = 3, size = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  labs(x = expression(hat(eta)), y = expression(paste(sqrt("Resíduos de Pearson"))))
  # labs(x = "Valores Preditos", y = expression(paste(sqrt("Resíduos de Pearson"))))
ggsave(filename = 'srpearson.pdf', device = "pdf", width = 10, height = 6, dpi = 300)

dados %>% ggplot(aes(x=xb, y=sdeviance)) + geom_point(size = 2) + tema +
  scale_y_continuous(limits = c(-3, 4), breaks = seq(-4, 4, by = 1)) +
  theme(text = element_text(size=20)) +
  geom_hline(yintercept = c(-3, 3), col = "red", lty = 3, size = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  labs(x = expression(hat(eta)), y = expression(paste(sqrt("Resíduos da Deviance"))))
  # labs(x = "Valores Preditos", y = expression(paste(sqrt("Resíduos da Deviance"))))
ggsave(filename = 'srdeviance.pdf', device = "pdf", width = 10, height = 6, dpi = 300)

## Preditor Linear (neta) versus vetor resposta ajustado (z) --- avaliação da função de ligação
dados %>% mutate(z = xb + (art - predito) / predito) -> dados
dados %>% ggplot(aes(x=xb, y=z)) + 
  geom_point(size = 2) + tema + geom_smooth(method = "lm", se = F) +
  scale_y_continuous(limits = c(-1.2, 8), breaks = seq(-1.2, 8, l = 5)) +
  scale_x_continuous(limits = c(-0.12, 2.6), breaks = seq(-0.12, 2.6, l = 5)) +
  labs(x = expression(hat(eta)))
ggsave(filename = 'link1.pdf', device = "pdf", width = 10, height = 6, dpi = 300)

## Resíduos absolutos versus valores ajustados --- avaliação da função de variância
dados %>% mutate(absdeviance = abs(rdeviance)) -> dados
dados %>% ggplot(aes(x = predito, y = absdeviance)) + 
  geom_point() + tema +
  scale_x_continuous(limits = c(0.8, 13), breaks = seq(0.8, 13, l = 5)) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, l = 5), labels = FF(seq(0, 3.5, l = 5), 2)) +
  labs(x = "Valores preditos", y = "|Deviance|")
ggsave(filename = 'var1.pdf', device = "pdf", width = 10, height = 6, dpi = 300)

## Half-Normal plot with simulated envelope
dados$fem <- relevel(dados$fem, "Women")
mod <- glm.nb(art ~ fem + mar + kid5 + ment, data = dados)
summary(mod)
my.hnp <- hnp(mod, resid.type = "deviance", plot.sim = F, how.many.out = T, print.on = T, paint.out = T)
plot(my.hnp)

pdf(file = "halfnormal1.pdf", width = 11, height = 7)
par(mar = c(3.2, 3.2, 1.5, 1.5), cex = 1.8)
plot(my.hnp$x, my.hnp$residuals, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', cex = 0.3, pch = 19,
     xlim = c(0, 3.5), ylim = c(0, 3.5))
lines(my.hnp$x, my.hnp$lower); lines(my.hnp$x, my.hnp$upper); lines(my.hnp$x, my.hnp$median)
mtext("Percentil da N(0, 1)", side = 1, line = 2.0, cex = 1.8)
mtext( expression(paste(sqrt("Resíduos da Deviance"))), side = 2, line =2, cex = 1.8)
abline(h=seq(0, 3.5, by = 0.5), v=seq(0, 3.5, by = 0.5), col = "gray", lty = "dotted")
axis(1, seq(0, 3.5, by = 0.5))
axis(2, seq(0, 3.5, by = 0.5))
text(x = 0.6, y = 3.1, labels = paste0("Total de pontos: ", my.hnp$total))
text(x = 1.05, y = 2.6, labels = paste0("Pontos fora do envelope: ", my.hnp$out, " (",
                                        FF(100 * my.hnp$out / my.hnp$total, 2), "%)"))
graphics.off()

## Alavancagem e influencia
dados %>% mutate(indice = 1:nrow(dados)) -> dados

### Indice versus elementos da matriz leverage
with(dados, plot(indice, leverage, type = 'h'))
dados %>% mutate(aux.leverage = ifelse(leverage <= 0.05, '1', '0')) -> dados 
dados %>% ggplot(aes(x = indice, y = leverage, col = factor(aux.leverage))) +
  geom_point(size = 2) + scale_color_manual(values = c("red","black")) +
  tema + theme(legend.position="none") +
  geom_text(data = dados[which(dados$aux.leverage == 0),], aes(x = indice, y = leverage + 0.0035, label = indice)) +
  scale_y_continuous(limits = c(0, 0.09), breaks = seq(0, 0.09, l = 5), labels = FF(seq(0, 0.09, l = 5),2)) +
  scale_x_continuous(limits = c(0, 920), breaks = seq(0, 920, l = 5)) +
  labs(x = "Índice", y = expression(h[ii]))
ggsave(filename = 'leverage1.pdf', device = "pdf", width = 10, height = 6, dpi = 300)

### Índice versus distância de cook
with(dados, plot(indice, cook, type = 'h'))
plot(mod, which = 4)
dados %>% mutate(aux.cook = ifelse(cook <= 0.03, '1', '0')) -> dados 
dados %>% ggplot(aes(x = indice, y = cook, col = factor(aux.cook))) +
  geom_point(size = 2) + scale_color_manual(values = c("red","black")) +
  tema + theme(legend.position="none") +
  geom_text(data = dados[which(dados$aux.cook == 0),], aes(x = indice, y = cook + 0.0018, label = indice), size = 3) +
  scale_y_continuous(limits = c(0, 0.051), breaks = seq(0, 0.051, l = 5), labels = FF(seq(0, 0.051, l = 5),2)) +
  scale_x_continuous(limits = c(0, 920), breaks = seq(0, 920, l = 5)) +
  labs(x = "Índice", y = "Distância de Cook")
ggsave(filename = 'cook1.pdf', device = "pdf", width = 10, height = 6, dpi = 300)


### Leverage versus resíduo studendizado de pearson/deviacen
with(dados, plot(leverage, sdpearson))


with(dados, plot(leverage, sdeviance))




#####################################################################################

dados$fem <- factor(dados$fem)
dados$mar <- factor(dados$mar)
dados$fem <- relevel(dados$fem, "Women")
#####################################################################################
mod <- aodml(art ~ fem + mar + kid5 + ment, data = dados, family = "nb")
summary(mod)
dados$xb2 <- dados$xb^2
mod <- aodml(art ~ fem + mar + kid5 + ment + xb2, data = dados, family = "nb")
summary(mod)

# The probablity of zero articles in the negative binomial.
mu.nb <- exp(predict(mod))
theta <- mod$theta
znb <- (theta / (mu.nb+theta)) ^ theta
mean(znb)
mean(dnbinom(0, mu = mu.nb, size = theta))












#####################################################################################
length(my.hnp$median)
df <- data.frame(x = my.hnp$x, residuals = my.hnp$residuals, lower = my.hnp$lower, upper = my.hnp$upper, 
                 median = my.hnp$median)

df %>% ggplot(aes(x = x, y = residuals)) + 
  geom_point(size = 0.8) + 
  geom_line(aes(x = x, y = lower), inherit.aes = F) + 
  geom_line(aes(x = x, y = upper)) +
  geom_line(aes(x = x, y = median)) +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by = 0.5)) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by = 0.5)) +
  tema + labs(x = "Percentil da N(0, 1)", y = expression(paste(sqrt("Resíduos da Deviance"))))
range(df$residuals)
ggsave(filename = 'halfnormal1.pdf', device = "pdf", width = 10, height = 6, dpi = 300)



