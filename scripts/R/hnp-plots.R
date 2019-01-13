rm(list = ls())
setwd("C:/Users/User/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho")
library(hnp)
library(MASS)
library(foreign)
library(pscl)
FF        <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}
dados     <- read.dta("couart2.dta")
dados$fem <- relevel(dados$fem, "Women")


# hnp function ------------------------------------------------------------
hnp.plot <- function(myhnp, dist, auxdist)
{
  pdf(file = paste0("hnp-", auxdist, ".pdf"), width = 11, height = 7)
  par(mar = c(3.2, 3.2, 1.5, 1.5), cex = 1.8)
  plot(myhnp$x, myhnp$residuals, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', cex = 0.3, pch = 19,
       xlim = c(0, 3.5), ylim = c(0, 8.5))
  lines(myhnp$x, myhnp$lower); lines(myhnp$x, myhnp$upper); lines(myhnp$x, myhnp$median)
  mtext("Percentil da N(0, 1)", side = 1, line = 2.0, cex = 1.8)
  mtext(expression(paste(sqrt("Resíduos de Pearson"))), side = 2, line =2, cex = 1.8)
  abline(h=seq(0, 8, by = 1), v=seq(0, 3.5, by = 0.5), col = "gray", lty = "dotted")
  axis(1, seq(0, 3.5, by = 0.5))
  axis(2, seq(0, 8, by = 1))
  text(x = 0.6, y = 8.2, labels = paste0(" Total de pontos: ", myhnp$total))
  text(x = 1.05, y = 7.2, labels = paste0("Pontos fora do envelope: ", myhnp$out, " (",
                                          FF(100 * myhnp$out / myhnp$total, 2), "%)"))
  mtext(dist, cex = 1.5, adj = 0)
  graphics.off()
}

# Poisson -----------------------------------------------------------------
mod.p <- glm(art ~ fem + mar + kid5 + ment, data = dados, family = poisson(link = "log"))
hnp.p <- hnp(mod.p, resid.type = "pearson", plot.sim = F, how.many.out = T, print.on = T, paint.out = T)
hnp.plot(myhnp = hnp.p, dist = "Poisson", auxdist = 'poi')

# Negative Binomial -------------------------------------------------------
mod.nb   <- glm.nb(art ~ fem + mar + kid5 + ment, data = dados, link = 'log')
hnp.nb   <- hnp(mod.nb, resid.type = "pearson", plot.sim = F, how.many.out = T, print.on = T, paint.out = T)
hnp.plot(myhnp = hnp.nb, dist = "Binomial Negativa", auxdist = 'bn')

# Zero-Inflated Poisson ---------------------------------------------------
mod.zip  <- zeroinfl(art ~ fem + mar + kid5 + ment, data = dados, dist = 'poisson')
hnp.zip  <- hnp(mod.zip, resid.type = "pearson", plot.sim = F, how.many.out = T, print.on = T, paint.out = T)
hnp.plot(myhnp = hnp.zip, dist = "Poisson Inflacionada de Zero", auxdist = "zip")

# Zero Inflated Negative Binomial -----------------------------------------
mod.zinb <- zeroinfl(art ~ fem + mar + kid5 + ment, data = dados, dist = 'negbin')
hnp.zinb <- hnp(mod.zinb, resid.type = "pearson", plot.sim = F, how.many.out = T, print.on = T, paint.out = T)
hnp.plot(myhnp = hnp.zip, dist = "Binomial Negativa Inflacionada de Zero", auxdist = "zinb")
