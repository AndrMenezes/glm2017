rm(list = ls())
library(foreign)
library(MASS)
library(rootSolve)
setwd("C:/Users/User/Dropbox/4? S?rie/Modelos Lineares Generalizados/trabalho")
#####################################################################################
dados <- read.dta("couart2.dta")
head(dados)
dados$fem <- factor(dados$fem)
dados$mar <- factor(dados$mar)
dados$fem <- relevel(dados$fem, "Women")
#####################################################################################
mod <- glm.nb(art ~ fem + mar + kid5 + ment, data = dados)
summary(mod)
logLik(mod)
phiSAS    <- 0.4417
sd.phiSAS <- 0.0530
llSAS     <- -1561.0481
betas     <- coef(mod)
y         <- dados$art
X         <- model.matrix( ~ fem + mar + kid5 + ment, data  = dados)
#####################################################################################
ll <- function(beta, phi, y, X)
{
  mui  <- exp(X %*% beta)
  sum(log(dnbinom(x = y, size = 1 / phi, mu = mui)))
}
llperf <- function(phi)
{
  optim(par = betas, fn = ll, phi = phi, y = y, X = X,  control = list(fnscale = -1))$value
}
dev <- function(llres)
{
  2 * (llSAS - llres)
}
icperf <- function(alpha)
{
  qchi       <- qchisq(1 - alpha, df = 1)
  fperf      <- approxfun(phis, dev.perf - qchi)
  ic         <- uniroot.all(fperf, c(0, 10)) 
  devic      <- c(dev(ll(beta = betas, phi = ic[1], y = y, X = X)),
                  dev(ll(beta = betas, phi = ic[2], y = y, X = X)))
  list(ic, devic)
}
######################################################################################
phis       <- sort(c(seq(0.2, 0.8, l = 30), phiSAS))
perfil     <- sapply(phis, llperf)
dev.perf   <- dev(perfil)
icperf90   <- icperf(0.10)
icperf95   <- icperf(0.05)
icperf99   <- icperf(0.01)
dev.df     <- data.frame(phis, dev.perf)
ind        <- which(dev.df$phis == phiSAS)
#######################################################################################
pdf(file = "profile-bn.pdf", width = 8, height = 6)
par(mar = c(3.2, 3.2, 1.5, 1.5), cex = 1.8)
plot(phis, dev.perf, type = "l", xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', lwd = 2,
     xlim = c(0.2, 0.8), ylim = c(0, 30))
mtext(expression(phi), side = 1, line = 2.0, cex = 1.8)
mtext("Deviance", side = 2.1, line =2.2, cex = 1.8)
abline(h=seq(0, 30, l = 5), v=seq(0.2, 0.8, l = 5), col = "gray", lty = "dotted")
axis(1, seq(0.2, 0.8, l = 5))
axis(2, seq(0, 30, l = 5))
abline(v = phiSAS, col = "grey", lty = 2, lwd = 2)
points(x = phiSAS, y = dev.perf[ind], pch = 15)
arrows(x0 = icperf90[[1]], y0 = icperf90[[2]], x1 = icperf90[[1]], y1 = 0, lty = 2, col = "gray", lwd = 2, length = 0.12)
segments(x0 = icperf90[[1]][1], y0 = icperf90[[2]][1], x1 = icperf90[[1]][2], y1 = icperf90[[2]][2], lty = 2, col = "gray", lwd = 2)
arrows(x0 = icperf95[[1]], y0 = icperf95[[2]], x1 = icperf95[[1]], y1 = 0, lty = 2, col = "gray", lwd = 2, length = 0.12)
segments(x0 = icperf95[[1]][1], y0 = icperf95[[2]][1], x1 = icperf95[[1]][2], y1 = icperf95[[2]][2], lty = 2, col = "gray", lwd = 2)
arrows(x0 = icperf99[[1]], y0 = icperf99[[2]], x1 = icperf99[[1]], y1 = 0, lty = 2, col = "gray", lwd = 2, length = 0.12)
segments(x0 = icperf99[[1]][1], y0 = icperf99[[2]][1], x1 = icperf99[[1]][2], y1 = icperf99[[2]][2], lty = 2, col = "gray", lwd = 2)
text(x = icperf90[[1]][1] + 0.05, y = icperf90[[2]][1] + 0.5, labels = "90%", cex = 0.8)
text(x = icperf90[[1]][1] + 0.05, y = icperf95[[2]][1] + 0.5, labels = "95%", cex = 0.8)
text(x = icperf90[[1]][1] + 0.05, y = icperf99[[2]][1] + 0.5, labels = "99%", cex = 0.8)
graphics.off()

plot(x = teo, y = emp,  type = 'p', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', xlim = c(0, 1), ylim = c(0,1), col = 1, bty = 'l');
box();abline(0, 1, lwd = 1);
abline(h=seq(0, 1, by = 0.25), v=seq(0, 1, by = 0.25), col = "lightgray", lty = "dotted")
axis(1, seq(0, 1, by = 0.25),FF(seq(0, 1, by = 0.25), 2))
axis(2, seq(0, 1, by = 0.25),FF(seq(0, 1, by = 0.25), 2))
mtext("Theoretical probabilities", side = 1, line = 2.0, cex = 1.8)
mtext("Empirical probabilities", side = 2.1, line =2.2, cex = 1.8)
