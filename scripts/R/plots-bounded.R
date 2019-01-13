rm(list = ls(all.names = TRUE))
packages <- c('betareg', 'simplexreg', 'hnp', 'tidyverse', 'fitdistrplus')
sapply(packages, require, character.only = T)
source('C:/Users/User/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho/scripts/rpp2-simplex.R')
setwd('C:/Users/User/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho')
FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}

# Dados -------------------------------------------------------------------
dados  <- read.delim(file = 'idh_pt2010.txt')
sul    <- dados %>% filter(regiao == 'Sul')
parana <- dados %>% filter(ufn == 'Paraná' & pt2006 != 0)
head(parana)

# Descritiva --------------------------------------------------------------
x        <- parana$pt2010
fit.beta <- coef(fitdist(x, 'beta')) 
fit.simp <- coef(fitdist(x, 'simplex', start = list(mu = 0.2, sig = 1)))
vx       <- seq(0.15, 0.85, l = 10000)
pdf("hist.pdf", width = 9, height = 6)
par(mar = c(3.2, 3.2, 1.5, 1.5), cex = 1.5) # mar = c(3.2, 3.2, 1.5, 1.5), cex = 1.8
hist(x, probability = T, main = '', xaxt = 'n', yaxt = 'n', breaks = 20, xlim = c(0.16, 0.83), ylim = c(0, 3.8), xlab = '', ylab = '')
lines(vx, dbeta(x = vx, shape1 = fit.beta[1], shape2 = fit.beta[2]), lwd = 2, col = 'blue')
lines(vx, dsimplex(x = vx, mu = fit.simp[1], sig = fit.simp[2]), lwd = 2, col = 'red')
legend('topright', c('Beta', 'Simplex'), col = c('blue', 'red'), lwd = 2, inset = 0.02, bty = 'n')
axis(side = 1, at = seq(0.16, 0.83, l = 5), labels = FF(seq(0.16, 0.83, l = 5), 2))
axis(side = 2, at = seq(0, 3.8, l = 5), labels = FF(seq(0, 3.8, l = 5), 2)); box()
graphics.off()

# Preditores --------------------------------------------------------------
preditor1   <- pt2010 ~ pt2006 + gini + idhm_e + idhm_l + idhm_r + urbprop + pdes18m + pibperc
preditor2   <- pt2010 ~ pt2006 + gini + idhm_e + idhm_r + urbprop + pdes18m 

# Ajustes -----------------------------------------------------------------
mod.beta     <- betareg(preditor1, data = parana, link = 'logit', link.phi = 'identity')
mod.simp     <- simplexreg(preditor1, data = parana, link = 'logit')
mod.beta2    <- betareg(preditor2, data = parana, link = 'logit', link.phi = 'identity')
mod.simp2    <- simplexreg(preditor2, data = parana, link = 'logit')

# hnp Beta ----------------------------------------------------------------

# diagfun
d.fun <- function(obj) residuals(obj, type = 'sweighted2')

# simfun
s.fun <- function(n, obj) {
  mu  <- obj$fitted.values
  phi <- obj$coefficients$precision
  rbeta(n, shape1 = mu * phi, shape2 = (1 - mu) * phi)
}

# fitfun
my.data  <- parana
f.fun <- function(y.) betareg(y. ~ pt2006 + gini + idhm_e + idhm_l + idhm_r + urbprop + pdes18m + pibperc,
                              link = 'logit', link.phi = 'identity', data = my.data)

# hnp call
set.seed(1502)
beta.hnp <- hnp(mod.beta, newclass = TRUE, diagfun = d.fun, simfun = s.fun, fitfun = f.fun, halfnormal = F,
                how.many.out = T, paint.out = T, plot = F)
pdf(file = "hnp-beta1.pdf", width = 10, height = 7)
par(mar = c(3.5, 3.5, 1.2, 0.6), cex = 1.8)
plot(beta.hnp, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', cex = 0.6, ylim = c(-4, 4))
mtext("Percentil da N(0, 1)", side = 1, line = 2.0, cex = 1.8)
mtext("Resíduos padronizados ponderados", side = 2, line =2, cex = 1.8)
abline(h = seq(-4, 4, l = 5), v=seq(-3, 3, l = 5), col = "gray", lty = "dotted")
axis(1, seq(-3, 3, l = 5))
axis(2, seq(-4, 4, l = 5), FF(seq(-4, 4, l = 5), 1))
text(x = -0.6, y = 3, labels =  paste0("Total de pontos: ", beta.hnp$total, '\n',
                                         "Pontos fora do envelope: ", beta.hnp$out, " (",
                                         FF(100 * beta.hnp$out / beta.hnp$total, 2), "%)"))
title('Beta', cex = 1.5)
graphics.off()

# hnp Simplex -------------------------------------------------------------

# diagfun
# d.fun <- function(obj) residuals(obj, type = 'appstdPerr')
d.fun <- function(obj) residuos.simplex(obj)$rpp

# simfun
s.fun <- function(n, obj) {
  mu  <- obj$meanmu
  phi <- sqrt(obj$Dispersion)
  rsimplex(n, mu =  mu, sig = phi)
}

# fitfun
my.data  <- parana
f.fun <- function(y.) simplexreg(y. ~ pt2006 + gini + idhm_e + idhm_l + idhm_r + urbprop + pdes18m + pibperc,
                                 link = 'logit', data = my.data)

# hnp call
set.seed(1502)
simp.hnp <- hnp(mod.simp, newclass = TRUE, diagfun = d.fun, simfun = s.fun, fitfun = f.fun, halfnormal = F,
                how.many.out = T, paint.out = T, plot = F)
pdf(file = "hnp-simp1.pdf", width = 10, height = 7)
par(mar = c(3.5, 3.5, 1.2, 0.6), cex = 1.8)
plot(simp.hnp, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', cex = 0.6, ylim = c(-4, 4))
mtext("Percentil da N(0, 1)", side = 1, line = 2.0, cex = 1.8)
mtext("Resíduos padronizados ponderados", side = 2, line =2, cex = 1.8)
abline(h = seq(-4, 4, l = 5), v=seq(-3, 3, l = 5), col = "gray", lty = "dotted")
axis(1, seq(-3, 3, l = 5))
axis(2, seq(-4, 4, l = 5), FF(seq(-4, 4, l = 5), 1))
text(x = -0.6, y = 3, labels = paste0("Total de pontos: ", simp.hnp$total, '\n',
                                        "Pontos fora do envelope: ", simp.hnp$out, " (",
                                        FF(100 * simp.hnp$out / simp.hnp$total, 2), "%)"))
title('Simplex', cex = 1.5)
graphics.off()


# Preditos versus Observados ----------------------------------------------
preditos <- read.delim('pred.txt')
head(preditos)

pdf(file = "pred-beta1.pdf", width = 9, height = 6)
par(mar = c(3.5, 3.5, 1.2, 1.2), cex = 1.6)
with(preditos, plot(betalower ~ pt2010, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', cex = 0.8))
with(preditos, abline(lsfit(x = pt2010, y = betalower), lty = 2, lwd = 2))
