rm(list = ls(all.names = TRUE))
library(betareg)
library(lmtest)
library(car)
library(simplexreg)
library(tidyverse)
setwd('C:/Users/User/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho')
FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}

# Dados -------------------------------------------------------------------
dados  <- read.delim(file = 'idh_pt2010.txt')
sul    <- dados %>% filter(regiao == 'Sul')
parana <- dados %>% filter(ufn == 'Paraná' & pt2006 != 0)

# Preditores --------------------------------------------------------------
head(parana)
preditor1   <- pt2010 ~ pt2006 + gini + idhm_e + idhm_l + idhm_r + urbprop + pdes18m + pibperc
preditor2   <- pt2010 ~ pt2006 + gini + idhm_e + idhm_r + urbprop + pdes18m 
preditor3   <- pt2010 ~ pt2006 + pt2006 + gini + idhm_e + idhm_r + urbprop + pdes18m | pt2006


# Ajustes -----------------------------------------------------------------
mod.beta1    <- betareg(preditor1, data = parana, link = 'logit', link.phi = 'identity')
mod.beta2    <- betareg(preditor2, data = parana, link = 'logit', link.phi = 'identity')
mod.beta3    <- betareg(preditor3, data = parana, link = 'loglog', link.phi = 'identity')

mod.simp1    <- simplexreg(preditor1, data = parana, link = 'logit')
mod.simp2    <- simplexreg(preditor2, data = parana, link = 'logit')


# LR teste ----------------------------------------------------------------
lrtest(mod.beta2, mod.beta1)
AIC(mod.beta1, mod.beta2)
lrtest(mod.beta3, mod.beta2)
AIC(mod.beta3, mod.beta2)

lrtest(mod.simp2, mod.simp1)

# RESET teste -------------------------------------------------------------
lrtest(mod.beta2, . ~ . + I(predict(mod.beta2, type = "link")^2))
lrtest(mod.beta3, . ~ . + I(predict(mod.beta3, type = "link")^2))


# Vuong.test --------------------------------------------------------------
vuong.test <- function(y, mu1, mu2, phi1, phi2)
{
  n           <- length(y)
  llA         <- dbeta(y, mu1 * phi1, (1 - mu1) * phi1, log = T)
  llB         <- log(dsimplex(y, mu2, sqrt(phi2)))
  omega.hat.2 <- (n - 1) / n * var(llA - llB)
  lr          <- sum(llA - llB)
  teststat    <- (1/sqrt(n)) * lr/sqrt(omega.hat.2)
  list(TLR = teststat, p.value = pnorm(q = teststat, lower.tail = F))
}
muhat.beta <- predict(mod.beta1)
muhat.simp <- predict(mod.simp1)
phi.beta   <- mod.beta1$coefficients$precision
phi.simp   <- mod.simp1$Dispersion
vuong.test(y = parana$pt2010, mu1 = muhat.beta, mu2 = muhat.simp, phi1 = phi.beta, phi2 = phi.simp)





