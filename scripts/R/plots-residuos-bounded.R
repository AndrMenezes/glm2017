rm(list = ls(all.names = TRUE))
packages <- c('simplexreg', 'hnp', 'ggplot2', 'dplyr')
sapply(packages, require, character.only = T)
setwd('C:/Users/User/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho')
source('C:/Users/User/Dropbox/4° Série/Modelos Lineares Generalizados/trabalho/scripts/rpp2-simplex.R')
FF   <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}
tema <- theme(text = element_text(size=20), panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(size = 1.2))

# Dados -------------------------------------------------------------------
dados  <- read.delim(file = 'idh_pt2010.txt')
sul    <- dados %>% filter(regiao == 'Sul')
parana <- dados %>% filter(ufn == 'Paraná' & pt2006 != 0)
head(parana)

# Preditores --------------------------------------------------------------
preditor1   <- pt2010 ~ pt2006 + gini + idhm_e + idhm_l + idhm_r + urbprop + pdes18m + pibperc
preditor2   <- pt2010 ~ pt2006 + gini + idhm_e + idhm_r + urbprop + pdes18m 
preditor3   <- pt2010 ~ pt2006 + pt2006 + gini + idhm_e + idhm_r + urbprop + pdes18m | pt2006

# Ajustes -----------------------------------------------------------------
mod.simp1   <- simplexreg(preditor1, link = 'logit', data = parana)
mod.simp2   <- simplexreg(preditor2, link = 'logit', data = parana)
mod.simp3   <- simplexreg(preditor3, link = 'logit', data = parana)

# Envelope ----------------------------------------------------------------
# d.fun <- function(obj) residuals(obj, type = 'appstdPerr')
d.fun <- function(obj) residuos.simplex(obj)$rpp
s.fun <- function(n, obj) {
  mu  <- obj$meanmu
  phi <- sqrt(obj$Dispersion)
  rsimplex(n, mu =  mu, sig = phi)
}
my.data <- parana
f.fun <- function(y.) simplexreg(y. ~ pt2006 + gini + idhm_e + idhm_r + urbprop + pdes18m,
                                 link = 'logit', data = my.data)

simp.hnp <- hnp(mod.simp2, newclass = TRUE, diagfun = d.fun, simfun = s.fun, fitfun = f.fun, halfnormal = F,
                how.many.out = T, paint.out = T, plot = F)
pdf(file = "hnp-simp2.pdf", width = 11, height = 7)


par(mar = c(3.5, 3.5, 1.2, 0.6), cex = 1.8)
plot(simp.hnp, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', cex = 0.6, ylim = c(-4, 4))
mtext("Percentil da N(0, 1)", side = 1, line = 2.0, cex = 1.8)
mtext("Resíduos padronizados ponderados", side = 2, line =2, cex = 1.8)
abline(h = seq(-4, 4, l = 5), v=seq(-3, 3, l = 5), col = "gray", lty = "dotted")
axis(1, seq(-3, 3, l = 5))
axis(2, seq(-4, 4, l = 5), FF(seq(-4, 4, l = 5), 1))
graphics.off()


# Predito versus observado ------------------------------------------------
preditos <- read.delim('pred.txt')
preditos %>% ggplot(aes(x = pt2010, y = simppred)) + 
  geom_point(size = 2) + geom_smooth(se = F, method = 'lm') +
  labs(x = 'Valores observados', y = 'Valores ajustados') + tema
ggsave(filename = 'pred-ajust.pdf', device = "pdf", width = 10, height = 7, dpi = 300)


# Criando data.frame para os gráficos -------------------------------------
res <- residuos.simplex(mod.simp2)
df  <- data.frame(id = 1:nrow(parana), etahat = mod.simp2$predict, muhat = mod.simp2$meanmu,
                  rpp = res$rpp, dcook = res$DC, leverage = res$ht)
head(df)

# Resíduos versus preditor linear -----------------------------------------
df %>% mutate(aux1 = ifelse(rpp  > 3.05 | rpp < -3.05,  '1', '0')) -> df 
df %>% ggplot(aes(x = etahat, y = rpp)) + 
  geom_point(size = 2) + 
  geom_text(data = df[which(df$aux1 == 1),], aes(x = etahat, y = rpp + 0.35, label = id)) +
  tema + scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, l = 5)) +
  theme(text = element_text(size=20)) +
  geom_hline(yintercept = c(-3, 3), col = "red", lty = 3, size = 1) +
  geom_hline(yintercept = 0, lty = 3, size = 1) +
  labs(x = expression(hat(eta)), y = "Resíduos padronizados ponderados") 
ggsave(filename = 'rsimplex.pdf', device = "pdf", width = 10, height = 7, dpi = 300)


# Distância de Cook -------------------------------------------------------
df %>% mutate(aux2 = ifelse(dcook <= 0.05, '1', '0')) -> df 
df %>% ggplot(aes(x = id, y = dcook, col = factor(aux2))) +
  geom_point(size = 2) + scale_color_manual(values = c("red","black")) +
  tema + theme(legend.position="none") +
  geom_text(data = df[which(df$aux2 == 0),], aes(x = id, y = dcook + 0.004, label = id), size = 3) +
  scale_y_continuous(limits = c(0, 0.075), breaks = seq(0, 0.07, l = 5), labels = FF(seq(0, 0.07, l = 5),2)) +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, l = 5)) +
  labs(x = "Índice das observações", y = "Distância de Cook")
ggsave(filename = 'cook2.pdf', device = "pdf", width = 10, height = 7, dpi = 300)


# Alavancagem -------------------------------------------------------------
df %>% mutate(aux.leverage = ifelse(leverage <= 0.05, '1', '0')) -> df
df %>% ggplot(aes(x = id, y = leverage, col = factor(aux.leverage))) +
  geom_point(size = 2) + scale_color_manual(values = c("red","black")) +
  tema + theme(legend.position="none") +
  geom_text(data = df[which(df$aux.leverage == 0),], aes(x = id, y = leverage + 0.0035, label = id)) +
  labs(x = "Índice das observações", y = expression(h[ii]))
ggsave(filename = 'leverage1.pdf', device = "pdf", width = 10, height = 6, dpi = 300)


# Análise de observações influentes ---------------------------------------
obs <- c(109, 135, 302)
parana[obs, ]








