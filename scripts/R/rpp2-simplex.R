# Resíduo padronizado ponderado 2 (Miyashiro, 2008) -----------------------
residuos.simplex <- function(mod)
{
  muhat   <- mod$meanmu
  phi2hat <- mod$Dispersion
  X       <- as.matrix(mod$x$mean)
  Y       <- as.matrix(mod$y)
  k       <- length(coef(mod))
  mEd2    <- (3*phi2hat)/(muhat * (1 - muhat)) + 1/(muhat^3 * (1 - muhat)^3)
  gdiff   <- -1 / (muhat * (muhat - 1))
  A       <- diag(mEd2 / gdiff^2)
  aux     <- solve(t(X) %*% A %*% X)
  H       <- sqrt(A) %*% X %*% aux %*% t(X) %*% sqrt(A) 
  ht      <- diag(H)
  qt      <- phi2hat * mEd2
  dhat    <- (Y -  muhat)^2 / (Y * (1 - Y) * muhat^2 * (1-muhat)^2)
  ut      <- ((Y - muhat)/(muhat * (1-muhat))) * (dhat + 1/(muhat^2 * (1-muhat)^2))
  rpp     <- ut / sqrt(qt * (1 - ht))
  DC      <- rpp^2 * ( ht / (k * (1 - ht)))
  return(list(rpp = rpp, DC = DC, ht = ht))
}

