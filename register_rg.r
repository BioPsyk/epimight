# Kc: lifetime prevalence SCZ in the general population
# Krc: lifetime prevalence of SCZ on those whose parents have CAD
# kf: lifetime prevalence CAD in the general population
# Ac: number of cases used to calculate Kc
# Arc: number of cases used to calculate Krc
# Af: number of cases used to calculate Kf
# h2c: heritability of SCZ (estimated before)
# h2f: heritability of CAD (estimated before)
# ar: coefficient of relationship (2pi). by default is 1/2 because you are working with parent-offsprint or sibs

rhog.calculation <- function(Kc, Krc, Kf, Ac, Arc, Af, h2c,h2f,ar=1/2)
{
  Tc <- qnorm(Kc, lower.tail= FALSE)
  yc <- dnorm(Tc)
  Trc <- qnorm(Krc, lower.tail= FALSE)
  yrc <- dnorm(Trc)
  Tf <- qnorm(Kf, lower.tail= FALSE)
  yf <- dnorm(Tf)
  
  i <- yf/Kf
  num <- Tc-Trc * sqrt(1 - (1 - Tf/i) * (Tc^2 -Trc^2))
  den <- ar * (i + (i-Tf)*Trc^2)
  rhh <- num/den
  rhog <- rhh/sqrt(h2f*h2c)
  # se estimation
  Wg <- Kf^2/yf^2 * (1-Kf) / Af
  vvg <- (1/i - ar*rhh*(i-Tf))^2 # there is a + in Wray and a - in Falconer
  Wr <- Krc^2/yrc^2 *(1-Krc) / Arc + Kc^2/yc^2 * (1-Kc) / Ac
  vvr <- (1/i)^2
  se <- 1/ar * sqrt(vvg * Wg + vvr * Wr)
  ci.l <- rhh - 1.96 * se
  ci.u <- rhh + 1.96 * se
  ci.l.r <- ci.l/sqrt(h2f*h2c)
  ci.u.r <- ci.u/sqrt(h2f*h2c)
  result = c(rhh, rhog, se, ci.l, ci.u, ci.l.r, ci.u.r)
  names(result) = c("rhh","rhog", "SE","U95","L95","U95_h2","L95_h2")
  return(result)
}
