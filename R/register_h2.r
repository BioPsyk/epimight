# K1: lifetime prevalence general population
# kr: lifetime prevalence in the relatives of the affected ones
# A1: number of cases used to calculate K1
# Ar: number of cases used to calculate Kr

h2.calculation = function(K1,Kr,A1,Ar,ar=1/2)
{
	# h2 estimate
	T1   = qnorm(K1, lower.tail= FALSE) # lifetime prevalence unaffected/general population represeting the upper tail z value
	y    = dnorm(T1)
	i    = y/K1 
	Tr   = qnorm(Kr, lower.tail= FALSE) # lifetime prevalence in the relatives of the affected ones represeting the upper tail z value
	yr   = dnorm(Tr)

	num  = T1-Tr * sqrt(1 - (1 - T1/i) * (T1^2 -Tr^2))
	den  = ar * (i + (i-T1)*Tr^2)
	h2   = num/den
	# se estimation
	Wg   = (((K1^2)/(y^2)) * (1-K1)) / A1
	vvg  = (1/i - ar*h2*(i-T1))^2 # there is a + in Wray and a - in Falconer
	Wr   = Kr^2/yr^2 * (1-Kr) / Ar
	vvr  = (1/i)^2

	se   = 1/ar * sqrt(vvg * Wg + vvr * Wr)
	ci.l = h2 - 1.96 * se
	ci.u = h2 + 1.96 * se
	output = rbind(c(h2, se, ci.l, ci.u))
	colnames(output) = c("h2", "se", "L95", "U95")
	return(output)
}