
library(tikzDevice)




filename = "bactrian"
cols = 1
rows = 1
basewidth = 3.5



options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(paste0(filename, ".tex"), width = 1.61*basewidth*cols, height = basewidth*rows, standAlone = TRUE,
    	packages = c("\\usepackage{tikz}",
	         "\\usepackage[active,tightpage,psfixbb]{preview}",
	         "\\PreviewEnvironment{pgfpicture}",
	         "\\setlength\\PreviewBorder{0pt}",
	         "\\usepackage{amssymb}"))



par(mfrow = c(rows, cols), mar = c(5,5,5,2))
par(las=1)
par(cex.main = 1.3)
par(cex.lab = 1.3)



dbactrian = function(x, m){

	
	s = 1
	dens = 1 / (2*s*sqrt(2*pi*(1-m^2)))
	dens = dens * (exp( -(x + m*s)^2 / (2 * (1-m^2)) * s^2 ) + exp( -(x - m*s)^2 / (2 * (1-m^2)) * s^2 ))
	dens

}


x = seq(from = -3, to = 3, by = 0.001)


# Bactrian
plot(0, 1, xlim = range(x), ylim = c(0,1.1), type = "n", xlab = "$\\Sigma$", ylab = "Probability density function $p(\\Sigma|m)$", main = "Bactrian distribution", xaxs = "i", yaxs = "i", axes = F)


mvals = c(0, 0.95, 0.98)

cols = c("#008cba", "#8cba00", "#ba008c")
cols = c("#00000077", "#00658877", "#08c0ff77")
lwds = c(1,1,1)
ltys = c("solid", "dashed", "dotted")
ltys = c("solid", "solid", "solid")



for (i in 1:length(mvals)){
	m = mvals[i]
	col = cols[i]
	lwd = lwds[i]
	lty = ltys[i]
	polygon(x, dbactrian(x, m), col = col, lty = lty, lwd = lwd)
}

legend("topleft", paste0("$m=", mvals, "$", c(" (Normal)", "", "")), fill = cols,  box.lwd = 0)


axis(1)
axis(2, las = 2)





dev.off()




tools::texi2pdf(paste0(filename, ".tex"))









