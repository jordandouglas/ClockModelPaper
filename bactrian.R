
library(tikzDevice)



setwd("/home/jdou557/Documents/Marsden2019/papers/ClockModelPaper/Figures")


filename = "bactrian"
cols = 1
rows = 1
basewidth = 2.5



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



par(mfrow = c(rows, cols), mar = c(4,1,2,1))
par(las=1)
#par(cex = 1.0)



dbactrian = function(x, m){

	
	s = 1
	dens = 1 / (2*s*sqrt(2*pi*(1-m^2)))
	dens = dens * (exp( -(x + m*s)^2 / (2 * (1-m^2)) * s^2 ) + exp( -(x - m*s)^2 / (2 * (1-m^2)) * s^2 ))
	dens
}


x = seq(from = -3, to = 3, by = 0.001)
plot(0, 1, xlim = range(x), ylim = c(0,1.1), type = "n", xlab = "$\\alpha$", ylab = "", main = "Bactrian distribution", xaxs = "i", yaxs = "i", axes = F)


mvals = c(0.92, 0.95, 0.98)

cols = c("#008cba", "#8cba00", "#ba008c")
cols = c("#001922", "#006588", "#08c0ff")
lwds = c(2,2,2)
ltys = c("solid", "dashed", "dotted")
ltys = c("solid", "solid", "solid")



for (i in 1:length(mvals)){
	m = mvals[i]
	col = cols[i]
	lwd = lwds[i]
	lty = ltys[i]
	lines(x, dbactrian(x, m), col = col, lty = lty, lwd = lwd)
}

legend("topleft", paste0("$m=", mvals, "$"), col = cols, lwd = lwds, lty = ltys, box.lwd = 0)


axis(1)






dev.off()




tools::texi2pdf(paste0(filename, ".tex"))
system(paste(getOption("pdfviewer"), paste0(filename, ".pdf")))








