
library(tikzDevice)

#S = 0.5
#M = -0.5 * S * S
xmax = 2


Svals = c(0.1, 0.5, 1.0)
Mvals = -0.5 * Svals * Svals
Scols = c("#c91f37", "#36e0c8", "#747474")


basewidth = 4
baseheight = basewidth / 1.618
filename = "rateparameterisation.tex"
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(filename, width = basewidth*2, height = baseheight*2, standAlone = TRUE,
    	packages = c("\\usepackage{tikz}",
	         "\\usepackage[active,tightpage,psfixbb]{preview}",
	         "\\PreviewEnvironment{pgfpicture}",
	         "\\setlength\\PreviewBorder{0pt}",
	         "\\usepackage{amssymb}"))



par(mfrow = c(2,2))
par(mar = c(5,5,5,5))
par(cex.lab = 1.5)
par(cex.main = 1.5)


# 1) pdf
x = seq(from = 0, to = xmax, by = 0.01)

plot(0, 0, type = "n", xaxs = "i", lwd = 3, yaxs = "i", axes = F, xlab = "Branch rate $r$", ylab = "$p(r|\\sigma)$",
main = "Probability density function (log-normal)", xlim = c(0, xmax), ylim=c(0, 5))



for (i in 1:length(Svals)){
	S = Svals[i]
	M = Mvals[i]
	polygon(c(0, x, xmax), c(0, dlnorm(x, M, S), 0), border = Scols[i], lwd = 3)

}


lines(c(1,1), c(0,5), lty="2828")
text(1, 5, "Mean=1", adj=c(0,1))

legend("topright", paste0("$\\sigma=", Svals, "$"), fill = Scols, bty="n")


axis(1)
axis(2, las=2)




# 2) real
plot(0, 1, type = "n", xaxs = "i", yaxs = "i", axes = F, xlab = "Abstraction $\\vec{\\mathcal{R}}^{\\,}$ [rate]", ylab = "Branch rate $r(\\vec{\\mathcal{R}}^{\\,})$",
main = "\\textit{real}", xlim =c(0, xmax), ylim=c(0, xmax))

lines(c(0, xmax), c(0, xmax), lwd = 3)
axis(1)
axis(2, at = seq(from = 0, to = xmax, by = 0.5), las=2)



# 3) categories
nbins = 10
plot(0, 1, type = "n",  yaxs = "i", axes = F, xlab = "Abstraction $\\vec{\\mathcal{R}}^{\\,}$ [category number]", ylab = "Branch rate $r(\\vec{\\mathcal{R}}^{\\,})$",
main = paste0("\\textit{cat} with ", nbins, " bins"), xlim =c(0, nbins-1), ylim=c(0, xmax))



b = 0:(nbins-1)


for (i in 1:length(b)) {
	lines(c(b[i], b[i]), c(0, xmax), lty = "1818", lwd = 0.5)
}


for (i in 1:length(Svals)){
	S = Svals[i]
	M = Mvals[i]
	y = qlnorm ((b+0.5) / nbins, M, S)
	points(b, y, pch = 16, col = Scols[i])
}


axis(1, at = 0:(nbins-1))
axis(2, at = seq(from = 0, to = xmax, by = 0.5), las=2)



# 4) quantiles
plot(0, 1, type = "n", yaxs = "i", axes = F, xlab = "Abstraction $\\vec{\\mathcal{R}}^{\\,}$ [quantile]", ylab = "Branch rate $r(\\vec{\\mathcal{R}}^{\\,})$",
main = paste0("\\textit{quant}"), xlim =c(0, 1), ylim=c(0, xmax))


for (i in 1:length(Svals)){
	S = Svals[i]
	M = Mvals[i]
	x = seq(from = 0, to = 1, by = 0.001)
	y = qlnorm (x, M, S)
	lines(x, y, pch = 16, col = Scols[i], lwd = 3)
}


axis(1, at = seq(from = 0, to = 1, by = 0.1))
axis(2, at = seq(from = 0, to = xmax, by = 0.5), las=2)

#setwd("/home/jdou557/Documents/Marsden2019/papers/ClockModelPaper/Figures")

dev.off()
tools::texi2pdf(filename)
