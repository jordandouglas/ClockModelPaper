
library(tikzDevice)

S = 0.3
M = -0.5 * S * S
xmax = 2.5


setwd("/home/jdou557/Documents/Marsden2019/Months/January2020/ClockModelPaper/Figures")


par(mfrow = c(2,3))




# Categories pdf
ypts = dlnorm(x, M, S)
plot(x, ypts , ylim = c(0, max(ypts)*1.2), type = "n", xaxs = "i", yaxs = "i", axes = F, xlab = expression("r"[i]), ylab = "Density",
main = "cats pdf (10 bins)")

nbins = 10
cats = 0:(nbins-1)
bin.cols = "#d3d3d3"
for (i in cats){
	

	x0 = qlnorm((i)/nbins, M, S)
	x1 = qlnorm((i+1)/nbins, M, S)
	if (i == nbins-1) {
		x1 = xmax
	}
	xmid = qlnorm((i+0.5)/nbins, M, S)
	y = dlnorm(xmid, M, S)

	rect(x0, 0, x1, y, col = bin.cols, border = "white", lwd = 2) 
	points(xmid, y, pch = 16, col = cols, cex = 1.5)


	text(xmid, y + 0.08, i)


}

lines(x, ypts , col = cols, lwd = 3)
axis(1)



# Rates pdf

x = seq(from = 0, to = xmax, by = 0.01)

cols = "#008cba"
plot(x, dlnorm(x, M, S), type = "l", xaxs = "i", col = cols, lwd = 3, yaxs = "i", axes = F, xlab = expression("r"[i]), ylab = "Density",
main = "real pdf")
axis(1)



# Quantiles pdf
nquant = 10
qrates = numeric(0)
for (i in 1:(nquant)){


	if (i == 1) {
		qrates[i] = qlnorm(0.1 / (nquant -1), M, S)
	}else if (i < nquant) {
		qrates[i] = qlnorm((i-1) / (nquant -1), M, S)
	}else{
		qrates[i] = qlnorm((nquant - 1 - 0.1) / (nquant - 1), M, S)
	}

}



quantToRate = function(q) {

	v = q * (nquant-1)
        i = floor(v)
	index = i + 1
	r = qrates[index]
	if (index < nquant) {
      		r = r + (qrates[index+1] - qrates[index]) * (v - i);
      	}
	r

}

plot.new()


# Categories cdf
rates = qlnorm((cats+0.5)/nbins, M, S)
ypts = plnorm(rates, M, S)
plot(cats, ypts , type="n", xlab = "ci", ylab = "ri", yaxs = "i", main = "cats cdf (10 bins)", axes = F, ylim = c(0, 1))


for (i in 1:length(cats) ){

	yi = plnorm( rates[i], M, S)
	lines(c(cats[i], cats[i]), c(0, yi ), col = bin.cols, lwd = 2)

}
points(cats, plnorm(rates, M, S), pch = 16, col = cols, cex = 1.5)
axis(1, at = cats)
axis(2, at = seq(from = 0, to = xmax, by = 0.5))




# Rates cdf
ypts = plnorm(x, M, S)
plot(x, ypts , type="l", xlab = "ri", ylab = "pi", yaxs = "i", xaxs = "i", col = cols, lwd = 3, main = "rates cdf", axes = F, ylim = c(0, 1))
axis(1)
axis(2)



# Quantiles cdf

ypts = plnorm(x, M, S)
plot(x, ypts , type="l", xlab = "ri", ylab = "pi", yaxs = "i", xaxs = "i", col = cols, lwd = 3, main = "quant cdf (100 pieces)", axes = F, ylim = c(0, 1))
axis(1)
axis(2)


yquant = seq(from = 0.01, to = 0.999, by = 0.01)
xquant = unlist(sapply(yquant, quantToRate))
lines(xquant, yquant, col = "black", lwd = 2, lty = "2121")





################







filename = "rateparameterisation"
cols = 2
rows = 2
basewidth = 2*1.61



options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(paste0(filename, ".tex"), width = basewidth*cols, height = basewidth*0.8*rows, standAlone = TRUE,
    	packages = c("\\usepackage{tikz}",
	         "\\usepackage[active,tightpage,psfixbb]{preview}",
	         "\\PreviewEnvironment{pgfpicture}",
	         "\\setlength\\PreviewBorder{0pt}",
	         "\\usepackage{amssymb}"))



par(mfrow = c(rows, cols))
par(las=1)
#par(cex = 1.0)



cat.col = "#008cba"

# Categories pdf
ypts = dlnorm(x, M, S)
plot(x, ypts , ylim = c(0, max(ypts)*1.2), type = "n", axes = F, xlab = "$r_i$", ylab = "$f(r_i|\\sigma)$", main = "Clock model pdf", cex.lab = 1.5, cex.main = 1.4)
mtext("(\\textit{cat} with n=10 bins)")

nbins = 10
cats = 0:(nbins-1)
bin.cols = "#d3d3d3"
for (i in cats){
	

	x0 = qlnorm((i)/nbins, M, S)
	x1 = qlnorm((i+1)/nbins, M, S)
	if (i == nbins-1) {
		x1 = xmax
	}
	xmid = qlnorm((i+0.5)/nbins, M, S)
	y = dlnorm(xmid, M, S)

	rect(x0, 0, x1, y, col = bin.cols, border = "white", lwd = 2) 
	points(xmid, y, pch = 16, col = cat.col, cex = 1)


	text(xmid, y + 0.14, i, cex = 1.1)


}

lines(x, ypts , col = cat.col, lwd = 3)
axis(1)







# Cat cdf
ypts = plnorm(x, M, S)
plot(x, ypts , type="l", xlab = "$r_i$", ylab = "$F(r_i|\\sigma)$", col = cat.col, lwd = 3, main = "Clock model cdf", axes = F, ylim = c(0, 1), las = 3, cex.lab = 1.5, cex.main = 1.4)
mtext("(\\textit{cat} with n=10 bins)")
axis(1)
axis(2)

#legend("bottomright", c("\\textit{cat} (10 bins)", "\\textit{quant} (100 pieces)"), col = c(cat.col, "black"), lwd = c(0, 2), lty = c("1111", "2121"), pch = c(16, NA))





for (i in cats){
	

	xmid = qlnorm((i+0.5)/nbins, M, S)
	y = (i+0.5)/nbins

	points(xmid, y, pch = 16, col = cat.col, cex = 1)


	yshift = ifelse(i == cats[length(cats)] | i == cats[length(cats)-1], -0.06, 0.06)
	text(xmid, y + yshift, i, cex = 1.1)


}






# Quantiles pdf
nquant = 10
qrates = numeric(0)
for (i in 1:(nquant)){


	if (i == 1) {
		qrates[i] = qlnorm(0.1 / (nquant -1), M, S)
	}else if (i < nquant) {
		qrates[i] = qlnorm((i-1) / (nquant -1), M, S)
	}else{
		qrates[i] = qlnorm((nquant - 1 - 0.1) / (nquant - 1), M, S)
	}

}




# quant cdf
ypts = plnorm(x, M, S)
plot(x, ypts , type="l", xlab = "$r_i$", ylab = "$F(r_i|\\sigma)$", col = cat.col, lwd = 3, main = "Clock model cdf", axes = F, ylim = c(0, 1), las = 3, cex.lab = 1.5, cex.main = 1.4)
mtext("(\\textit{quant} with n=10 pieces)")
axis(1)
axis(2)

#legend("bottomright", c("\\textit{cat} (10 bins)", "\\textit{quant} (100 pieces)"), col = c(cat.col, "black"), lwd = c(0, 2), lty = c("1111", "2121"), pch = c(16, NA))




yquant = seq(from = 0.001, to = 0.9999, by = 0.001)
xquant = unlist(sapply(yquant, quantToRate))
lines(xquant, yquant, col = "black", lwd = 2, lty = "2424")


ybits = ((1:nquant)-1) / (nquant - 1)
xbits = unlist(sapply(ybits, quantToRate))
points(xbits, ybits, pch = 16, col = "black")





rmin_y = 0.09 / (nquant-1)
rmin_x = quantToRate(rmin_y)
lines(c(rmin_x, rmin_x), c(0, 1), lty = "1313")
text(rmin_x, 0.8, "$r_{min}$", adj = 0, cex = 1.2)

rmax_y = (nquant-1-0.01) / (nquant-1)
rmax_x = quantToRate(rmax_y)
lines(c(rmax_x, rmax_x), c(0, 1), lty = "1313")
text(rmax_x, 0.8, "$r_{max}$", adj = 1, cex = 1.2)

legend("bottomright", c("$F(r_i|\\sigma)$", "$\\hat{F}(r_i|\\sigma)$"), col = c(cat.col, "black"), lwd = c(5, 2), lty = c("1111", "2424"), bg = "white")

# Quantiles pdf
nquant = 100
qrates = numeric(0)
for (i in 1:(nquant)){


	if (i == 1) {
		qrates[i] = qlnorm(0.1 / (nquant -1), M, S)
	}else if (i < nquant) {
		qrates[i] = qlnorm((i-1) / (nquant -1), M, S)
	}else{
		qrates[i] = qlnorm((nquant - 1 - 0.1) / (nquant - 1), M, S)
	}

}




# quant cdf
ypts = plnorm(x, M, S)
plot(x, ypts , type="l", xlab = "$r_i$", ylab = "$F(r_i|\\sigma)$", col = cat.col, lwd = 5, main = "Clock model cdf", axes = F, ylim = c(0, 1), las = 3, cex.lab = 1.5, cex.main = 1.4)
mtext("(\\textit{quant} with n=100 pieces)")
axis(1)
axis(2)



yquant = seq(from = 0.001, to = 0.9999, by = 0.001)
xquant = unlist(sapply(yquant, quantToRate))
lines(xquant, yquant, col = "black", lwd = 2, lty = "2424")


rmin_y = 0.09 / (nquant-1)
rmin_x = quantToRate(rmin_y)
lines(c(rmin_x, rmin_x), c(0, 1), lty = "1313")
text(rmin_x, 0.8, "$r_{min}$", adj = 0, cex = 1.2)

rmax_y = (nquant-1-0.09) / (nquant-1)
rmax_x = quantToRate(rmax_y)
lines(c(rmax_x, rmax_x), c(0, 1), lty = "1313")
text(rmax_x, 0.8, "$r_{max}$", adj = 1, cex = 1.2)


legend("bottomright", c("$F(r_i|\\sigma)$", "$\\hat{F}(r_i|\\sigma)$"), col = c(cat.col, "black"), lwd = c(5, 2), lty = c("1111", "2424"), bg = "white")



dev.off()




tools::texi2pdf(paste0(filename, ".tex"))
system(paste(getOption("pdfviewer"), paste0(filename, ".pdf")))








