
library(tikzDevice)



basewidth = 7
baseheight = basewidth * 0.618
filename = "clockSD.tex"
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(filename, width = basewidth, height = baseheight*1, standAlone = TRUE,
    	packages = c("\\usepackage{tikz}",
	         "\\usepackage[active,tightpage,psfixbb]{preview}",
	         "\\PreviewEnvironment{pgfpicture}",
	         "\\setlength\\PreviewBorder{0pt}",
	         "\\usepackage{amssymb}"))



par(mfrow = c(1,1))
par(mar = c(5,5,5,5))
par(cex.lab = 1.4)
par(cex.main = 1.4)



sigma = 0.4
mu = -0.5*sigma^2
sigma2 = 0.8
mu2 = -0.5*sigma2^2

r = seq(from = 0.001, to = 2, length = 500)
q1 = plnorm(r, mu, sigma)
q2 = plnorm(r, mu2, sigma2)


# $\\vec{\\mathcal{R}}$
plot(r, q1, type = "l", xlab = "Rate", ylab = "Quantile", main = "$\\sigma$ scale operators", xaxs = "i", yaxs = "i", axes = F, lwd=2, xlim = c(0, 2), ylim = c(0,1))
points(r, q2, type = "l", col = "red", lwd=2)

legend("topleft", c(paste0("$\\sigma=", sigma, "$"), paste0("$\\sigma^\\prime=", sigma2, "$")), col = c("black", "red"), lwd = 2, bty = "n")


# Scale
s_r0 = 0.7
s_q0 = plnorm(s_r0, mu, sigma)
s_q1 = plnorm(s_r0, mu2, sigma2)
lines(c(s_r0, s_r0), c(0, max(s_q0, s_q1)), lwd = 2, col = "#696969", lty = "1515")

lines(c(0, s_r0), c(s_q0, s_q0), lty = "1515", col = "black", lwd = 2)
lines(c(0, s_r0), c(s_q1, s_q1), lty = "1515", col = "red", lwd = 2)
points(c(s_r0, s_r0), c(s_q0, s_q1), pch = 16, col = c("black", "red"))

text(s_r0/2, max(s_q0, s_q1) + 0.01, "New quantiles", adj = c(0.5, 0))



# Cis scale
c_r0 = 1.4
c_q0 = plnorm(c_r0, mu, sigma)
c_r1 = qlnorm(c_q0, mu2, sigma2)
lines(c(0, max(c_r0, c_r1)), c(c_q0, c_q0), lwd = 2, col = "#696969", lty = "1515")

lines(c(c_r0, c_r0), c(0, c_q0), lty = "1515", col = "black", lwd = 2)
lines(c(c_r1, c_r1), c(0, c_q0), lty = "1515", col = "red", lwd = 2)
points(c(c_r0, c_r1), c(c_q0, c_q0), pch = 16, col = c("black", "red"))

text(max(c_r0, c_r1)/2, c_q0 + 0.01, "New rates", adj = c(0.5, 0))




axis(1, at = c(0, 1, 2, s_r0, c_r0, c_r1), labels = c("0.0", "1.0", "2.0", "$r=r^\\prime$", "$r$", "$r^\\prime$")) 
axis(2, at = c(0,  1, s_q0, s_q1, c_q0), labels = c("0.0", "1.0", "$q$", "$q^\\prime$", "$q = q^\\prime$" ) ,las=2)






dev.off()
tools::texi2pdf(filename)








