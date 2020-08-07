library(graphics)
library(tikzDevice)
library(shape)
#library(plotly)

set.seed(1234566)


basewidth = 5
baseheight = basewidth 
filename = "bimodal.tex"
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(filename, width = basewidth*2, height = baseheight*1, standAlone = TRUE,
    	packages = c("\\usepackage{tikz}",
	         "\\usepackage[active,tightpage,psfixbb]{preview}",
	         "\\PreviewEnvironment{pgfpicture}",
	         "\\setlength\\PreviewBorder{0pt}",
	         "\\usepackage{amssymb}"))



par(mfrow = c(1,2))
par(mar = c(5,5,5,5))
par(cex.lab = 1.4)
par(cex.main = 1.4)




yule.rate = 1/5
sigma = 0.2


t.prior = function(t){
	dexp(t, yule.rate, log = T)
	#dlnorm(t, -0.5*sigma*sigma, sigma, log = T)
}

r.prior = function(r){
	dlnorm(r/5, -0.5*sigma*sigma, sigma, log = T)
}


prior = function(rate, len){
	t.prior(len) + r.prior(rate)
}


likelihood = function(seq1, seq2, rate, len){


	d = len*rate

	sum(log(ifelse(seq1 == seq2, 1/4 + 3/4*exp(-4*d/3), 1/4 - 1/4*exp(-4*d/3)))) 

}

xmax = 10
mu = 1 * 4/3
t = 0.1
grid.len = 500
dL = 20000
x = seq(from = 0.01, to = xmax, length = grid.len)
y = seq(from = 0.01, to = xmax, length = grid.len)

for (L in c(80, 90)){
	#plot(0, 0, xlim=c(0, 2), ylim = c(0, 1), type = "n", xlab="Branch rate $r$", ylab="Branch length $\\tau$", axes = F, main = paste("Branch likelihood when L =", L, "sites"))



	seq1 = sample(1:4, L, replace = T)
	nmut = L*mu*t
	mut.loc = sample(1:L, nmut)
	seq2 = seq1
	seq2[mut.loc] = sample(1:4, nmut,  replace = T)


	lnp = matrix(0, nrow = grid.len, ncol = grid.len)
	lnL = matrix(0, nrow = grid.len, ncol = grid.len)
	for (i in 1:length(x)){

		for (j in 1:length(y)){

			lnL[i,j] = likelihood(seq1, seq2, x[i], y[j])
			lnp[i,j] = prior(x[i], y[j])

		}

	}

	print(paste("The max likelihood is", max(lnL), " and the max prior is", max(lnp)))
	lnpos = lnL + lnp
	

	#contour(x=x, y=y, z=L, type = "contour", xlab="Branch rate $r$", ylab="Branch length $\\tau$",  main = paste("Branch likelihood when L =", L, "sites"))
	nbins = 20

	cols = rev(c(hcl.colors(nbins-1, palette="Terrain"), "white"))
	lnpos.vec = as.numeric(lnpos)
	breaks = rev(seq(from = max(lnpos.vec) + 10, by = -dL, length = nbins))
	#breaks = round(breaks / 10, 0) * 10
	breaks = c(min(lnpos.vec) - 10, breaks)
	breaks  =signif(breaks, 2)
	breaks = round(breaks / 10, 0) * 10
	#seq(from = min(lnL.vec), to = max(lnL.vec), length = nbins+1)
	image(x, y, xlim = c(0, xmax), ylim = c(0, xmax), lnpos, xlab="Branch rate $r$", ylab="Branch length $\\tau$", axes=F,  main = paste0("log-posterior when L = ", L/1000, "kb"), col = cols,
			  useRaster = TRUE, xaxs = "i", yaxs = "i")



	#par(new=TRUE)
	#contour(x, y, lnL, xlab="", ylab="", axes=F,  main = "", nlevels = nbins, levels = breaks, xaxs = "i", yaxs = "i", col = "black", lwd = 2, labcex = 1.0, xlim = c(0, xmax), ylim = c(0, xmax))

	axis(1, at = seq(from = 0, to = xmax, by = 0.5))
	axis(2, at = seq(from = 0, to = xmax, by = 0.5), las=2)
	
	#filled.contour(x, y, lnL, xlab="", ylab="", axes=F,  main = "", nlevels = nbins, levels = breaks, xaxs = "i", yaxs = "i", col = cols, lwd = 2)


	if (FALSE) {

		# Draw an arrow
		arrow.col = "#FF4500" #"black" #"#FF4500"
		font.col = arrow.col
		#points(0.3, 0.3, pch = 16, col = arrow.col)

		r0 = 0.5
		t0 = 0.32
		


		dr = 0.5

		# Randomwalk
		Arrows(r0, t0, r0 + dr, t0, lcol = arrow.col,  arr.type="curved")
		#text(r0 + dr + 0.05, t0, "$x^\\prime$", adj = c(0, 0.3), col = font.col)
		text(r0 + dr/2, t0 + 0.02, "\\texttt{RW}", adj = c(0.5, 0), cex = 1.2)



		# Constant distance
		dr = 0.3
		r1 = r0 - dr
		t1 = r0/r1 * t0
		#r1 = r0*t0/t1
		Arrows(r0, t0, r1, t1, lcol = arrow.col,  arr.type="curved")
		#text(r1, t1 + 0.05, "$x^\\prime$", adj = c(0.5, 0), col = font.col)
		text((r0 + r1)/2 - 0.01, (t0 + t1)/2, "\\texttt{CD}", adj = c(1, 0.5), cex = 1.2)

		points(r0, t0, pch = 21, col = "black", bg=arrow.col, cex = 2, lwd = 2)
		#text(r0, t0, "$x$", adj = c(0, 0), col = font.col)

	}		

}










dev.off()
tools::texi2pdf(filename)
