
library(tikzDevice)
library(HDInterval)



parameters = c("TreeHeight", "birthRate", "kappa", "freqParameter.1", "ucldStdev",  "RateStatLogger.mean", "RateStatLogger.variance")


parameters.latex = lapply(parameters, function(x) x)
names(parameters.latex) = parameters
parameters.latex["birthRate"] = "Yule birth rate $\\lambda$"
parameters.latex["TreeHeight"] = "Tree height $h$"
parameters.latex["kappa"] = "HKY model $\\kappa$"
parameters.latex["freqParameter.1"] = "Nucleotide frequency $f$"
parameters.latex["ucldStdev"] = "Clock standard deviation $\\sigma$"
parameters.latex["RateStatLogger.mean"] = "Branch rate mean"
parameters.latex["RateStatLogger.variance"] = "Branch rate variance"


BURNIN = 0.1


settings = rev(list.dirs("sim", recursive = F))
nsim = 100
N = 30

# True values
true.df = read.table(paste0("true/", "priorSamples_N", N, ".log"), sep = "\t", header = T)
true.df[,"RateStatLogger.mean"] = numeric(0)
true.df[,"RateStatLogger.variance"] = numeric(0)
true.df = true.df[1:nsim,parameters]



for (sett in settings){


	s = gsub("[.]", "", gsub("sim/", "", sett))
	filename = paste0("WCSS_", s)
	cat(paste(s, "\n"))

	# Estimated values
	means.df = true.df
	for (col in colnames(means.df)) means.df[,col] = NA
	lower.df = means.df
	upper.df = means.df



	for (sim in 1:nsim){
	

		cat(paste(sim, "\n"))

		# Estimated values
		sim.df = read.table(paste0(sett, "/logfile_", sim, ".log"), sep = "\t", header = T)
		sim.df = sim.df[floor(BURNIN*nrow(sim.df)):nrow(sim.df),]

		# Put the true rates/variances in the true df
		true.df[sim,"RateStatLogger.mean"] = sim.df[1,"RateStatLogger.true.mean"]
		true.df[sim,"RateStatLogger.variance"] = sim.df[1,"RateStatLogger.true.variance"]


		# Tidy column names
		colnames(sim.df)[colnames(sim.df) == "Tree.height"] = "TreeHeight"
		colnames(sim.df)[colnames(sim.df) == "kappa.t"] = "kappa"
		colnames(sim.df)[grep("freqParameter", colnames(sim.df))[1]] = "freqParameter.1"
		colnames(sim.df)[colnames(sim.df) == "RateStatLogger.mcmc.mean"] = "RateStatLogger.mean"
		colnames(sim.df)[colnames(sim.df) == "RateStatLogger.mcmc.variance"] = "RateStatLogger.variance"
		sim.df = sim.df[,parameters]
	
		# Get mean and HPD of each estimated parameter
		for (p in parameters){
		
			m = mean(sim.df[,p])
			hpd = HDInterval::hdi(sim.df[,p])
			
			means.df[sim,p] = m
			lower.df[sim,p] = as.numeric(hpd[1])
			upper.df[sim,p] = as.numeric(hpd[2])
		
		}
		
		
	}
	
	
	cat("Making plots...\n")
	
	# Make plot
	options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
	"\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
	"\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(paste0(filename, ".tex"), width = 8, height = 8, standAlone = TRUE,
		packages = c("\\usepackage{tikz}",
			 "\\usepackage[active,tightpage,psfixbb]{preview}",
			 "\\PreviewEnvironment{pgfpicture}",
			 "\\setlength\\PreviewBorder{0pt}",
			 "\\usepackage{amssymb}"))
	par(mfrow = c(3,3))


	
	for (p in parameters){
	
		x = true.df[,p]
		y = means.df[,p]
	
		xymax = c(0, max(x,y)*1.5)
	
		plot(0, 0, type = "n", ylim = xymax, xlim = xymax, xlab = "True value", ylab = "Estimated value", xaxs = "i", yaxs = "i", axes = F, main = parameters.latex[[p]])

		lines(xymax, xymax, lty = "2525")
		
		
		coverage = 0
		for (sim in 1:nsim){
			l = lower.df[sim,p]
			u = upper.df[sim,p]
			t = x[sim]
			covered = l <= t & t <= u
			lines(c(t, t), c(l, u), col = ifelse(covered, "#008cba", "#ba2e00"))
			if (covered) coverage = coverage + 1
		}
		points(x, y, pch = 16)
		
		mtext(paste0("Coverage: ", coverage, "\\%"), cex = 0.7)
		
		
		
		axis(1)
		axis(2, las = 2)
	
	
	}

	dev.off()
	tools::texi2pdf(paste0(filename, ".tex"))




}






















