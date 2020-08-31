
library(tikzDevice)
library(HDInterval)
library(lubridate)
library(colorspace)
library(rjson)



PER_CHAIN = FALSE

#settings = c("cat", "real", "quant", "bactrian95", "bactrian98")
settings = c("cat",  "real_06", "quant_06", "real", "quant", "bactrian95", "bactrian98", "NER", "AVMN")
settings = c("cat", "cat_adaptive",  "real_06", "real", "real_adaptive", "quant_06_cached", "quant_cached", "quant_cached_adaptive", "bactrian95", "NER", "AVMVN")

benchmark = data.frame(batch = numeric(0), param = character(0), dataset = character(0), partition = numeric(0), setting = character(0), nstates = numeric(0), finalESS = numeric(0), finalMCD = numeric(0), done = logical(0),  error = logical(0))




settings.names = lapply(settings, function(x) x)
settings.names["cat"] = "\\textit{cat}"
settings.names["cat_adaptive"] = "adapt(\\textit{cat})"

settings.names["real"] = "cons(\\textit{real})"
settings.names["real_06"] = "nocons(\\textit{real})"
settings.names["real_adaptive"] = "adapt(\\textit{real})"

settings.names["quant_cached"] = "cons(\\textit{quant})"
settings.names["quant_06_cached"] = "nocons(\\textit{quant})"
settings.names["quant_cached_adaptive"] = "adapt(\\textit{quant})"
settings.names["bactrian95"] = "Bactrian+adapt(\\textit{real})"
settings.names["NER"] = "NER+Bactrian+adapt(\\textit{real})"
settings.names["AVMVN"] = "AVMVN+NER+Bactrian+adapt(\\textit{real})"




settings.names.short = settings.names
settings.names.short["bactrian95"] = "Bactrian"
settings.names.short["NER"] = "NER"
settings.names.short["AVMVN"] = "AVMVN"


latex.table = read.table("../latex.table", sep = "&")
colnames(latex.table) = c("ID", "N", "P", "L", "Leff", "ref")
latex.table$ref = gsub(" ", "", gsub("[0-9].+", "", latex.table$ref))



latex.table$completion = ""

progress.df = read.csv("progressVM.csv", header = T)
#progress.df$batch = ifelse(progress.df$batch == 4, 5, ifelse(progress.df$batch == 5, 4, progress.df$batch))


#parameters = c("posterior", "likelihood", "prior", "height", "birthRate", "kappa",  "ucldStdev", "clockRate", "TipRates", "RateStatLogger.mean", "RateStatLogger.variance")
parameters = c("prior", "likelihood", "TipRates", "treeLength", "ucldStdev", "kappa")
parameters = c("prior", "likelihood", "TipRates", "RateStatLogger.mean", "treeLength", "ucldStdev", "kappa")


parameters.latex = lapply(parameters, function(x) x)
names(parameters.latex) = parameters
parameters.latex["posterior"] = "Posterior $P(\\theta|D)$"
parameters.latex["likelihood"] = "Likelihood"
parameters.latex["prior"] = "Prior"
parameters.latex["birthRate"] = "Yule birth rate $\\lambda$"
parameters.latex["gammaShape"] = "$\\Gamma$ shape"
parameters.latex["kappa"] = "HKY model $\\kappa$"
#parameters.latex["freqParameter"] = "Nucleotide frequency $f$"
parameters.latex["ucldStdev"] = "Clock SD $\\sigma$"
parameters.latex["TipRates"] = "Leaf rate"
parameters.latex["RateStatLogger.mean"] = "Mean branch rate"
parameters.latex["treeLength"] = "Tree length"
parameters.latex["height"] = "Tree height $h$"



parameters.latex.small = lapply(parameters, function(x) x)
names(parameters.latex.small) = parameters
parameters.latex.small["posterior"] = "$P$"
parameters.latex.small["likelihood"] = "$\\mathcal{L}$"
parameters.latex.small["prior"] = "$p$"
parameters.latex.small["birthRate"] = "$\\lambda$"
parameters.latex.small["gammaShape"] = "$\\Gamma$"
parameters.latex.small["kappa"] = "$\\kappa$"
#parameters.latex.small["freqParameter"] = "$f$"
parameters.latex.small["ucldStdev"] = "$\\sigma$"
parameters.latex.small["TipRates"] = "$r$"
parameters.latex.small["treeLength"] = "$l$"
parameters.latex.small["height"] = "$h$"
parameters.latex.small["clockRate"] = "$\\mu$"
parameters.latex.small["RateStatLogger.variance"] = "$v$"
parameters.latex.small["RateStatLogger.mean"] = "$\\bar{r}$"



getDateTime = function(str){

	year = as.numeric(strsplit(str, " +")[[1]][6])
	month = strsplit(str, " +")[[1]][2]
	day = strsplit(str, " +")[[1]][3]
	if (nchar(day) == 1) day = paste0("0", day)
	time = strsplit(str, " +")[[1]][4]

	parse_date_time(paste(year, month, day, time), "%Y %b %d %H:%M:%S")
				

}


ESS.df = data.frame(batch = numeric(0), param = character(0), dataset = character(0), partition = numeric(0), setting = character(0), nstates = numeric(0),
		ESS = numeric(0), hr = numeric(0), ESS.hr = numeric(0), Leff = numeric(0), L = numeric(0), N = numeric(0), sigma = numeric(0),
		CD_weight = numeric(0), RW_weight = numeric(0), SPP_weight = numeric(0), NER_weight = numeric(0), NER_acc = numeric(0), NE_acc = numeric(0),
		leafCD_weight = numeric(0), leafAVMVN_weight = numeric(0), cis_weight = numeric(0), trans_weight = numeric(0),  nsamples = numeric(0))





speed.df = data.frame(batch = numeric(0), dataset = character(0), setting = character(0), state = numeric(0), speed = numeric(0))

# For each batch
batchfolders = "BatchN" #list.dirs(recursive = F)
for (f in batchfolders){

	print(f)

	if (f == "./datasets") next
	batchNum = as.numeric(gsub("./Batch", "", f))

	setwd(f)
	batch = as.numeric(substring(f, 8))

	# For each dataset in this batch
	datasetFolders = list.dirs(recursive = F)
	for (d in datasetFolders) {

		
		if (length(grep("Ran_", d)) > 0) next
		print(d)

		dataset = substring(d, 3)
		matching_row_num = sapply(latex.table$ref, function(pattern) length(grep(pattern, dataset)) > 0)
		if (sum(matching_row_num) == 0) {
			print(paste("No matching row for", dataset))
			print(latex.table$ref)
			next
		}

		setwd(d)
		matching_row = latex.table[matching_row_num,]
		N = matching_row$N
		ID = matching_row$ID

		# For each partition in this dataset
		partitionFolders = list.dirs(recursive = F)
		partitionsDone = numeric(0)
		for (pf in partitionFolders){

			setwd(pf)

			P = as.numeric(substring(pf, 12))

			P_index = which(as.numeric(strsplit(as.character(matching_row$P), "/")[[1]]) == P)
			L = as.numeric(strsplit(as.character(matching_row$L), "/")[[1]])[P_index]
			Leff = as.numeric(strsplit(as.character(matching_row$Leff), "/")[[1]])[P_index]
			


			doSetting = TRUE
			if (doSetting) {

				# For each setting
				for (s in settings) {


					setwd(s)
					
					
					# Total time on VM
					runtime.hr = 1
					progress.rows = progress.df[progress.df$batch == "V" & 
												progress.df$dataset == gsub("/", "", dataset) &
												progress.df$setting == s,]

					if (nrow(progress.rows) > 0) {
						start.date = progress.rows[progress.rows$status == "start", "date"]
						finish.date = progress.rows[progress.rows$status == "finish", "date"]
						if (length(start.date) != 0 & length(finish.date) != 0){
						
							start.date = getDateTime(start.date)
							finish.date = getDateTime(finish.date)
							int = interval(start.date, finish.date)
							runtime.hr = time_length(int, "hour")
							
							if(runtime.hr < 0.01) cat(paste("Warning:", dataset, s, "has a runtime of", runtime.hr, "\n"))
							
						}
					}else{
						cat(paste("Warning: cannot find runtime for", dataset, s, "\n"))
					}




					# For each batch
					batchFolders = list.dirs(recursive = F)
					for (bb in batchFolders) {
				
						setwd(bb)

						# Read in ess file
						ess.file = "ess.log"
		
						if (!file.exists(ess.file)){
							#print(paste("Cannot find", d, s, bb, ess.file))
							setwd("../")
							next
						}

						if (length(readLines(ess.file)) <= 1){
							print(paste("Empty file:", d, s, bb, ess.file))
							setwd("../")
							next
						}


						this.ESS.df = read.table(ess.file, sep = "\t", header = T)


						# Adaptive
						CD_weight = -1
						RW_weight = -1
						SPP_weight = -1
						NER_weight = -1
						NER_acc = -1
						NE_acc = -1
						leafCD_weight = -1
						leafAVMVN_weight = -1
						nsamples = 0
						cis_weight = -1
						trans_weight = -1
						
						state.in = paste0("tmp.xml.0state")
						if (file.exists(state.in)) {
							state.file = readLines(state.in)
							
							
							# N states
							nsamples = state.file[grep("sample=", state.file)]
							nsamples = gsub(".+sample.'", "", nsamples)
							nsamples = as.numeric(gsub("'.+", "", nsamples))
							
							if (length(grep("<!--", state.file)) > 0){
							
								operator.lines = paste(state.file[(grep("<!--", state.file)[1]+1):(grep("-->", state.file)[1]-1)], collapse = " ")
								json = fromJSON(operator.lines)
								ops = json$operators
								
								if (length(grep("adaptive", s)) == 1){
								

									match = which(sapply(ops, function(ele) ele$id == "AdaptableOperatorSampler.rates.internal"))
									if (length(match) == 1){
										weights = as.numeric(strsplit(gsub("([[]|[]])", "", ops[[match]]$weights), ",")[[1]])
										CD_weight = weights[1]
										RW_weight = weights[2]
										SPP_weight = weights[3]
									}
									
									
									match = which(sapply(ops, function(ele) ele$id == "AdaptableOperatorSampler.ucldStdev"))
									if (length(match) == 1){
									
									
										weights = as.numeric(strsplit(gsub("([[]|[]])", "", ops[[match]]$weights), ",")[[1]])
										cis_weight = weights[1]
										trans_weight = weights[3]
				
									}

								}
								
								
								if (length(grep("AVMVN", s)) == 1){
								
									match = which(sapply(ops, function(ele) ele$id == "AdaptableOperatorSampler.rates.leaf"))
									if (length(match) == 1){
										weights = as.numeric(strsplit(gsub("([[]|[]])", "", ops[[match]]$weights), ",")[[1]])
										leafCD_weight = weights[1]
										leafAVMVN_weight = weights[2]
										#print(paste("AVMVN weights", leafCD_weight, leafAVMVN_weight))
									}

								}
								
								
								
								if (length(grep("NER", s)) == 1){
									

									match = which(sapply(ops, function(ele) ele$id == "AdaptableOperatorSampler.NER"))
									if (length(match) == 1){
										weights = as.numeric(strsplit(gsub("([[]|[]])", "", ops[[match]]$weights), ",")[[1]])
										NER_weight = weights[2]
										NE_acc = ops[[match]]$operators[[1]]$accept / (ops[[match]]$operators[[1]]$reject + ops[[match]]$operators[[1]]$accept)
										NER_acc = ops[[match]]$operators[[2]]$accept / (ops[[match]]$operators[[2]]$reject + ops[[match]]$operators[[2]]$accept)
										#print(paste("NER weight", NER_weight, NE_acc, NER_acc))
									}

								
								}
								
							}
						
						}
						
						if (nsamples != 5e7 & nsamples != 1e8){
							print(paste("Warning:", dataset, s, "has", nsamples, "samples"))
						}
						
													
						sigma = as.numeric(this.ESS.df[1,"ucldStdev.mean"])
						if (sigma > 1.5){
							cat(paste("Warning: ", d, s, bb, "has sigma =", sigma, "\n"))
							setwd("../")
							next
							
						}
					
						for (p in parameters){

							pindex = grep(paste0(p, ".+ESS"), colnames(this.ESS.df))
							ESS = mean(as.numeric(this.ESS.df[1,pindex]), na.rm = T)
							

							#print(paste(p, ESS))
		
							if (!is.na(ESS) & ESS < 200){
								#cat(paste("Warning:", p, "has ESS =", signif(ESS, 2), "for", d, s, bb, "\n"))
							}

							
							ESS.df2 = data.frame(batch = bb, param = p, setting = s, dataset = dataset, ESS = ESS, hr = runtime.hr, partition = P, Leff = Leff, L = L, N = N, sigma = sigma,
									 CD_weight = CD_weight, RW_weight = RW_weight, SPP_weight = SPP_weight, NER_weight = NER_weight, NER_acc = NER_acc, NE_acc = NE_acc, leafCD_weight = leafCD_weight,
									 leafCD_weight = leafCD_weight, cis_weight = cis_weight, trans_weight = trans_weight, nsamples = nsamples)
							ESS.df = rbind(ESS.df, ESS.df2)

							
						}

						setwd("../")

					}

					setwd("../")
					
				}

			}


			setwd("../")
		}



		latex.table[sapply(latex.table$ref, function(pattern) length(grep(pattern, dataset)) > 0),"completion"] = paste(partitionsDone, collapse = "/")



		setwd("../")
	}



	setwd("../")

}



###########################
########### ESS ###########
###########################

#settings = c("cat", "real", "quant")

ESS.df = ESS.df[!is.na(ESS.df$ESS),]
if (PER_CHAIN) {
	div.by = ESS.df$nsamples / 1e6
	ESS.df$ESS.hr = ESS.df$ESS / div.by
}else{
	ESS.per.state = ESS.df$ESS / ESS.df$nsamples
	state.per.hr = 1e7 / ESS.df$hr
	ESS.df$ESS.hr = ESS.per.state * state.per.hr
}



datasets = unique(ESS.df[order(ESS.df$N), "dataset"]) # sort(as.character(unique(ESS.df$dataset)))

data.cols = c(rainbow_hcl(8, c = 250, l = 60), "#444444")
#c("red", "blue", "green", "yellow", "orange", "black", "purple", "darkgreen", "cyan", "magenta")
dataset_names = sapply(strsplit(datasets, "_"), function(ele) ele[1])

plotRealCat = function(){



	maxL = 20#max(ESS.df$L) + 2
	plot(0, 0, type = "n", xlim = c(0,maxL), ylim = c(0, 20000), xlab = "Sequence length $L$ (kb)", ylab = "ESS/hr of tip rates $r$", axes = F, xaxs = "i", yaxs = "i", 
					main ="Leaf rate mixing vs sequence length", cex.main = 1.5, cex.lab = 1.5)



	cols = c("#008cba", "#696969")

	pches = c(16, 17) 
	for (dataNum in 1:length(datasets)){
		s1 = "real_adaptive"
		s2 = "cat_adaptive"
		#s3 = "quant_cached_adaptive"
		d = datasets[dataNum]
		col = data.cols[dataNum]

		s1.df = ESS.df[ESS.df$dataset == d & ESS.df$setting == s1 & ESS.df$CD_weight >= 0 & ESS.df$param == "TipRates",]
		s2.df = ESS.df[ESS.df$dataset == d & ESS.df$setting == s2 & ESS.df$CD_weight >= 0 & ESS.df$param == "TipRates",]
		#s3.df = ESS.df[ESS.df$dataset == d & ESS.df$setting == s3 & ESS.df$CD_weight >= 0 & ESS.df$param == "TipRates",]

		L = mean(s1.df$L)

		y1 = mean(s1.df$ESS.hr)
		y2 = mean(s2.df$ESS.hr)
		#y3 = mean(s3.df$ESS.hr)
		
		
		#print(paste(L, y1, y2, d, col)) 
		
		lines(c(L, L), c(y1, y2), lty = "2525")
		text(L + 0.2, (y1 + y2)/2, paste0("$", signif(y1 / y2, 2), "\\times $"), adj = 0)

		points(L, y1, col = col, pch = pches[1])
		points(L, y2, col = col, pch = pches[2])



	}
	
	#abline(1, 0, lty = "2525")
	
	legend("topright", c("adapt (\\textit{real})", "adapt (\\textit{cat})"), col = "black", pch = pches, bty = "n", cex = 1.15)


	axis(1)
	axis(2, las = 2)

}


plotWeights = function() {



	maxL = 20#max(ESS.df$L) + 2
	plot(0, 0, type = "n", xlim = c(0,maxL), ylim = c(0, 1), xlab = "Sequence length $L$ (kb)", ylab = "Learned operator weight", axes = F, xaxs = "i", yaxs = "i", 
					main ="\\texttt{AdaptiveOperatorSampler} weights", cex.main = 1.5, cex.lab = 1.5)

	Ls = numeric(0)
	CDs = numeric(0)
	RWs = numeric(0)

	for (dataNum in 1:length(datasets)){


		s = "real_adaptive"
		d = datasets[dataNum]

		sub.df = ESS.df[ESS.df$dataset == d & ESS.df$setting == s & ESS.df$CD_weight >= 0 & ESS.df$param == "prior",]

		if (nrow(sub.df) == 0){
			print(paste("No operators for", d, s))
			next
		}

		CD =  mean(sub.df$CD_weight)
		RW =  mean(sub.df$RW_weight) 
		SPP = mean(sub.df$SPP_weight)

		pches = c(16, 17, 18) 

		points(mean(sub.df$L), CD, col = data.cols[dataNum], pch = pches[1])
		points(mean(sub.df$L), RW, col = data.cols[dataNum], pch = pches[2])
		#points(mean(sub.df$L), SPP, col = data.cols[dataNum], pch = pches[3])
		#, "\\texttt{SampleFromPrior}"

		CDs = c(CDs, CD)
		RWs = c(RWs, RW)
		Ls = c(Ls, mean(sub.df$L))


		legend("right", c("\\texttt{ConstantDistance}", "\\texttt{RandomWalk}"), pch = pches, bty = "n", cex = 1.15)


	}

	# Fit logit curve to CD
	logit = glm(formula = CDs ~ Ls, family = "binomial")
	x = seq(from = 0, to = maxL, length = 500)
	j = exp(logit$coefficients[1] + logit$coefficients[2]*x)
	y = j / (1+j)
	lines(x, y, col = "#69696999")

	
	# Fit logit curve to RW
	logit = glm(formula = RWs ~ Ls, family = "binomial")
	x = seq(from = 0, to = maxL, length = 500)
	j = exp(logit$coefficients[1] + logit$coefficients[2]*x)
	y = j / (1+j)
	lines(x, y, col = "#69696999")



	axis(1)
	axis(2, las = 2)



}




plotNERWeights = function(rates = FALSE) {


	maxL = 20#max(ESS.df$L) + 2

	if (!rates){
		main = "\\texttt{AdaptiveOperatorSampler(NER)} weights"
		ylab = "Learned operator weight"
		ylim = c(0,1)
		
		plot(0, 0, type = "n", xlim = c(0,maxL), ylim = ylim, xlab = "Sequence length $L$ (kb)", ylab = ylab, axes = F, xaxs = "i", yaxs = "i", 
					main = main, cex.main = 1.5, cex.lab = 1.5)
		
	}else{
		main = "Relative acceptance rate"
		ylab = "$\\alpha($NER$ \\{ \\mathcal{D}_{A,E}, \\mathcal{D}_{B,E}, \\mathcal{D}_{C,E} \\}) / \\alpha($NER$\\{ \\})$"
		ylim = c(0.5, 2)
		
		plot(0, 0, type = "n", xlim = c(0,maxL), ylim = ylim, xlab = "Sequence length $L$ (kb)", ylab = ylab, axes = F, xaxs = "i", yaxs = "i", 
					main = main, cex.main = 1.5, cex.lab = 1.5, log = "y")
		
	}



	
	

	Ls = numeric(0)
	NERs = numeric(0)
	NEs = numeric(0)
	pches = c(16, 17) 
	for (dataNum in 1:length(datasets)){


		s = "NER"
		d = datasets[dataNum]

		sub.df = ESS.df[ESS.df$dataset == d & ESS.df$setting == s & ESS.df$NER_weight >= 0 & ESS.df$param == "prior",]

		if (nrow(sub.df) == 0){
			print(paste("No operators for", d, s))
			next
		}


		

		if (!rates){
			NER =  mean(sub.df$NER_weight)
			NE = 1 - NER
			points(mean(sub.df$L), NER, col = data.cols[dataNum], pch = pches[1])
			points(mean(sub.df$L), NE, col = data.cols[dataNum], pch = pches[2])
			NEs = c(NEs, NE)
			
		
		
		}else{
			NER = mean(sub.df$NER_acc) / mean(sub.df$NE_acc)
			points(mean(sub.df$L), NER, col = data.cols[dataNum], pch = 18)
			
			
			if (NER > 1) cat(paste("For", d, "NER has an acceptance rate", signif(100*(NER-1), 2), "% higher than NarrowExchange\n"))
			
		}

		NERs = c(NERs, NER)
		Ls = c(Ls, mean(sub.df$L))

	}
	
	if (!rates) {
		legend("bottomleft", c("NER$ \\{ \\mathcal{D}_{A,E}, \\mathcal{D}_{B,E}, \\mathcal{D}_{C,E} \\}$", "NER$\\{ \\}$"), pch = pches, bty = "n", cex = 1.15)
	}else{
		legend("bottomright", dataset_names, fill = data.cols[1:length(datasets)], bg = "white", bty = "n", cex = 1)
	}


	if (!rates) {
		abline(0.5, 0, lty = "2525")
	}else{
		lines(c(0,maxL), c(1, 1), lty = "2525")
	}


	if (length(NERs) > 0){

		if (!rates) {
			# Fit logit curve to NER
			logit = glm(formula = NERs ~ Ls, family = "binomial")
			x = seq(from = 0, to = maxL, length = 500)
			j = exp(logit$coefficients[1] + logit$coefficients[2]*x)
			y = j / (1+j)
			lines(x, y, col = "#69696999")


		
			# Fit logit curve to NE
			logit = glm(formula = NEs ~ Ls, family = "binomial")
			x = seq(from = 0, to = maxL, length = 500)
			j = exp(logit$coefficients[1] + logit$coefficients[2]*x)
			y = j / (1+j)
			lines(x, y, col = "#69696999")
		}else{
			logN = log(NERs)
			linear = lm(formula = logN ~ Ls)
			x = seq(from = 0, to = maxL, length = 500)
			y = exp(linear$coefficients[1] + linear$coefficients[2]*x)
			lines(x, y, col = "#69696999")
			
		}
	}

	axis(1)
	
	if (!rates){
		axis(2, las = 2)
	}else{
		axis(2, at = c(1/2, 2/3, 1, 3/2, 2/1), labels = paste0(c("$\\frac{1}{2}", "$\\frac{2}{3}", "$1", "$\\frac{3}{2}", "$\\frac{2}{1}"), "\\times$"), las = 2)
	}



}


plot.speed = function(filename){



	# Time per setting
	s0 = "cat"
	options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
		"\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
		"\\usepackage{amssymb}"))
	## I need the amssymb package because I use \mathcal and \mathbb
		tikz(paste0(filename, ".tex"), width = 8, height = 8 / 1.618, standAlone = TRUE,
			packages = c("\\usepackage{tikz}",
				 "\\usepackage[active,tightpage,psfixbb]{preview}",
				 "\\PreviewEnvironment{pgfpicture}",
				 "\\setlength\\PreviewBorder{0pt}",
				 "\\usepackage{amssymb}"))


	plot(0, 0, type = "n",  xlim = c(0, (length(settings)+1) * (length(datasets)+2) + 3), log = "y", ylim = c(0.5, 2), axes = F, xaxs = "i", yaxs = "i", xlab = "", ylab = "Relative samples/hr", main = "Relative speed compared with \\textit{cat}")
	

	i = 1
	xat = numeric(0)
	xlab = character(0)
	lines(c(0, (length(settings)-1) * (length(datasets)+2)), c(1,1), lty = "2525")
	for (s in settings[settings != s0]){

		setting_mean = numeric(0)
		i0 = i
	
		for (dataNum in 1:length(datasets)){
			
			d = datasets[dataNum]
			col = data.cols[dataNum]

			time0 = mean(ESS.df[ESS.df$dataset == d & ESS.df$setting == s0,"hr"])
			time = mean(ESS.df[ESS.df$dataset == d & ESS.df$setting == s,"hr"])
			E = log(time / time0)
			
			# Error bar in log space
			m1 = mean(E)

			#text(i, m1, parameters.latex.small[[p]], col = data.cols[dataNum], cex = 1.4)
			points(i, exp(m1), col = data.cols[dataNum], pch = 16)
			setting_mean = c(setting_mean, exp(m1))
			
			i = i + 1
		
		}
		
		
		setting_mean = mean(setting_mean)
		diff = 100 * signif((setting_mean - 1), 2)
		toprint = paste0(abs(diff), "\\% ", ifelse(diff > 0, "slower", "faster"))
		lines(c(i0, i), c(setting_mean, setting_mean), lwd = 0.5)
		text((i + i0)/2, 1.9, toprint, cex = 0.7, adj = 0.5)
		xat = c(xat, (i + i0)/2)
		xlab = c(xlab, settings.names.short[[s]])
		lines(c(i+1, i+1), c(0.0001, 100), lwd = 0.5, col = "#69696999")
		
		i = i + 2
		
	}


	
	axis(1, at = c(-1, xat, 100000), labels = c("", xlab, ""), las=2, cex.axis = 0.6)
	axis(2, at = c(1/2, 3/4, 1, 4/3, 2/1), labels = paste0(c("$\\frac{1}{2}", "$\\frac{3}{4}", "$1", "$\\frac{4}{3}", "$\\frac{2}{1}"), "\\times$"), las = 2)

	legend("right", dataset_names, fill = data.cols[1:length(datasets)], bg = "white", bty = "n", cex = 1)


	dev.off()
	tools::texi2pdf(paste0(filename, ".tex"))



}


plot.2 = function(filename, s1, s2) {


	options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
		"\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
		"\\usepackage{amssymb}"))
	## I need the amssymb package because I use \mathcal and \mathbb
		tikz(paste0(filename, ".tex"), width = 8, height = 8 / 1.618, standAlone = TRUE,
			packages = c("\\usepackage{tikz}",
				 "\\usepackage[active,tightpage,psfixbb]{preview}",
				 "\\PreviewEnvironment{pgfpicture}",
				 "\\setlength\\PreviewBorder{0pt}",
				 "\\usepackage{amssymb}"))

	par(mar = c(5,4,3,2))

	plot(0, 0, type = "n", xlim = c(0, (length(parameters)+1) * (length(datasets)+2) + 3), ylim = c(2/3, 1.7), xlab = "", ylab = "Relative ESS/hr",
				main = paste("Comparison of", settings.names[[s1]], "and", settings.names[[s2]]), axes = F, xaxs = "i", yaxs = "i", log = "y")
	i = 1

	xat = numeric(0)
	xlab = character(0)
	lines(c(0, (length(parameters)) * (length(datasets)+2)), c(1,1), lty = "2525")
	for (p in parameters){
	
		param_mean = numeric(0)
		i0 = i
	
		for (dataNum in 1:length(datasets)){
			
			d = datasets[dataNum]
			col = data.cols[dataNum]


			ESS1 = ESS.df[ESS.df$dataset == d & ESS.df$setting == s1 & ESS.df$param == p,"ESS.hr"]
			ESS2 = ESS.df[ESS.df$dataset == d & ESS.df$setting == s2 & ESS.df$param == p,"ESS.hr"]
			n = min(length(ESS1), length(ESS2))
			E = log(ESS1[1:n] / ESS2[1:n])
			
			# Error bar in log space
			m1 = mean(E)
			se1 = 1 * sqrt(var(E) / n)
			
			lines(c(i,i), exp(m1 + c(se1, -se1)), col = col)
			#text(i, m1, parameters.latex.small[[p]], col = data.cols[dataNum], cex = 1.4)
			points(i, exp(m1), col = data.cols[dataNum], pch = 16)
			param_mean = c(param_mean, exp(m1))
			
			i = i + 1

		}
		
		param_mean = mean(param_mean)
		toprint = 100 * signif((param_mean - 1), 2)
		lines(c(i0, i), c(param_mean, param_mean), lwd = 0.5)
		text((i + i0)/2, 1.5, paste0(toprint, "\\% faster"), cex = 0.9, adj = 0.5)
		xat = c(xat, (i + i0)/2)
		xlab = c(xlab, parameters.latex[[p]])
		lines(c(i+1, i+1), c(0.0001, 100), lwd = 0.5, col = "#69696999")
		
		i = i + 2

	}
	
	axis(1, at = c(-1, xat, 100000), labels = c("", xlab, ""), las=2, cex.axis = 0.6)
	axis(2, at = c(2/3, 3/4, 7/8, 1, 8/7, 4/3, 3/2), labels = paste0(c("$\\frac{2}{3}", "$\\frac{3}{4}", "$\\frac{7}{8}", "$1", "$\\frac{8}{7}", "$\\frac{4}{3}", "$\\frac{3}{2}"), "\\times$"), las = 2)
	
	#abline(1, 0, lty = "2525")
	
	legend("right", dataset_names, fill = data.cols[1:length(datasets)], bg = "white", bty = "n", cex = 1)
	
	
	dev.off()
	tools::texi2pdf(paste0(filename, ".tex"))

}



plot.cell = function(s1, s2, show.legend = FALSE){


		r.ESS = ESS.df[ESS.df$setting == s1,]
		q.ESS = ESS.df[ESS.df$setting == s2,]


		#na.rm = !( is.na(r.ESS$ESS.hr) | is.na(q.ESS$ESS.hr) )

		#r.ESS = r.ESS[na.rm,]
		#q.ESS = q.ESS[na.rm,]

		xymin = 1
		xymax = 20000

		xyrange = c(xymin, xymax) #c(xymin, max(c(r.ESS$ESS.hr, q.ESS$ESS.hr), na.rm = TRUE))
		#xyrange = c(0, max(c(r.ESS$MCD.states, q.ESS$MCD.states)))
		
		#xyrange = c(0, 2000) 
		per = ifelse(PER_CHAIN, "ESS/$10^6$ states", "ESS/hr")
		plot(1, 1, xlim = xyrange, ylim = xyrange, xlab = paste0(per, " [", settings.names[[s1]], "]"),  ylab = paste0(per, " [", settings.names[[s2]], "]"), 
		type = "n", main = paste0(settings.names[[s2]], " vs. ", settings.names[[s1]]), log="xy", xaxs = "i", yaxs = "i", axes = FALSE, cex.main = 1.5, cex.lab = 1.5)


		
		


		times = c(4, 16, 64)
		line.cols = c("#696969dd", "#696969aa", "#69696977")

		lwd_base = 3
		for (i in length(times):1){
			t = times[i]
			polygon(c(0.0001, xymax*10, xymax*10, 0.0001), c(0.0001*t, xymax*10*t, xymax*10/t, 0.0001/t), lwd =lwd_base * 0.7^(i), border=line.cols[i]) 
		}


		textshift = 1
		for (i in length(times):1){
			t = times[i]
			text(xymin*t*textshift, xymin*textshift, paste0("$\\frac{1}{", t, "} \\times$"), adj = c(-0.5, -0.6), cex = 1)
			text(xymin*textshift, xymin*t*textshift, paste0("$", t, "\\times$"), adj = c(-0.2, -2), cex = 1)
		}
		abline(0,1, lwd = lwd_base, col = "#696969")

		colfunc = colorRampPalette(c("black", "red"))
		#max.len = max(benchmark[,v])
		ncolours = 15
		l.cols = colfunc(ncolours)
		get.col = function(Leff) {
			j = ceiling((Leff / max.len) * ncolours)
			l.cols[j]
		}




		#data.cols = c(rainbow_hcl(length(datasets)-1, l = 70, c = 80), "black")
		for (dataNum in 1:length(datasets)) {
			d = datasets[dataNum]
			for (p in parameters) {



				#if (any(p == c("posterior", "birthRate", "gammaShape", "kappa",  "freqParameter"))) next

				s1.ESS = ESS.df[ESS.df$setting == s1 & ESS.df$param == p & ESS.df$dataset == d,]
				s2.ESS = ESS.df[ESS.df$setting == s2 & ESS.df$param == p & ESS.df$dataset == d,]

				#linear.model = lm(s2.ESS$ESS.hr ~ s1.ESS$ESS.hr)

				#c = linear.model$coefficients[1]
				#b_L = linear.model$coefficients[2]
				#abline(c, b_L)
				
				xvals = s1.ESS$ESS.hr
				yvals = s2.ESS$ESS.hr

				#xvals = s1.ESS$MCD.states
				#yvals = s2.ESS$MCD.states


				m1 = mean(xvals)
				m2 = mean(yvals)
				se1 = 1*sqrt(var(xvals) / length(xvals))
				se2 = 1*sqrt(var(yvals) / length(yvals))
				lines(c(m1 - se1, m1 + se1), c(m2, m2), col = paste0(data.cols[dataNum], "55"))
				lines(c(m1, m1), c(m2 - se2, m2 + se2), col = paste0(data.cols[dataNum], "55"))
				
				#text(m1, m2, paste0(parameters.latex.small[[p]], "$_", which(datasets == d), "$"), col = paste0(data.cols[dataNum], "99"))
				text(m1, m2, parameters.latex.small[[p]], col = data.cols[dataNum], cex = 1.4)

			}

		}

		
		#box()
		axis(1, cex = 2)
		axis(2, cex = 2, las=2)


		
		#gsub("^a", "", gsub("_", " ", datasets))
		if (show.legend) legend("bottomright", dataset_names, fill = data.cols[1:length(datasets)], bg = "white", bty = "n", cex = 1)
		#legend("bottomright", as.character(parameters.latex[names(parameters.pch)[parameters.pch != names(parameters.pch)]]), pch = as.numeric(unlist(parameters.pch)[parameters.pch != names(parameters.pch)]))



}


plot.grid = function(filename, settings, grid.like = TRUE, fn = sum) {


	

	if (grid.like) {
		columns = length(settings)-1
		rows = length(settings)-1
		width = 4*columns
		height = width
	}else{
		columns = choose(length(settings), 2)
		rows = 1
		width = 4*columns
		height = 4
	}


	options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
	    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
	    "\\usepackage{amssymb}"))
	## I need the amssymb package because I use \mathcal and \mathbb
		tikz(paste0(filename, ".tex"), width = width, height = height, standAlone = TRUE,
	    	packages = c("\\usepackage{tikz}",
		         "\\usepackage[active,tightpage,psfixbb]{preview}",
		         "\\PreviewEnvironment{pgfpicture}",
		         "\\setlength\\PreviewBorder{0pt}",
		         "\\usepackage{amssymb}"))





	if (grid.like) {
		par(mfrow = c(rows, columns))
		par(mar = c(5,5,5,5))
	}else{
		#mat = matrix(c( rep(columns+1, columns), 1:columns ), byrow = T, nrow = 2)
		#print(mat)
		#layout(mat, heights = c(0.5,4))
		par(mfrow = c(1, columns))
		par(mar = c(5,5,5,5))

	}
	

	


	parameters.pch = lapply(parameters, function(x) x)
	names(parameters.pch) = parameters
	parameters.pch["prior"] = 19
	parameters.pch["likelihood"] = 17
	parameters.pch["ucldStdev"] = 1
	parameters.pch["TipRates"] = 0





		
	
	first = TRUE
	for (x in 1:(length(settings) -1)) {
		s1 = settings[x]

		for (y in 2:length(settings)) {
			s2 = settings[y]
			if (y <= x) {
				#if (grid.like) plot.new()
				next
			} 


			plot.cell(s1, s2, first)
			first = FALSE

			
		}

	}

	if (!grid.like){
		#par(mar = c(0,0,0,0))
		#plot.new()

		#text(0.5, 0, main, cex = 2, adj = c(0.5, 1))
		#title(xlab = main, cex.lab = 1.5)
	}


	fn()

	if (FALSE) {

		plot(1, 1, type  = "n", ylim = c(0, max(1/ESS.df$mean.speed)),  xlim = c(1, length(settings)), axes = F, xlab = "Setting", ylab = "Speed ($10^6$ states/hr)", 
							main = "Speed comparison", cex.main = 2, cex.lab = 2)



		for (dataNum in 1:length(datasets)){

			dataset = datasets[dataNum]
			speeds.df = ESS.df[ESS.df$param == "prior" & ESS.df$dataset == dataset, ]
			speeds = numeric(length(settings))
			for (s in 1:length(settings)){
				setting = settings[s]
				speeds[s] = mean(1 / speeds.df[speeds.df$setting == setting, "mean.speed"])
			}

			if (any(is.na(speeds))) next

			pch = rep(16, length(settings))
			fastest = which(speeds == max(speeds))
			pch[fastest] = 17

			points(1:length(settings), speeds, col = data.cols[dataNum], pch = pch, type = "b")


		}


		axis(1, at = 1:length(settings), label = sapply(settings, function(ele) settings.names[[ele]]), las = 2, cex = 2)
		axis(2, cex = 2)
		#grid()


	}



	dev.off()

	tools::texi2pdf(paste0(filename, ".tex"))
	#system(paste(getOption("pdfviewer"), paste0(filename, ".pdf")))

}


# Round 1
plot.grid("ESS_round1_real", c("real_06", "real", "real_adaptive"), TRUE, plotWeights)
plot.cat = function(){
	plot.cell("cat", "cat_adaptive", show.legend = FALSE)
}
plot.grid("ESS_round1_catquant", c("quant_06_cached", "quant_cached", "quant_cached_adaptive"), TRUE, plot.cat)


# Round 2
plot.grid("ESS_round2", c("cat_adaptive", "real_adaptive", "quant_cached_adaptive"), TRUE, plotRealCat)

# Round 3
plot.2("ESS_round3", "bactrian95", "real_adaptive")


# Round 4
filename = "ESS_round4"
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
	"\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
	"\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(paste0(filename, ".tex"), width = 8, height = 8 / 1.618, standAlone = TRUE,
		packages = c("\\usepackage{tikz}",
			 "\\usepackage[active,tightpage,psfixbb]{preview}",
			 "\\PreviewEnvironment{pgfpicture}",
			 "\\setlength\\PreviewBorder{0pt}",
			 "\\usepackage{amssymb}"))
par(mfrow = c(1,2))
plotNERWeights(FALSE)
plotNERWeights(TRUE)
dev.off()
tools::texi2pdf(paste0(filename, ".tex"))
plot.2("ESS_round4b", "bactrian95", "NER")


# Round 5
plot.2("ESS_round5", "AVMVN", "NER")
plot.real = function(){
	plot.cell("real", "AVMVN", show.legend = FALSE)
}
plot.grid("ESS_round5b", c("cat", "AVMVN"), TRUE, plot.real)

#plot.2("ESS_round5b", "AVMVN", "real", 500)




plot.speed("ESS_timesettings")


write.table(ESS.df, "ESSall.tsv", sep = "\t", quote = F, row.names = F)



cons.rel = numeric(0)
nocons.rel = numeric(0)
cons.rel.s = numeric(0)
nocons.rel.s = numeric(0)
for (dataNum in 1:length(datasets)) {
		d = datasets[dataNum]
		A = mean(ESS.df[ESS.df$setting == "real_adaptive" & ESS.df$param == "TipRates" & ESS.df$dataset == d,"ESS.hr"], na.rm = T)
		C = mean(ESS.df[ESS.df$setting == "real" & ESS.df$param == "TipRates" & ESS.df$dataset == d,"ESS.hr"], na.rm = T)
		N = mean(ESS.df[ESS.df$setting == "real_06" & ESS.df$param == "TipRates" & ESS.df$dataset == d,"ESS.hr"], na.rm = T)
		cons.rel = c(cons.rel, A/C - 1)
		nocons.rel = c(nocons.rel, A/N - 1)
		
		
		A = mean(ESS.df[ESS.df$setting == "real_adaptive" & ESS.df$param == "ucldStdev" & ESS.df$dataset == d,"ESS.hr"], na.rm = T)
		C = mean(ESS.df[ESS.df$setting == "real" & ESS.df$param == "ucldStdev" & ESS.df$dataset == d,"ESS.hr"], na.rm = T)
		N = mean(ESS.df[ESS.df$setting == "real_06" & ESS.df$param == "ucldStdev" & ESS.df$dataset == d,"ESS.hr"], na.rm = T)
		cons.rel.s = c(cons.rel.s, A/C - 1)
		nocons.rel.s = c(nocons.rel.s, A/N - 1)
			
}


cat(paste("\nadapt (real) is", signif(mean(cons.rel), 2)*100, "% faster than cons (real) wrt r \n"))
cat(paste("adapt (real) is", signif(mean(nocons.rel), 2)*100, "% faster than nocons (real) wrt r \n"))

cat(paste("\nadapt (real) is", signif(mean(cons.rel.s), 2)*100, "% faster than cons (real) wrt sigma \n"))
cat(paste("adapt (real) is", signif(mean(nocons.rel.s), 2)*100, "% faster than nocons (real) wrt sigma \n"))

#print(cons.rel.s)
#print(nocons.rel.s)


for (dataNum in 1:length(datasets)){
	d = datasets[dataNum]
	sigma = mean(ESS.df[ESS.df$dataset == d,"sigma"], na.rm = T)

	cat(paste(d, "has a clock sd of", sigma, "\n"))

}




extreme_cat = -Inf
extreme_real = -Inf
x1 = numeric(0)
x2 = numeric(0)
for (p in parameters){

	X = numeric(0)
	Y = numeric(0)
	Z = numeric(0)
	for (dataNum in 1:length(datasets)) {
		d = datasets[dataNum]
		X = c(X, mean(ESS.df[ESS.df$setting == "cat" & ESS.df$param == p & ESS.df$dataset == d,"ESS.hr"]))
		Y = c(Y, mean(ESS.df[ESS.df$setting == "real" & ESS.df$param == p & ESS.df$dataset == d,"ESS.hr"]))
		Z = c(Z, mean(ESS.df[ESS.df$setting == "AVMVN" & ESS.df$param == p & ESS.df$dataset == d,"ESS.hr"]))
		
	}
	print(paste("The mean ESS under AVMVN for", p, "is", signif(mean(Z/X, na.rm = T), 3), "times as fast wrt cat"))
	print(paste("The mean ESS under AVMVN for", p, "is", signif(mean(Z/Y, na.rm = T), 3), "times as fast wrt real"))
	
	
	if (any(Z/X > extreme_cat)){
		extreme_cat = max(Z/X)
	}
	
	
	if (any(Z/Y > extreme_real)){
		extreme_real = max(Z/Y)
	}
	
	if (p != "kappa"){
		x1 = c(x1,  signif(mean(Z/X, na.rm = T), 3))
		x2 = c(x2,  signif(mean(Z/Y, na.rm = T), 3))
	}
	
	
	
}

print(paste("AVMVN is between", min(x1), "and", max(x1), "times as fast as cat"))
print(paste("In the most extreme case, AVMVN was", signif(extreme_cat, 3), "times as fast as cat"))

print(paste("AVMVN is between", min(x2), "and", max(x2), "times as fast as real"))
print(paste("In the most extreme case, AVMVN was", signif(extreme_real, 3), "times as fast as real"))



# Cis / trans weights
cis_real = numeric(0)
trans_real = numeric(0)
cis_quant = numeric(0)
trans_quant = numeric(0)
for (dataNum in 1:length(datasets)) {
	d = datasets[dataNum]
	
	
	x1 = ESS.df[ESS.df$setting == "real_adaptive" & ESS.df$dataset == d,"cis_weight"]
	x2 = ESS.df[ESS.df$setting == "real_adaptive" & ESS.df$dataset == d,"trans_weight"]
	x3 = ESS.df[ESS.df$setting == "quant_cached_adaptive" & ESS.df$dataset == d,"cis_weight"]
	x4 = ESS.df[ESS.df$setting == "quant_cached_adaptive" & ESS.df$dataset == d,"trans_weight"]
	
	cis_real = c(cis_real, mean(x1[x1 != -1]))
	trans_real = c(trans_real, mean(x2[x2 != -1]))
	cis_quant = c(cis_quant, mean(x3[x3 != -1]))
	trans_quant = c(trans_quant, mean(x4[x4 != -1]))
}
cat(paste("For real, CisScale has a mean weight of", signif(mean(cis_real), 3), "and scale has", signif(mean(trans_real), 3), "\n" ))
cat(paste("For quant, CisScale has a mean weight of", signif(mean(cis_quant), 3), "and scale has", signif(mean(trans_quant), 3), "\n" ))





if (FALSE) {


	scale = 3
	pdf("times.pdf", width = scale*4, height = scale*2)
	par(mfrow = c(2, 4))
	for (s in settings[1:8]) {


		plot(0, 1, xlim = c(1,length(datasets)), ylim = c(0.1, 20),  main = s, ylab = "Time (hr)", xlab = "dataset", axes = F)


		for (dataNum in 1:length(datasets)){
		
		
		
		
				d = datasets[dataNum]
				col = data.cols[dataNum]
				
				time = ESS.df[ESS.df$setting == s & ESS.df$dataset == d & ESS.df$param == "prior","hr"]
				
				points(rep(dataNum, length(time)), time, pch = 16, col = col)
				
				
				if (s == settings[1]){
					dataset_names = sapply(strsplit(datasets, "_"), function(ele) ele[1])
					legend("topleft", dataset_names, fill = data.cols[1:length(datasets)], bg = "white", bty = "n", cex = 1)
				}
				
		}


		axis(2, las=2)

	}
	dev.off()
	
}

