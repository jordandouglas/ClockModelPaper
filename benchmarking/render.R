
library(tikzDevice)
library(HDInterval)
library(lubridate)
library(colorspace)


PER_CHAIN = TRUE

#settings = c("cat", "real", "quant", "bactrian95", "bactrian98")
settings = c("cat",  "real_06", "quant_06", "real", "quant", "bactrian95", "bactrian98", "NER", "AVMN")
settings = c("cat",  "real_06", "real", "real_adaptive", "quant_06_cached", "quant_cached_adaptive", "quant_cached", "bactrian95")

benchmark = data.frame(batch = numeric(0), param = character(0), dataset = character(0), partition = numeric(0), setting = character(0), nstates = numeric(0), finalESS = numeric(0), finalMCD = numeric(0), done = logical(0),  error = logical(0))




settings.names = lapply(settings, function(x) x)
settings.names["cat"] = "\\textit{cat}"

settings.names["real"] = "cons (\\textit{real})"
settings.names["real_06"] = "nocons (\\textit{real})"
settings.names["real_adaptive"] = "adapt (\\textit{real})"

settings.names["quant_cached"] = "cons (\\textit{quant})"
settings.names["quant_06_cached"] = "nocons (\\textit{quant})"
settings.names["quant_cached_adaptive"] = "adapt (\\textit{quant})"
settings.names["bactrian95"] = "Bactrian (\\textit{real})"
settings.names["NER"] = "NER (\\textit{real})"


latex.table = read.table("../latex.table", sep = "&")
colnames(latex.table) = c("ID", "N", "P", "L", "Leff", "ref")
latex.table$ref = gsub(" ", "", gsub("[0-9].+", "", latex.table$ref))



latex.table$completion = ""

progress.df = read.csv("progress.csv", header = T)
progress.df$batch = ifelse(progress.df$batch == 4, 5, ifelse(progress.df$batch == 5, 4, progress.df$batch))


#parameters = c("posterior", "likelihood", "prior", "height", "birthRate", "kappa",  "ucldStdev", "clockRate", "TipRates", "RateStatLogger.mean", "RateStatLogger.variance")
parameters = c("prior", "likelihood", "TipRates", "height", "ucldStdev", "clockRate")


parameters.latex = lapply(parameters, function(x) x)
names(parameters.latex) = parameters
parameters.latex["posterior"] = "Posterior $P(\\theta|D)$"
parameters.latex["likelihood"] = "Likelihood $P(D|\\theta)$"
parameters.latex["prior"] = "Prior $P(\\theta)$"
parameters.latex["birthRate"] = "Yule birth rate $\\lambda$"
parameters.latex["gammaShape"] = "$\\Gamma$ shape"
parameters.latex["kappa"] = "$\\kappa$"
#parameters.latex["freqParameter"] = "Nucleotide frequency $f$"
parameters.latex["ucldStdev"] = "Clock SD $\\sigma$"
parameters.latex["TipRates"] = "Mean tip rate $r$"
#parameters.latex["treeLength"] = "Tree length $l$"
parameters.latex["height"] = "Tree height $h$"


getDateTime = function(str){

	year = as.numeric(strsplit(str, " +")[[1]][6])
	month =strsplit(str, " +")[[1]][2]
	day = strsplit(str, " +")[[1]][3]
	if (nchar(day) == 1) day = paste0("0", day)
	time = strsplit(str, " +")[[1]][4]

	parse_date_time(paste(year, month, day, time), "%Y %b %d %H:%M:%S")
				

}


ESS.df = data.frame(batch = numeric(0), param = character(0), dataset = character(0), partition = numeric(0), setting = character(0), nstates = numeric(0),
 ESS = numeric(0), hr = numeric(0), ESS.hr = numeric(0), Leff = numeric(0), L = numeric(0), N = numeric(0), mean.speed = numeric(0))


speed.df = data.frame(batch = numeric(0), dataset = character(0), setting = character(0), state = numeric(0), speed = numeric(0))

# For each batch
batchfolders = list.dirs(recursive = F)
for (f in batchfolders){

	print(f)

	if (f == "./datasets") next
	batchNum = as.numeric(gsub("./Batch", "", f))

	setwd(f)
	batch = as.numeric(substring(f, 8))

	# For each dataset in this batch
	datasetFolders = list.dirs(recursive = F)
	for (d in datasetFolders) {

		print(d)


		#if (d == "./Fong_2012" | d == "./McCormack_2013") next
		

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


				
					# Read in output file
					
					fileName = paste0("error_", s, ".err")
					if (!file.exists(fileName)) {
						#print("Error cannot file file")
						next
					}
					output_in = readLines(fileName)


					# Is it done?
					progress.rows = progress.df[progress.df$batch == batchNum & 
												progress.df$dataset == gsub("/", "", dataset) &
												progress.df$setting == s,]

					start.date = progress.rows[progress.rows$status == "start", "date"]
					if (length(start.date) == 0){
						next
					}
					#print(paste("start", start.date))
					start.date = getDateTime(start.date)
					finish.date = progress.rows[progress.rows$status == "finish", "date"]
					#print(paste("finish", finish.date))
					if (length(finish.date) == 0){
						next
					}
					finish.date = getDateTime(finish.date)
					runtime.hr = as.numeric(finish.date - start.date)

					error = FALSE
					errorLine = grep("[*][*]", output_in)
					if (length(errorLine) > 0) {
						error = TRUE
					}


					# Time per million
					time.lines = output_in[grep("Msamples", output_in)]
					speeds = gsub(".+ ", "", time.lines)
					speeds = gsub("/Msamples", "", speeds)
					times = sapply(strsplit(speeds, "[a-z]"), function(ele) ifelse(length(ele) == 3, as.numeric(ele[1]) + 	as.numeric(ele[2])/60 + as.numeric(ele[3])/3600,
																			ifelse(length(ele) == 2, 					 	as.numeric(ele[1])/60 + as.numeric(ele[2])/3600,
																																					as.numeric(ele[1])/3600  )))
					mean.speed = mean(times)


					if (dataset == "simulated"){
						ess.file = paste0("benchmark_",  s, ".log.ess")
					}else{
						ess.file = paste0("benchmark_",  s, P, ".log.ess")
					}

					if (!file.exists(ess.file)){
						print(paste("Cannot find", f, d, pf, ess.file))
						next
					}

					if (length(readLines(ess.file)) <= 1){
						print(paste("Empty file:", f, d, pf, ess.file))
						next
					}

					#print(paste("Processing", f, d, pf, ess.file))

					this.ESS.df = read.table(ess.file, sep = "\t", header = T)


					for (p in parameters){

						pindex = grep(paste0(p, ".+ESS"), colnames(this.ESS.df))
						#ESS.hr = mean(ESS_values[pindex]) / time.hr
						#print(paste(chain, "_", p, "_", ESS.hr))

						ESS = mean(as.numeric(this.ESS.df[1,pindex]), na.rm = T)
						#print(paste(p, ESS))

						
						ESS.df2 = data.frame(batch = batch, param = p, setting = s, dataset = dataset, ESS = ESS, hr = runtime.hr, partition = P, Leff = Leff, L = L, N = N ,mean.speed = mean.speed)
						ESS.df = rbind(ESS.df, ESS.df2)
						
					}




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





plot.grid = function(filename, settings, grid.like = TRUE, main = "") {


	

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
	

	

	parameters.latex.small = lapply(parameters, function(x) x)
	names(parameters.latex.small) = parameters
	parameters.latex.small["posterior"] = "$P$"
	parameters.latex.small["likelihood"] = "$L$"
	parameters.latex.small["prior"] = "$p$"
	parameters.latex.small["birthRate"] = "$\\lambda$"
	parameters.latex.small["gammaShape"] = "$\\Gamma$"
	parameters.latex.small["kappa"] = "$\\kappa$"
	#parameters.latex.small["freqParameter"] = "$f$"
	parameters.latex.small["ucldStdev"] = "$\\sigma$"
	parameters.latex.small["TipRates"] = "$r$"
	#parameters.latex.small["treeLength"] = "$l$"
	parameters.latex.small["height"] = "$h$"
	parameters.latex.small["clockRate"] = "$\\mu$"
	parameters.latex.small["RateStatLogger.variance"] = "$v$"
	parameters.latex.small["RateStatLogger.mean"] = "$\\bar{r}$"


	parameters.pch = lapply(parameters, function(x) x)
	names(parameters.pch) = parameters
	parameters.pch["prior"] = 19
	parameters.pch["likelihood"] = 17
	parameters.pch["ucldStdev"] = 1
	parameters.pch["TipRates"] = 0





		
	ESS.df$ESS.hr = ESS.df$ESS / ifelse(PER_CHAIN, 1, ESS.df$hr)
	first = TRUE
	for (x in 1:(length(settings) -1)) {
		s1 = settings[x]

		for (y in 2:length(settings)) {
			s2 = settings[y]
			if (y <= x) {
				#if (grid.like) plot.new()
				next
			} 




			r.ESS = ESS.df[ESS.df$setting == s1,]
			q.ESS = ESS.df[ESS.df$setting == s2,]


			#na.rm = !( is.na(r.ESS$ESS.hr) | is.na(q.ESS$ESS.hr) )

			#r.ESS = r.ESS[na.rm,]
			#q.ESS = q.ESS[na.rm,]

			xymin = 1

			xyrange = c(xymin, max(c(r.ESS$ESS.hr, q.ESS$ESS.hr), na.rm = TRUE))
			#xyrange = c(0, max(c(r.ESS$MCD.states, q.ESS$MCD.states)))
			
			#xyrange = c(0, 2000) 
			per = ifelse(PER_CHAIN, "ESS/chain", "ESS/hr")
			plot(1, 1, xlim = xyrange, ylim = xyrange, xlab = paste0(per, " [", settings.names[[s1]], "]"),  ylab = paste0(per, " [", settings.names[[s2]], "]"), 
			type = "n", main = paste0("Comparison of ", settings.names[[s2]], " and ", settings.names[[s1]]), log="xy", xaxs = "i", yaxs = "i", axes = FALSE, cex.main = 1.5, cex.lab = 1.5)


			
			xymax = xyrange[2]


			times = c(4, 16, 64)
			bg.cols = c("#A9A9A9", "#BEBEBE", "#E8E8E8")

			lwd_base = 1.6
			for (i in length(times):1){
				t = times[i]
				polygon(c(0.0001, xymax*10, xymax*10, 0.0001), c(0.0001*t, xymax*10*t, xymax*10/t, 0.0001/t), col = bg.cols[i], lwd =lwd_base * 0.6^i, border="black")
			}


			textshift = 1
			for (i in length(times):1){
				t = times[i]
				text(xymin*t*textshift, xymin*textshift, paste0("$\\frac{1}{", t, "} \\times$"), adj = c(-0.5, -0.6), cex = 1)
				text(xymin*textshift, xymin*t*textshift, paste0("$", t, "\\times$"), adj = c(-0.2, -2), cex = 1)
			}
			abline(0,1, lwd = lwd_base)

			colfunc = colorRampPalette(c("black", "red"))
			#max.len = max(benchmark[,v])
			ncolours = 15
			l.cols = colfunc(ncolours)
			get.col = function(Leff) {
				j = ceiling((Leff / max.len) * ncolours)
				l.cols[j]
			}



			datasets = as.character(unique(ESS.df$dataset))
			data.cols = c("red", "blue", "green", "yellow", "orange", "black", "purple", "darkgreen", "cyan", "magenta")
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
					se1 = 2*sqrt(var(xvals) / length(xvals))
					se2 = 2*sqrt(var(yvals) / length(yvals))
					lines(c(m1 - se1, m1 + se1), c(m2, m2), col = "black")
					lines(c(m1, m1), c(m2 - se2, m2 + se2), col = "black")
					
					#text(m1, m2, paste0(parameters.latex.small[[p]], "$_", which(datasets == d), "$"), col = data.cols[dataNum])
					text(m1, m2, parameters.latex.small[[p]], col = data.cols[dataNum], cex = 1.4)

				}

			}

			
			#box()
			axis(1, cex = 2)
			axis(2, cex = 2, las=2)


			dataset_names = sapply(strsplit(datasets, "_"), function(ele) ele[1])
			#gsub("^a", "", gsub("_", " ", datasets))
			if (first) legend("bottomright", dataset_names, fill = data.cols[1:length(datasets)], bg = "white", bty = "n", cex = 1)
			first = FALSE
			#legend("bottomright", as.character(parameters.latex[names(parameters.pch)[parameters.pch != names(parameters.pch)]]), pch = as.numeric(unlist(parameters.pch)[parameters.pch != names(parameters.pch)]))



			
		}

	}

	if (!grid.like){
		#par(mar = c(0,0,0,0))
		#plot.new()

		#text(0.5, 0, main, cex = 2, adj = c(0.5, 1))
		#title(xlab = main, cex.lab = 1.5)
	}



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






	dev.off()

	tools::texi2pdf(paste0(filename, ".tex"))
	#system(paste(getOption("pdfviewer"), paste0(filename, ".pdf")))

}



plot.grid("ESS_round1_real", c("real_06", "real", "real_adaptive"), TRUE, "Round 1")
plot.grid("ESS_round1_quant", c("quant_06_cached", "quant_cached", "quant_cached_adaptive"), TRUE, "Round 1")

#plot.grid("ESS_round3", c("bactrian95", "NER"), FALSE, "Round 3")
#plot.grid("ESS_round4", c("NER", "AVMN"), FALSE, "Round 4")

#plot.grid("ESS_real", c("real_06", "real"), TRUE, "Round 1")
#plot.grid("ESS_quant", c("quant_06", "quant"), TRUE, "Round 1")
plot.grid("ESS_round2", c("cat", "real_adaptive", "quant_cached_adaptive"), TRUE, "Round 2")

plot.grid("ESS_round3", c("real_adaptive", "bactrian95"), TRUE, "Round 3")




#plot.grid("ESS_round2", c("quant", "bactrian95", "bactrian98"), FALSE, "Round 2")

#plot.grid("ESS_full", settings)



write.table(ESS.df, "ESSall.tsv", sep = "\t", quote = F, row.names = F)

bad.df = ESS.df[ESS.df$ESS < 200,]
write.table(bad.df, "ESSbad.tsv", sep = "\t", quote = F, row.names = F)

print(paste("There are", nrow(bad.df), "parameters with ESS<200"))








