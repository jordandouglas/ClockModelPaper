
library(tikzDevice)
library(HDInterval)
library(lubridate)




#settings = c("cat",  "real_06", "quant_06", "real", "quant", "bactrian95", "bactrian98", "NER", "AVMN")
settings = c("real", "real_adaptive", "real_06")

benchmark = data.frame(batch = numeric(0), param = character(0), dataset = character(0), partition = numeric(0), setting = character(0), nstates = numeric(0), finalESS = numeric(0), finalMCD = numeric(0), done = logical(0),  error = logical(0))


latex.table = read.table("/home/jdou557/Documents/Marsden2019/Months/January2020/fromVM/latex.table", sep = "&")
colnames(latex.table) = c("ID", "N", "P", "L", "Leff", "ref")
latex.table$ref = gsub(" ", "", gsub("[0-9].+", "", latex.table$ref))



latex.table$completion = ""

progress.df = read.csv("progress.csv", header = T)
progress.df$batch = ifelse(progress.df$batch == 4, 5, ifelse(progress.df$batch == 5, 4, progress.df$batch))


parameters = c("posterior", "likelihood", "prior", "treeLength", "height", "birthRate", "kappa",  "freqParameter", "ucldStdev", "clockRate", "TipRates", "RateStatLogger.mean", "RateStatLogger.variance")


parameters.latex = lapply(parameters, function(x) x)
names(parameters.latex) = parameters
parameters.latex["posterior"] = "Posterior $P(\\theta|D)$"
parameters.latex["likelihood"] = "Likelihood $P(D|\\theta)$"
parameters.latex["prior"] = "Prior $P(\\theta)$"
parameters.latex["birthRate"] = "Yule birth rate $\\lambda$"
parameters.latex["gammaShape"] = "$\\Gamma$ shape"
parameters.latex["kappa"] = "$\\kappa$"
parameters.latex["freqParameter"] = "Nucleotide frequency $f$"
parameters.latex["ucldStdev"] = "Clock SD $\\sigma$"
parameters.latex["TipRates"] = "Mean tip rate $r$"
parameters.latex["treeLength"] = "Tree length $l$"
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
 ESS = numeric(0), hr = numeric(0), ESS.hr = numeric(0), Leff = numeric(0), L = numeric(0), N = numeric(0))


speed.df = data.frame(batch = numeric(0), dataset = character(0), setting = character(0), state = numeric(0), speed = numeric(0))

# For each batch
batchfolders = list.dirs(recursive = F)
for (f in batchfolders){

	if (f == "./datasets") next
	batchNum = as.numeric(gsub("./Batch", "", f))

	setwd(f)
	batch = as.numeric(substring(f, 8))

	# For each dataset in this batch
	datasetFolders = list.dirs(recursive = F)
	for (d in datasetFolders) {


		#if (d == "./Fong_2012" | d == "./McCormack_2013") next
		

		dataset = substring(d, 3)
		matching_row_num = sapply(latex.table$ref, function(pattern) length(grep(pattern, dataset)) > 0)
		if (sum(matching_row_num) == 0) {
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
					finished = 
					progress.rows = progress.df[progress.df$batch == batchNum & 
												progress.df$dataset == gsub("/", "", dataset) &
												progress.df$setting == s,]

					start.date = progress.rows[progress.rows$status == "start", "date"]
					start.date = getDateTime(start.date)
					finish.date = progress.rows[progress.rows$status == "finish", "date"]
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



				
			

					this.ESS.df = read.table(paste0("benchmark_",  s, P, ".log.ess"), sep = "\t", header = T)


					for (p in parameters){

						pindex = grep(paste0(p, ".+ESS"), colnames(this.ESS.df))
						#ESS.hr = mean(ESS_values[pindex]) / time.hr
						#print(paste(chain, "_", p, "_", ESS.hr))

						ESS = mean(as.numeric(this.ESS.df[1,pindex]), na.rm = T)
						#print(paste(p, ESS))

						
						ESS.df2 = data.frame(batch = batch, param = p, setting = s, dataset = dataset, ESS = ESS, hr = runtime.hr, partition = P, Leff = Leff, L = L, N = N)
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



#write.table(benchmark, sep = ",", quote = F, row.names = F, "benchmark.csv")
#write.table(latex.table, sep = ",", quote = F, row.names = F, "data.done.csv")



rows = 2
columns = 2

filename = "benchmark"
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(paste0(filename, ".tex"), width = 5*columns, height = 5*rows, standAlone = TRUE,
    	packages = c("\\usepackage{tikz}",
	         "\\usepackage[active,tightpage,psfixbb]{preview}",
	         "\\PreviewEnvironment{pgfpicture}",
	         "\\setlength\\PreviewBorder{0pt}",
	         "\\usepackage{amssymb}"))



#pdf(paste0("CalSim_", taxa, ".pdf"), width = 2.5*cols, height = 2.5*rows)

par(mfrow = c(rows, columns), mar = c(4,4,4,4))




col.map = lapply(settings, function(x) x )
col.map["cat"] = "black"
col.map["real"] = "red"
col.map["quant"] = "#008cba"
col.map["real_06"] = "orange"
col.map["quant_06"] = "#d3d3d3"
col.map["bactrian95"] = "purple"
col.map["bactrian98"] = "green"
col.map["NER"] = "cyan"


get.col = function(theSettings) {
	unlist(sapply(theSettings, function(s) col.map[[as.character(s)]]))
}


cols = get.col(benchmark$setting)
plot(benchmark$Leff, benchmark$time.hr, type="n", pch = 16, col = cols, xlab = "$L_{eff}$ (kb)", ylab = "Time until convergence (hr)", main = "Time to convergence")



for (s in settings) {

	df = benchmark[benchmark$setting == s,]
	df = df[order(df$Leff),]
	linear.model = lm(time.hr ~ Leff, data = df)

	c = linear.model$coefficients[1]
	b_L = linear.model$coefficients[2]
	#b_N = linear.model$coefficients[3]

	#abline(c, b_L, col = col.map[[s]])

	points(df$Leff, df$time.hr, pch = 16, type="b", col = get.col(s))

}



legend("topright", gsub("_", "", settings), col = get.col(settings), pch = 16)

plot(benchmark$N, benchmark$time.hr, type="n", pch = 16, col = cols, xlab = "$N$", ylab = "Time until convergence (hr)", main = "Time to convergence")


data.cols = c("red", "blue", "green", "yellow", "orange")
for (s in settings) {

	df = benchmark[benchmark$setting == s,]
	df = df[order(df$N),]
	linear.model = lm(time.hr ~ N, data = df)

	c = linear.model$coefficients[1]
	b_L = linear.model$coefficients[2]
	#b_N = linear.model$coefficients[3]

	#abline(c, b_L, col = col.map[[s]])

	points(df$N, df$time.hr, pch = 16, type="b", col = get.col(s))

}





plot(benchmark$setting, benchmark$time.hr, ylab = "Time to convergence (hr)", axes = F)
ns = as.numeric(sapply(settings, function(s) sum(benchmark$setting == s)))
axis(1, at = 1:length(settings), labels = paste0(gsub("_", "", settings), " (",  ns, ")"))
axis(2)


#plot(benchmark$Leff * benchmark$N, benchmark$time.hr, type="n", pch = 16, col = cols, xlab = "$N \\times L_{eff}$ (kb)", ylab = "Time until convergence (hr)", main = "Time to convergence", log = "xy")



for (s in settings) {

	df = benchmark[benchmark$setting == s,]
	df = df[order(df$Leff * df$N),]
	linear.model = lm(time.hr ~ Leff, data = df)

	c = linear.model$coefficients[1]
	b_L = linear.model$coefficients[2]
	#b_N = linear.model$coefficients[3]

	#abline(c, b_L, col = col.map[[s]])

	#points(df$Leff * df$N, df$time.hr, pch = 16, type="b", col = get.col(s))
	#text(df$Leff * df$N, df$time.hr, df$id, col = get.col(s))

}






dev.off()

tools::texi2pdf(paste0(filename, ".tex"))
system(paste(getOption("pdfviewer"), paste0(filename, ".pdf")))




###
#plot(benchmark$setting, benchmark$MCD.states, ylab = "Time to convergence (hr)", log = "y")


#dev.new()
#xyrange = c(0, max(benchmark$time.hr[benchmark$setting == "real" | benchmark$setting == "quant"]))
#plot(benchmark$time.hr[benchmark$setting == "real"], benchmark$time.hr[benchmark$setting == "quant"], xlim = xyrange, ylim = xyrange, xlab = "Real", ylab = "Quant")
#abline(0,1)




#nocat = benchmark[benchmark$setting != "cat",]
#nocat[nocat$setting == "real","time.hr"] = benchmark[benchmark$setting == "real","time.hr"] - benchmark[benchmark$setting == "cat","time.hr"]
#nocat[nocat$setting == "real_06","time.hr"] = benchmark[benchmark$setting == "real_06","time.hr"] - benchmark[benchmark$setting == "cat","time.hr"]
#nocat[nocat$setting == "quant","time.hr"] = benchmark[benchmark$setting == "quant","time.hr"] - benchmark[benchmark$setting == "cat","time.hr"]
#nocat[nocat$setting == "quant_06","time.hr"] = benchmark[benchmark$setting == "quant_06","time.hr"] - benchmark[benchmark$setting == "cat","time.hr"]

#plot(nocat$setting, nocat$time.hr, ylab = "Time to convergence (hr)")


plot(benchmark$time.hr, benchmark$time.hr, type="n", pch = 16, col = cols, xlab = "cat time until convergence (hr)", ylab = "Setting time until convergence (hr)", main = "Time to convergence")
abline(0, 1)



for (s in settings[-1]){


	points(benchmark[benchmark$setting == "cat","time.hr"], benchmark[benchmark$setting == s,"time.hr"], col = get.col(s)) 



}





## MCD
plot(0, 0, xlim = c(0, 40),  ylim = c(0, 40),  type = "n", xlab = "Bactrian(0.95)", ylab = "NER + Bactrian(0.95)")

datasets = as.character(unique(benchmark$dataset))
data.cols = c("red", "blue", "green", "yellow", "orange")
for (i in 1:length(datasets)) {
	d = datasets[i]
	y = benchmark[benchmark$setting == "NER" & benchmark$dataset == d,"time.hr"]
	x = benchmark[benchmark$setting == "bactrian95" & benchmark$dataset == d,"time.hr"]
	points(mean(x), mean(y), col = data.cols[i], pch = 16)	

	se_x = sqrt(var(x) / length(x))
	se_y = sqrt(var(y) / length(y))
	segments(mean(x) + se_x, mean(y), mean(x) - se_x, mean(y))
	segments(mean(x), mean(y) + se_y, mean(x), mean(y) - se_y)

	points(x, y, col = data.cols[i], pch = 1)	


}

legend("topleft", datasets, col = data.cols, pch = 16)

abline(0,1)



###########################
########### ESS ###########
###########################

#settings = c("cat", "real", "quant")

ESS.df = ESS.df[!is.na(ESS.df$ESS),]

columns = length(settings)-1
rows = length(settings)-1

filename = "ESS"
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(paste0(filename, ".tex"), width = 4*columns, height = 4*rows, standAlone = TRUE,
    	packages = c("\\usepackage{tikz}",
	         "\\usepackage[active,tightpage,psfixbb]{preview}",
	         "\\PreviewEnvironment{pgfpicture}",
	         "\\setlength\\PreviewBorder{0pt}",
	         "\\usepackage{amssymb}"))


par(mfrow = c(rows, columns), mar = c(4,4,4,4))

parameters.latex.small = lapply(parameters, function(x) x)
names(parameters.latex.small) = parameters
parameters.latex.small["posterior"] = "$P$"
parameters.latex.small["likelihood"] = "$L$"
parameters.latex.small["prior"] = "$p$"
parameters.latex.small["birthRate"] = "$\\lambda$"
parameters.latex.small["gammaShape"] = "$\\Gamma$"
parameters.latex.small["kappa"] = "$\\kappa$"
parameters.latex.small["freqParameter"] = "$f$"
parameters.latex.small["ucldStdev"] = "$\\sigma$"
parameters.latex.small["TipRates"] = "$r$"
parameters.latex.small["treeLength"] = "$l$"
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


	
ESS.df$ESS.hr = ESS.df$ESS / ESS.df$hr
for (x in 1:(length(settings) -1)) {
	s1 = settings[x]

	for (y in 2:length(settings)) {
		s2 = settings[y]
		if (y <= x) {
			plot.new()
			next
		} 


		for (v in c("Leff")) {


			r.ESS = ESS.df[ESS.df$setting == s1,]
			q.ESS = ESS.df[ESS.df$setting == s2,]


			#na.rm = !( is.na(r.ESS$ESS.hr) | is.na(q.ESS$ESS.hr) )

			#r.ESS = r.ESS[na.rm,]
			#q.ESS = q.ESS[na.rm,]

			xymin = 1

			xyrange = c(xymin, max(c(r.ESS$ESS.hr, q.ESS$ESS.hr)))
			#xyrange = c(0, max(c(r.ESS$MCD.states, q.ESS$MCD.states)))
			
			#xyrange = c(0, 2000) 
			plot(1, 1, xlim = xyrange, ylim = xyrange, xlab = paste0("ESS/hr (\\textit{", gsub("_", " ", s1), "})"), 
											 ylab = paste0("ESS/hr (\\textit{", gsub("_", " ", s2), "})"), 
			type = "n", main = paste0("Comparison of \\textit{", gsub("_", " ", s2),"} and \\textit{", gsub("_", " ", s1),"} parameterisations"), log="xy")


			
			xymax = xyrange[2]


			times = c(2, 5, 20)
			bg.cols = c("#A9A9A9", "#BEBEBE", "#E8E8E8")

			for (i in length(times):1){
				t = times[i]
				polygon(c(0.0001, xymax*10, xymax*10, 0.0001), c(0.0001*t, xymax*10*t, xymax*10/t, 0.0001/t), col = bg.cols[i], lty = "2626", border=NA)
			}


			for (i in length(times):1){
				t = times[i]
				text(xymin*t, xymin, paste0("$\\frac{1}{", t, "} \\times$"), adj = c(-0.5, -0.6), cex = 0.7)
				text(xymin, xymin*t, paste0("$", t, "\\times$"), adj = c(-0.2, -2), cex = 0.7)
			}
			abline(0,1)

			colfunc = colorRampPalette(c("black", "red"))
			#max.len = max(benchmark[,v])
			ncolours = 15
			l.cols = colfunc(ncolours)
			get.col = function(Leff) {
				j = ceiling((Leff / max.len) * ncolours)
				l.cols[j]
			}

			


			for (i in 1:nrow(r.ESS)) {

				r.df = r.ESS[i,]
				p = as.character(r.df$param)

				#if (any(p == c("posterior", "birthRate", "gammaShape", "kappa",  "freqParameter"))) next


				q.df = ESS.df[ESS.df$setting == s2 & as.character(ESS.df$dataset) == as.character(r.df$dataset) & as.character(ESS.df$param) == p & ESS.df$partition == r.df$partition,]
				Leff = benchmark[benchmark$setting == s2 & as.character(benchmark$dataset) == as.character(r.df$dataset) & benchmark$P == r.df$partition, v]
				#if (Leff < 0.2) next
				#text(r.df$ESS.hr, q.df$ESS.hr, parameters.latex.small[[p]], col = "red")#get.col(Leff))

				
				#points(r.df$ESS.hr, q.df$ESS.hr, pch = parameters.pch[[p]], col = "black")#get.col(Leff))



			}

			datasets = as.character(unique(ESS.df$dataset))
			data.cols = c("red", "blue", "green", "yellow", "orange", "black", "purple", "darkgreen")
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
					text(m1, m2, parameters.latex.small[[p]], col = data.cols[dataNum])

				}

			}

			legend("topleft", gsub("^a", "", gsub("_", " ", datasets)), col = data.cols[1:length(datasets)], pch = 16, bg = "white")
			box()

			#legend("bottomright", as.character(parameters.latex[names(parameters.pch)[parameters.pch != names(parameters.pch)]]), pch = as.numeric(unlist(parameters.pch)[parameters.pch != names(parameters.pch)]))



		}
	}

}


dev.off()

tools::texi2pdf(paste0(filename, ".tex"))
system(paste(getOption("pdfviewer"), paste0(filename, ".pdf")))




write.table(ESS.df, "ESS.csv", row.names = FALSE, quote = FALSE, sep = ",")




for (x in 1:(length(settings) -1)) {
	s1 = settings[x]

	for (y in 2:length(settings)) {
		s2 = settings[y]
		if (y <= x) {
			next
		} 


		print(paste0(s2, "/", s1))

		datasets = as.character(unique(ESS.df$dataset))
		medians = numeric(0)
		for (d in datasets) {

			d1.ESS = ESS.df[ESS.df$setting == s1 & ESS.df$dataset == d,]
			d2.ESS = ESS.df[ESS.df$setting == s2 & ESS.df$dataset == d,]
			#print(paste0(d, " has ", mean(d2.ESS$nstates), " for ", s2, " and ", mean(d1.ESS$nstates), " for ", s1))

			for (p in parameters) {

				if (!any(p == c("TipRates", "ucldStdev", "posterior", "prior", "treeLength"))) next

				r.ESS = ESS.df[ESS.df$setting == s1 & ESS.df$param == p & ESS.df$dataset == d,]
				q.ESS = ESS.df[ESS.df$setting == s2 & ESS.df$param == p & ESS.df$dataset == d,]

				include = rep(TRUE, nrow(r.ESS))
				for (i in 1:nrow(r.ESS)) {

					#r.df = r.ESS[i,]
					#Leff = benchmark[benchmark$setting == "quant" & as.character(benchmark$dataset) == as.character(r.df$dataset) & benchmark$P == r.df$partition, v]
					#if (Leff < 1) include[i] = FALSE

				}


				diff = q.ESS$ESS.hr[include] / r.ESS$ESS.hr[include]
				m = median(diff)
				se =  1.2533 * sqrt(var(diff) / length(diff))
				#print(paste0(d, " ", p, " has median ", round(m, 3), " +/- ", round(se, 3), " (n=", length(diff), ")"))
				medians = c(medians, m)

			}
		}


		print(paste("On average, the median difference is", round(mean(medians), 3)))

	print("-----------")
	print("")
	print("")

	}
}



















columns = 3
rows = 3

filename = "ESSbatches"
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(paste0(filename, ".tex"), width = 4*columns, height = 4*rows, standAlone = TRUE,
    	packages = c("\\usepackage{tikz}",
	         "\\usepackage[active,tightpage,psfixbb]{preview}",
	         "\\PreviewEnvironment{pgfpicture}",
	         "\\setlength\\PreviewBorder{0pt}",
	         "\\usepackage{amssymb}"))


par(mfrow = c(rows, columns), mar = c(4,4,4,4))


d = "Crawford_2012"
for (b in 1:9){


	
	r.ESS = ESS.df[ESS.df$setting == "real" & ESS.df$dataset == d & ESS.df$batch == b,]
	q.ESS = ESS.df[ESS.df$setting == "quant" & ESS.df$dataset == d & ESS.df$batch == b,]

	if (nrow(r.ESS) == 0 | nrow(q.ESS) == 0) next

	xyrange = c(0, max(c(r.ESS$ESS, q.ESS$ESS)))


	plot(r.ESS$ESS, q.ESS$ESS, xlab = paste0("real (", round(mean(r.ESS$hr),3), " hr)"), ylab = paste0("quant (", round(mean(q.ESS$hr),3), " hr)"), main = paste("Batch", b), xlim = xyrange, ylim = xyrange)
	abline(0, 1)
	abline(0, 2)
	abline(0, 0.5)


}





dev.off()

tools::texi2pdf(paste0(filename, ".tex"))
system(paste(getOption("pdfviewer"), paste0(filename, ".pdf")))

















# Speed

plot(0, 0, xlim = c(0, max(speed.df$state)), ylim = c(0, max(speed.df$speed)), xlab = "State number", ylab = "Samples / million states" )

datasets = as.character(unique(speed.df$dataset))
setting.cols = c("red", "blue", "green", "black", "orange")



gradients.df = data.frame(setting = character(0), batch = numeric(0), gradient = numeric(0), dataset = character(0))
for (dataNum in 1:length(datasets)) {
	d = datasets[dataNum]
	
	for (settingsNum in 1:length(settings)) {
		s = settings[settingsNum]

		
		speed.d.s.df = speed.df[speed.df$setting == s & speed.df$dataset == d,]

		batches = sort(as.numeric(unique(speed.df$batch)))
		
		for (b in batches) {

			speed.d.s.b.df = speed.d.s.df[speed.d.s.df$batch == b & speed.d.s.df$state >= 5* 10^6,]
			if (nrow(speed.d.s.b.df) == 0) next
			points(speed.d.s.b.df$state, speed.d.s.b.df$speed, type = "l", col = setting.cols[settingsNum])


			m = lm(speed ~ state, data = speed.d.s.b.df)
			gradient = as.numeric(m$coefficients[2]) * 10^6	
			gradients.df2 = data.frame(setting = s, batch = b, gradient = gradient, dataset = d)
			gradients.df = rbind(gradients.df, gradients.df2)		

		}


	}


}


grid(nx = 50, ny = 50)




plot(

plot(gradients.df$setting, gradients.df$gradient)















# Over L
datasets = as.character(unique(ESS.df$dataset))

rel.params = c("prior", "ucldStdev", "TipRates")
ESS.df$ESS.hr = ESS.df$ESS / ESS.df$nstates * 10^6# Temporary
ESS.df = ESS.df[!is.na(ESS.df$ESS),]
#settings = c("cat",  "real", "quant", "bactrian95")

settings = c("quant",  "bactrian95", "bactrian98", "NER", "AVMN")



baseline = settings[1]
target = settings[-1]
data.cols = c("red", "blue", "orange", "purple")



columns = length(rel.params)
rows = 3




filename = "ESSoverNL"
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb
	tikz(paste0(filename, ".tex"), width = 4*columns, height = 4*rows, standAlone = TRUE,
    	packages = c("\\usepackage{tikz}",
	         "\\usepackage[active,tightpage,psfixbb]{preview}",
	         "\\PreviewEnvironment{pgfpicture}",
	         "\\setlength\\PreviewBorder{0pt}",
	         "\\usepackage{amssymb}"))


par(mfrow = c(rows, columns), mar = c(4,4,4,4))



for (x in c("N", "L", "Leff")) {



	for (p in rel.params) {

		p.df = ESS.df[ESS.df$param == p,]

		plot(0, 1, type = "n", xlim = c(0, max(p.df[,x])), ylim = c(0, 5), xlab = x, ylab = "ESS / $10^6$ states compared with $\\textit{cat}$", main = parameters.latex[[p]] )
		abline(1, 0, lty = "2626")
		for (d in datasets) {

			
			d.df = ESS.df[ESS.df$d == d & ESS.df$param == p,]
			x_val = d.df[1,x]



			for (s0 in 1:length(target)) {
				s = target[s0]

				ess_s = d.df[d.df$s == s, "ESS.hr"]
				ess_b = d.df[d.df$s == baseline, "ESS.hr"]
				diff = ess_s / ess_b
				se = sqrt(var(diff) / length(diff))


				points(x_val, mean(diff), col = data.cols[s0], pch = 16)

				segments(x_val, mean(diff) - se, x_val, mean(diff) + se, col = data.cols[s0])
				

			}

		}
		


		legend("topleft", gsub("_", "", target), pch = 16, col = data.cols[1:length(target)])

	}





}




dev.off()

tools::texi2pdf(paste0(filename, ".tex"))
system(paste(getOption("pdfviewer"), paste0(filename, ".pdf")))




s1 = ESS.df[ESS.df$setting == "cat","ESS.hr"]
s2 = ESS.df[ESS.df$setting == "quant","ESS.hr"]


plot(s1, s2, xlab = "cat", ylab = "quant")

