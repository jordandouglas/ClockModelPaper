library(ape)
args = commandArgs(trailingOnly=TRUE)


print(args)

logfile.folder = args[1] 
output.txt.folder  = args[2]


logfile.folder = "../true/"
output.txt.folder = "../sim/"

benchmarking = FALSE

#nsim = 100
nsim = 100
#ntaxa = 100
ntaxa = 30
#templates = c("cat", "real", "quant", "real_06", "quant_06")
#templates = c("bactrian95")

templates = c("cat_adaptive", "quant_cached_adaptive", "real_adaptive", "AVMVN")


for (xx in 1:length(templates)) {
	templ = templates[xx]

	
	
	# Read the log file
	L = read.table(file=paste0(logfile.folder, "/priorSamples_N", ntaxa, ".log"), sep="\t", header=T)

	# Read the .xml template
	template_file = paste0("simulate_", templ, ".xml")
	xml_template = readLines(template_file) 

	# Read species tree
	trees = read.nexus(file=paste0(logfile.folder, "/priorSamples_N", ntaxa, ".trees"))



	# String replace
	sim = 1
	for (i in 1:nrow(L)){


		tree = trees[i]
		tree = read.tree(text = write.tree(tree))

		xml_populated = xml_template


		# Sample	proportionInvariant	gammaShape	kappa	ucldStdev	rateCategories.1	rateCategories.2	...	rateCategories.199	birthRate	freqParameter.1	freqParameter.2	freqParameter.3	freqParameter.4


		# Put tree in file
 		xml_populated = gsub("INSERT_TREE_HERE", write.tree(tree), xml_populated)


		# Clock SD
		xml_populated = gsub("INSERT_CLOCK_SD_HERE", L[i,"ucldStdev"], xml_populated)


		# Frequencies 
		xml_populated = gsub("INSERT_FREQUENCIES_HERE", paste(as.numeric(L[i, paste0("freqParameter.", 1:4)]), collapse = " "), xml_populated)


		# Kappa
		xml_populated = gsub("INSERT_KAPPA_HERE", L[i,"kappa"], xml_populated)



		# Simualation number
		xml_populated = gsub("SIMNUM", sim, xml_populated)


		# Log line
		xml_populated = gsub("PUT_LOG_LINE_HERE", paste0("\t<!-- Data simulated from state \n\t", paste(colnames(L), collapse = "\t"), "\n\t", paste(L[i,], collapse = "\t"), "\n\t-->"), xml_populated)



		# Output log file
		xml_populated = gsub("OUTPUT_RATES", paste0("logfile_", sim), xml_populated)



		# Quantiles
		xml_populated = gsub("INSERT_QUANTILES_HERE", paste0(runif(2*ntaxa-2, 0, 1), collapse = " "), xml_populated)
		
		
		
		# Write the new xml file
		print(paste("Saving to", paste0(output.txt.folder, templ, "/run", sim, ".xml")))
		write(paste(xml_populated, collapse = "\n"), paste0(output.txt.folder, templ, "/run", sim, ".xml"))



		sim = sim + 1
		if (sim > nsim){
			break
		}


	}



}






