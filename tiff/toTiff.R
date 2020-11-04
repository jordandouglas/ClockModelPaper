library(pdftools)


files = c("../Figures/rateparameterisation.pdf", "../Figures/correlations.pdf", "../Figures/clockSD.pdf", 
		"../Figures/bactrian.pdf", "../Figures/NarrowExchange.pdf",  "../Figures/acceptanceRates.pdf", "../Figures/tournament.pdf",
		"../benchmarking/benchmarkingVM/ESS_round1_real.pdf",
		"../benchmarking/benchmarkingVM/ESS_round2.pdf",
		"../benchmarking/benchmarkingVM/ESS_timesettings.pdf",
		"../benchmarking/benchmarkingVM/ESS_round3.pdf",
		"../Figures/Fig12.pdf",
		"../benchmarking/benchmarkingVM/ESS_round5.pdf")
for (i in 1:length(files)){


	f = files[i]
	o = paste0("ig", i, ".tiff")
	pdf_convert(f, "tiff", dpi=600, filenames = o)


	#tiff(o, compression = "lzw")



	#dev.off()

}

