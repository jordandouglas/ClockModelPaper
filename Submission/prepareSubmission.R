library(pdftools)
library(tikzDevice)
library(raster)
library(rgdal)

manuscript = readLines("../manuscript.tex")
out.manuscript = "../submission.tex"

# Get figure directories
figlines = grep("includegraphics", manuscript)
fig.dir = gsub("[}]*", "", gsub(".+[{]", "", manuscript[figlines]))
figlines = figlines[fig.dir != "PLOS-submission.eps"]
fig.dir = fig.dir[fig.dir != "PLOS-submission.eps"]
fig.dir  = paste0("../", fig.dir)


# Comment out figures
manuscript[figlines] = paste("%%%%%", manuscript[figlines])
write(paste(manuscript, collapse = "\n"), out.manuscript)


# Convert to tiff and compress it
tifoptions = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")
for (i in 1:length(fig.dir)) {


	filename = paste0("Figures/Fig", i, ".tif")
	pdf_convert(fig.dir[i], format="tiff", filenames=filename, dpi = 500, antialias = FALSE)


	# Compress
	x = raster(filename)
	writeRaster(x, filename, options = tifoptions, overwrite = TRUE)

}

tools::texi2pdf(out.manuscript)



