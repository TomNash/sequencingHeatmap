OutputFilenames <- function(top, input.file, subsets.directory, sequencing){
  if (is.null(top)){
    base.filename <- basename(tools::file_path_sans_ext(input.file))
    eps.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", base.filename, "-all.eps")
    csv.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", basename(tools::file_path_sans_ext(sequencing)),
                           "-", base.filename, "-all-truncated.csv")
    pdf.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", base.filename, "-all.pdf")
    fc.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/",
                                  base.filename, "-all-foldChange.csv")
    tiff.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", base.filename, "-all.tiff")
    xfig.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", base.filename, "-all.fig")
  } else {
    top <- strtoi(top)
    base.filename <- basename(tools::file_path_sans_ext(input.file))
    eps.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", base.filename, "-top", top, ".eps")
    csv.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", basename(tools::file_path_sans_ext(sequencing)),
                           "-", base.filename, "-top", top, "-truncated.csv")
    pdf.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", base.filename, "-top", top, ".pdf")
    fc.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/",
                                  base.filename, "-top", top, "-foldChange.csv")
    tiff.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", base.filename, "-top", top, ".tiff")
    xfig.full <- paste0("sequencingHeatmap-output/", subsets.directory, "/", base.filename, "-top", top, ".fig")
  }
  file.list <- list(eps=eps.full, csv=csv.full, pdf=pdf.full, fc=fc.full, tiff=tiff.full, xfig=xfig.full)
  return(file.list)
}
