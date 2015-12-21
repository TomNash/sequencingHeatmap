output_filenames <- function(top,input_file,inputs,deseq){
  if (is.null(top)){
    base_filename <- basename(file_path_sans_ext(input_file))
    eps_full <- paste0("deseqHeatmap-output/",inputs,"/",base_filename,"-all.eps")
    csv_full <- paste0("deseqHeatmap-output/",inputs,"/",basename(file_path_sans_ext(deseq)),
                           "-",base_filename,"-all-truncated.csv")
    pdf_full <- paste0("deseqHeatmap-output/",inputs,"/",base_filename,"-all.pdf")
    fc_full <- paste0("deseqHeatmap-output/",inputs,"/",
                                  base_filename,"-all-foldChange.csv")
    tiff_full <- paste0("deseqHeatmap-output/",inputs,"/",base_filename,"-all.tiff")
    xfig_full <- paste0("deseqHeatmap-output/",inputs,"/",base_filename,"-all.fig")
  } else {
    top <- strtoi(top)
    base_filename <- basename(file_path_sans_ext(input_file))
    eps_full <- paste0("deseqHeatmap-output/",inputs,"/",base_filename,"-top",top,".eps")
    csv_full <- paste0("deseqHeatmap-output/",inputs,"/",basename(file_path_sans_ext(deseq)),
                           "-",base_filename,"-top",top,"-truncated.csv")
    pdf_full <- paste0("deseqHeatmap-output/",inputs,"/",base_filename,"-top",top,".pdf")
    fc_full <- paste0("deseqHeatmap-output/",inputs,"/",
                                  base_filename,"-top",top,"-foldChange.csv")
    tiff_full <- paste0("deseqHeatmap-output/",inputs,"/",base_filename,"-top",top,".tiff")
    xfig_full <- paste0("deseqHeatmap-output/",inputs,"/",base_filename,"-top",top,".fig")
  }
  file_list <- list(eps=eps_full,csv=csv_full,pdf=pdf_full,fc=fc_full,tiff=tiff_full,xfig=xfig_full)
  return(file_list)
}