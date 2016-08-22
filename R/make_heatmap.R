MakeHeatmap <- function(heatmap.data, graph.title, cluster, filenames) {
  heatmap.colors <- c("#000033", "#333366", "#666699", "#9999CC", "#CCCCFF", 
                      "#EEEEFF", "#FFFFFF", "#FFEEEE", "#FFCCCC", "#FF9999",
                      "#FF6666", "#FF3333", "#CC0000")
  
  if (grepl("gene", cluster)) {
    dendro.status="row"
    col.v=FALSE
    row.v=TRUE
  } else if (grepl("sample", cluster)) {
    dendro.status="column"
    col.v=TRUE
    row.v=FALSE
  } else if (grepl("both", cluster)) {
    dendro.status="both"
    col.v=TRUE
    row.v=TRUE
  }
  
  # Plot the heatmaps to EPS, PDF, TIFF
  setEPS()
  postscript(filenames$eps)
  gplots::heatmap.2(heatmap.data,
            hclustfun=function(x) hclust(x, method="ward.D2"),
            distfun=function(x) dist(x, method="euclidean"),
            main=graph.title, 
            trace="none",         # turns off trace lines inside the heat map
            margins=c(12, 9),     # widens margins around plot
            dendrogram=dendro.status,
            density.info="none",
            Rowv=row.v,
            Colv=col.v,
            breaks=c(-7:-1/7, 1:7/7),
            cexRow=1, cexCol=1, srtCol=30,
            col=heatmap.colors)
  invisible(dev.off())               # close the EPS device
  print(paste0("PostScript of heatmap has been created at ./", filenames$eps))
  
  pdf(filenames$pdf, height=11, width=8.5)
  gplots::heatmap.2(heatmap.data,
            hclustfun=function(x) hclust(x, method="ward.D2"),
            distfun=function(x) dist(x, method="euclidean"),
            main=graph.title,
            trace="none",         # turns off trace lines inside the heat map
            margins=c(12, 9),     # widens margins around plot
            dendrogram=dendro.status,
            density.info="none",
            Rowv=row.v,
            Colv=col.v,
            keysize=1,
            breaks=c(-7:-1/7, 1:7/7),
            cexRow=1, cexCol=1, srtCol=30,
            col=heatmap.colors)
  invisible(dev.off())               # close the PDF device
  print(paste0("PDF of heatmap has been created at ./", filenames$pdf))

  if (capabilities("tiff")) {
    tiff(filenames$tiff)
    gplots::heatmap.2(heatmap.data,
              hclustfun=function(x) hclust(x, method="ward.D2"),
              distfun=function(x) dist(x, method="euclidean"),
              main=graph.title,
              trace="none",         # turns off trace lines inside the heat map
              margins=c(12, 9),     # widens margins around plot
              dendrogram=dendro.status,
              density.info="none",
              Rowv=row.v,
              Colv=col.v,
              breaks=c(-7:-1/7, 1:7/7),
              cexRow=1, cexCol=1, srtCol=30,
              col=heatmap.colors)
    invisible(dev.off())               # close the TIFF device
    print(paste0("TIFF of heatmap has been created at ./", filenames$tiff))
  }
  
  suppressWarnings(xfig(filenames$xfig))
  gplots::heatmap.2(heatmap.data,
            hclustfun=function(x) hclust(x, method="ward.D2"),
            distfun=function(x) dist(x, method="euclidean"),
            main=graph.title,
            trace="none",         # turns off trace lines inside the heat map
            margins=c(12, 9),     # widens margins around plot
            dendrogram=dendro.status,
            density.info="none",
            Rowv=row.v,
            Colv=col.v,
            breaks=c(-7:-1/7, 1:7/7),
            cexRow=1, cexCol=1, srtCol=30,
            col=heatmap.colors)
  invisible(dev.off())               # close the FIG device
  print(paste0("FIG of heatmap has been created at ./", filenames$xfig))
}
