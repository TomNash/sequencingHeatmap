PreprocessData <- function(raw.data, input.file, subsets.directory, id.method, top, filenames,
                           data.columns, cutoff.p, base.mean.count) {
  # Read in contents of current file
  gene.list <- readLines(paste0(subsets.directory, "/", input.file))
  
  if (gene.list[1] %in% c("gene", "sample", "both")) {
    cluster <- gene.list[1]
    gene.list <- gene.list[-1]
  } else {
    warning(paste0("No tree status indicated for ", input.file, ", will skip this file until format corrected"))
    return(FALSE)
  }
  # Graph title is now first line
  if (!gene.list[1] %in% raw.data[, 1]) {
    graph.title <- gene.list[1]
    gene.list <- gene.list[-1]
  } else{
    warning(paste0("No graph title provided for ", input.file, ", will skip this file until format corrected"))
    return(FALSE)
  }
  
  if (!dir.exists(paste0(getwd(), "/sequencingHeatmap-output/", subsets.directory))){
    print(paste0("Creating output directory at ", getwd(), "/sequencingHeatmap-output/", subsets.directory))
    dir.create(file.path(getwd(), "sequencingHeatmap-output/", subsets.directory), showWarnings = FALSE)
  }
  
  # Extract gene name and ID
  if (id.method == "symbol") {
    heatmap.id <- toupper(raw.data$Symbol)
    gene.list <- toupper(gene.list)
  }
  else if (id.method == "geneid") { # preferred
    heatmap.id <- toupper(raw.data$GeneID)
    gene.list <- toupper(gene.list)
  }
  
  # Filter down to desired genes
  matching.genes <- na.omit(match(gene.list, heatmap.id))
  heatmap.filtered.genes <- raw.data[matching.genes, ]

  # Sort by adjusted p-value
  heatmap.filtered.genes$padj <- as.numeric(heatmap.filtered.genes$padj)
  heatmap.filtered.genes <- heatmap.filtered.genes[order(heatmap.filtered.genes$padj), ]
  
  # Filter down to just data columns
  heatmap.values <- heatmap.filtered.genes[, data.columns]
  heatmap.values[heatmap.values==0] <- 1
  
  remove.indices <- which(as.numeric(heatmap.filtered.genes$padj >= cutoff.p) | 
                           is.na(heatmap.filtered.genes$padj) | 
                           heatmap.filtered.genes$baseMean < base.mean.count)
  
  # Extract desired values, check if desired best p-values
  if (length(remove.indices) > 0) {
    heatmap.filtered.genes <- heatmap.filtered.genes[-remove.indices, ]
    heatmap.values <- heatmap.values[-remove.indices, ]
    if (dim(heatmap.values)[1] == 0) {
      warning(paste0("No genes from file ", input.file, " have sufficient p-value or base mean. Moving to next file in subset."))
      return(FALSE)
    }
  }
  if (!is.null(top)) {
    if (top > dim(heatmap.filtered.genes)[1]) {
      warning(paste0("More top values (", top, ") desired than available. Will use all available genes."))
    } 
    else {
      heatmap.filtered.genes <- heatmap.filtered.genes[1:top, ]
      heatmap.values <- heatmap.values[1:top, ]
    }
  }
  write.csv(heatmap.filtered.genes, file=filenames$csv, row.names=F)
  return(list(genes=heatmap.filtered.genes, values=heatmap.values, title=graph.title, cluster=cluster))
}
