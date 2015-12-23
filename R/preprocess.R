preprocess <- function(raw_data,input_file,inputs,method,top,filenames,use_column_range){
  # Read in contents of current file
  geneList <- readLines(paste0(inputs,"/",input_file))
  
  if (geneList[1] %in% c("gene","sample","both")) {
    cluster <- geneList[1]
    geneList <- geneList[-1]
  } else {
    warning(paste0("No tree status indicated for ",input_file,", will skip this file until format corrected"))
    return(FALSE)
  }
  # Graph title is now first line
  if (!geneList[1] %in% raw_data[,1]) {
    graph_title <- geneList[1]
    geneList <- geneList[-1]
  } else{
    warning(paste0("No graph title provided for ",input_file,", will skip this file until format corrected"))
    return(FALSE)
  }
  
  if (!dir.exists(paste0(getwd(),"/deseqHeatmap-output/",inputs))){
    print(paste0("Creating output directory at ",getwd(),"/deseqHeatmap-output/",inputs))
    dir.create(file.path(getwd(),"deseqHeatmap-output/",inputs), showWarnings = FALSE)
  }
  
  # Extract gene name and ID
  if(method == "symbol") {
    heatmap_id <- toupper(raw_data$Symbol)
    geneList <- toupper(geneList)
  }
  else if (method == "geneid") { # preferred
    heatmap_id <- toupper(raw_data$GeneID)
    geneList <- toupper(geneList)
  }
  
  # Filter down to desired genes
  matchingGenes <- na.omit(match(geneList,heatmap_id))
  heatmap_filtered_genes <- raw_data[matchingGenes,]

  # Sort by adjusted p-value
  heatmap_filtered_genes$padj <- as.numeric(heatmap_filtered_genes$padj)
  heatmap_filtered_genes <- heatmap_filtered_genes[order(heatmap_filtered_genes$padj),]
  
  # Filter down to just data columns
  heatmap_values <- heatmap_filtered_genes[,use_column_range]
  heatmap_values[heatmap_values==0] <- 1
  
  removeIndices <- which(as.numeric(heatmap_filtered_genes$padj >= 0.4) | 
                           is.na(heatmap_filtered_genes$padj) | 
                           heatmap_filtered_genes$baseMean < 15)
  
  # Extract desired values, check if desired best p-values
  if (length(removeIndices) > 0) {
    heatmap_filtered_genes <- heatmap_filtered_genes[-removeIndices,]
    heatmap_values <- heatmap_values[-removeIndices,]
    if (dim(heatmap_values)[1] == 0) {
      warning(paste0("No genes from file ", input_file, " have sufficient p-value or base mean. Moving to next file in subset."))
      return(FALSE)
    }
  }
  if (!is.null(top)) {
    if (top > dim(heatmap_filtered_genes)[1]) {
      warning(paste0("More top values (",top,") desired than available. Will use all available genes."))
    } 
    else {
      heatmap_filtered_genes <- heatmap_filtered_genes[1:top,]
      heatmap_values <- heatmap_values[1:top,]
    }
  }
  # Use available columns from the list desired in #71 for 
  # output csv of truncated data to just desired genes
  output_csv <- heatmap_filtered_genes[,colnames(heatmap_filtered_genes)[c("Symbol","GeneID","Alias","Description") %in% names]]
  output_csv <- data.frame(output_csv,heatmap_filtered_genes[,use_column_range])
  output_csv <- data.frame(output_csv,heatmap_filtered_genes[,c("baseMean","log2FoldChange","lfcSE","stat","pvalue","padj")])
  write.csv(output_csv,file=filenames$csv)
  return(list(genes=heatmap_filtered_genes,values=heatmap_values,title=graph_title,cluster=cluster))
}