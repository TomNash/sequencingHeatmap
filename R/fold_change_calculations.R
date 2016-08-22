FoldChangeCalculations <- function(data.list, filenames, input.file) {
  gene.data <- data.list$genes
  heatmap.values <- data.list$values
  base.means <- gene.data$baseMean
  symbol <- gene.data$Symbol
  gene.ID <- gene.data$gene.ID
  # Convert to logBASE2 space and calculate difference of values and medians
  log2space.diff <- log2(heatmap.values) - log2(base.means)
  
  # Apply fold-change to boost +/-
  fold.change <- ifelse(log2space.diff > 0, 2^log2space.diff, (-1)*2^(-log2space.diff))
  
  # Write out to CSV the fold-change data
  unique.dup.symbol <- unique(symbol[which(symbol %in% symbol[duplicated(symbol)])])
  for (dup.symbol in unique.dup.symbol) {
    dup.symbol.index <- which(symbol %in% dup.symbol)
    for (i in 1:length(dup.symbol.index)) {
      symbol[dup.symbol.index[i]] <- ifelse(i==1,
                                         dup.symbol,
                                         paste0(dup.symbol, ".", gene.ID[dup.symbol.index[i]]))
    }
  }
  processed.data <- data.frame(symbol, fold.change, row.names=1)
  write.csv(processed.data, file=filenames$fc)
  print(paste0("CSV of fold-change values has been created for ", input.file))
  
  if (dim(fold.change)[1] == 1){
    print(paste0("Only one gene found to be significant for ", input.file, " - No heatmap creation"))
    return(FALSE)
  }
  
  # Normalize fold-change for heatmap drawing
  pos.indices <- which(fold.change >= 1)
  neg.indices <- which(fold.change < -1)
  
  # LogBASE2 of the fold-changes
  fold.change[pos.indices] <- log2(fold.change[pos.indices])
  fold.change[neg.indices] <- -log2(-fold.change[neg.indices])
  
  # Global normalization, divide by max value
  fold.change.g <- fold.change/max(abs(fold.change))
  
  # Square-root scaling
  fold.change.gnorm <- ifelse(fold.change.g > 0, fold.change.g^0.5, -(-(fold.change.g))^0.5)
  
  # Create data frame combining labels with normalized fold-change values
  gnorm.processed.data <- data.frame(symbol, fold.change.gnorm, row.names=1)
  gnorm.mat.processed.data <- data.matrix(gnorm.processed.data)
  return(gnorm.mat.processed.data)
}
