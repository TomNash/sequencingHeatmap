fc_calculations <- function(data_list,filenames,input_file) {
  gene_data <- data_list$genes
  heatmap_values <- data_list$values
  baseMeans <- gene_data$baseMean
  symbol <- gene_data$Symbol
  # Convert to logBASE2 space and calculate difference of values and medians
  log2space_diff <- log2(heatmap_values) - log2(baseMeans)
  
  # Apply fold-change to boost +/-
  fold_change <- ifelse(log2space_diff > 0, 2^log2space_diff, (-1)*2^(-log2space_diff))
  
  # Write out to CSV the fold-change data
  processed_data <- data.frame(symbol,fold_change,row.names=1)
  write.csv(processed_data,file=filenames$fc)
  print(paste0("CSV of fold-change values has been created for ",input_file))
  
  if (dim(fold_change)[1] == 1){
    print(paste0("Only one gene found to be significant for ",input_file," - No heatmap creation"))
    return(FALSE)
  }
  
  # Normalize fold-change for heatmap drawing
  pos_indices <- which(fold_change >= 1)
  neg_indices <- which(fold_change < -1)
  
  # LogBASE2 of the fold-changes
  fold_change[pos_indices] <- log2(fold_change[pos_indices])
  fold_change[neg_indices] <- -log2(-fold_change[neg_indices])
  
  # Global normalization, divide by max value
  fold_change_g <- fold_change/max(abs(fold_change))
  
  # Square-root scaling
  fold_change_gnorm <- ifelse(fold_change_g > 0, fold_change_g^0.5,-(-(fold_change_g))^0.5)
  
  # Create data frame combining labels with normalized fold-change values
  gnorm_processed_data <- data.frame(symbol,fold_change_gnorm,row.names=1)
  gnorm_mat_processed_data <- data.matrix(gnorm_processed_data)
  return(gnorm_mat_processed_data)
}