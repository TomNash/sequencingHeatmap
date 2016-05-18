#' Sequencing Data Heatmap Creation
#'
#' This function allows you to create heatmaps from a sequencing output spreadsheet.
#' @param sequencing The path to the sequencing output spreadsheet (.xlsx), i.e. sequencing="path/to/output.xlsx"
#' @param sheet The sheet number in the spreadsheet with data, i.e sheet=1
#' @param columns The column range of data to use, i.e. columns=5:31
#' @param inputs The path to the directory which contains files of genes of interest, i.e.
#' inputs="path/to/inputs/"
#' @param method The method of identifying each gene. It must be either method="symbol" or method="geneid"
#' @param top (optional) The number of top genes to select, filtered by adjusted p-value. Can use
#' multiple values for one run, i.e. top=100 or top=c(100,125,150)
#' @examples
#' \dontrun{
#' 
#' library(sequencingHeatmap)
#' sequencingHeatmap(sequencing="sequencingoutput.xlsx", sheet=1, columns=5:14,
#' inputs="experiment1", method="symbol", top=c(100,125,150))
#' }
#' @details
#' \strong{Formatting of an input file:} \cr
#' Method of clustering the results, either "gene", "sample", or "both" \cr
#' Title of the heatmap plot \cr
#' GENE1 \cr
#' GENE2 \cr
#' GENE3 \cr
#' ... \cr
#' \cr
#' \strong{Example input file:} \cr
#' gene \cr
#' Sample Graph Title \cr
#' SFRP2 \cr
#' CD4 \cr
#' BRCA1 \cr
#' ... \cr
#' \cr
#' \strong{Outputs} (found in \code{./sequencingHeatmap-output})\strong{:} \cr
#' PDF, TIFF, FIG, and EPS of heatmaps \cr
#' CSV of fold-changes \cr
#' * All file names correspond to those in the provided inputs directory \cr
#' \cr
#' \strong{Directory structure for inputs and resultings outputs with given args:}
#' \preformatted{
#' .
#' |---sequencingoutput.xlsx                 # sequencing='sequencingoutput.xlsx'
#' |---experiment1                      # inputs='experiment1'
#' |   |---genelist1.txt
#' |----sequencingHeatmap-output
#' |   |---experiment1
#' |       |---genelist1-top100.pdf     # top=c(100,125)
#' |       |---genelist1-top125.pdf     
#' |       |---genelist1-top100.tiff
#' |       |---genelist1-top125.tiff
#' |       |---genelist1-top100.eps
#' |       |---genelist1-top125.eps
#' |       |---genelist1-top100.fig
#' |       |---genelist1-top125.fig
#' |       |---genelist1-top100-foldChange.csv
#' |       |---genelist1-top125-foldChange.csv
#' |       |---sequencingoutput-genelist1-top100-truncated.csv
#' |       |---sequencingoutput-genelist1-top125-truncated.csv}
#' @export

sequencingHeatmap <- function(sequencing,sheet,columns,inputs,method,cutoff.p=0.4,
                         baseMeanCount=15,top){
  if(missing(top)) { top=NULL }
  
  input_verify(sequencing,sheet,columns,inputs,method,top)
  if (!dir.exists(paste0(getwd(),"/sequencingHeatmap-output"))) {
    print(paste0("Creating output directory at ",getwd(),"/sequencingHeatmap-output/"))
    
    dir.create(file.path(getwd(), "sequencingHeatmap-output"), showWarnings = FALSE)
  }
  # Read in Excel spreadsheet
  raw_data <- openxlsx::read.xlsx(sequencing,sheet=strtoi(sheet))

  if(stringr::str_sub(inputs,-1)=="/") { inputs <- substr(inputs,1,nchar(inputs)-1) }

  for (input_file in list.files(inputs)){
    if(!is.null(top)) {
      for (i in 1:length(top)) {
        filenames <- output_filenames(top[i],input_file,inputs,sequencing)
        preprocessed_data <- preprocess(raw_data,input_file,inputs,method,top[i],
                                        filenames,columns,cutoff.p,baseMeanCount)
        if (typeof(preprocessed_data)=="logical") { break }
        heatmap_ready <- fc_calculations(preprocessed_data,filenames,input_file)
        if (typeof(heatmap_ready)=="logical") { break }
        heatmap_make(heatmap_ready,preprocessed_data$title,
                     preprocessed_data$cluster,filenames)
      }
    } else {
      filenames <- output_filenames(top,input_file,inputs,sequencing)
      preprocessed_data <- preprocess(raw_data,input_file,inputs,method,top,
                                      filenames,columns,cutoff.p,baseMeanCount)
      if (typeof(preprocessed_data)=="logical") { break }
      heatmap_ready <- fc_calculations(preprocessed_data,filenames,input_file)
      if (typeof(heatmap_ready)=="logical") { break }
      heatmap_make(heatmap_ready,preprocessed_data$title,
                   preprocessed_data$cluster,filenames)
    }
  }
}
