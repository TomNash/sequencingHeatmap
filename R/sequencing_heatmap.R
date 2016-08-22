#' Sequencing Data Heatmap Creation
#'
#' This function allows you to create heatmaps from a sequencing output spreadsheet.
#' @param input.file The path to the sequencing output spreadsheet (.xlsx), i.e. sequencing="path/to/output.xlsx"
#' @param sheet The sheet number in the spreadsheet with data, i.e sheet=1
#' @param data.columns The column range of data to use, i.e. data.columns=5:31
#' @param subsets.directory The path to the directory which contains files of genes of interest, i.e.
#' subsets.directory="path/to/inputs/"
#' @param id.method The method of identifying each gene. It must be either id.method="symbol" or id.method="geneid"
#' @param base.mean.count Threshold for omitting genes, all genes with expression below this value will not be included,
#' default is set to 15 but set to 0 to include all genes
#' @param top (optional) The number of top genes to select, filtered by adjusted p-value. Can use
#' multiple values for one run, i.e. top=100 or top=c(100,125,150)
#' @examples
#' \dontrun{
#' 
#' library(sequencingHeatmap)
#' sequencingHeatmap(sequencing="sequencingoutput.xlsx", sheet=1, data.columns=5:14,
#' subsets.directory="experiment1", id.method="symbol", top=c(100,125,150))
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
#' |---sequencingoutput.xlsx                 # input.file='sequencingoutput.xlsx'
#' |---experiment1                      # subsets.directory='experiment1'
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

SequencingHeatmap <- function(input.file, sheet, data.columns, subsets.directory, id.method, cutoff.p=0.4,
                         base.mean.count=15, top){
  
  if (missing(top)) {
    top=NULL
  }
  InputVerify(input.file, sheet, data.columns, subsets.directory, id.method, top)
  if (!dir.exists(paste0(getwd(), "/sequencingHeatmap-output"))) {
    print(paste0("Creating output directory at ", getwd(), "/sequencingHeatmap-output/"))
    
    dir.create(file.path(getwd(), "sequencingHeatmap-output"), showWarnings = FALSE)
  }
  # Read in Excel spreadsheet
  raw.data <- openxlsx::read.xlsx(input.file, sheet=strtoi(sheet))

  if (stringr::str_sub(subsets.directory, -1)=="/") {
    subsets.directory <- substr(subsets.directory, 1, nchar(subsets.directory)-1)
  }

  for (input.file in list.files(subsets.directory)){
    if (!is.null(top)) {
      for (i in 1:length(top)) {
        filenames <- OutputFilenames(top[i], input.file, subsets.directory, input.file)
        preprocessed.data <- PreprocessData(raw.data, input.file, subsets.directory, id.method, top[i],
                                            filenames, data.columns, cutoff.p, base.mean.count)
        if (typeof(preprocessed.data)=="logical") { 
          break
        }
        heatmap.ready <- FoldChangeCalculations(preprocessed.data, filenames, input.file)
        if (typeof(heatmap.ready)=="logical") {
          break
        }
        MakeHeatmap(heatmap.ready, preprocessed.data$title,
                    preprocessed.data$cluster, filenames)
      }
    } else {
      filenames <- OutputFilenames(top, input.file, subsets.directory, input.file)
      preprocessed.data <- PreprocessData(raw.data, input.file, subsets.directory, id.method, top, 
                                          filenames, data.columns, cutoff.p, base.mean.count)
      if (typeof(preprocessed.data)=="logical") {
        break
      }
      heatmap.ready <- FoldChangeCalculations(preprocessed.data, filenames, input.file)
      if (typeof(heatmap.ready)=="logical") {
        break
      }
      MakeHeatmap(heatmap.ready, preprocessed.data$title, 
                  preprocessed.data$cluster, filenames)
    }
  }
}
