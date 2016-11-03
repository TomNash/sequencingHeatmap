# RNA Sequencing Heatmap R Package
sequencingHeatmap is an R package that aims to simplify and expedite the creation of heatmaps from provided RNA sequencing output spreadsheets.
Users provide the output file, a directory containing subsets of genes to map, and other specifications to produce heatmaps in the
following file formats: PDF, TIFF, FIG, and PostScript. Fold-change calculations are performed with normalization through dividing by
the maximum value and square root transformation.

> Note: Threshold is set by default to include only genes with a reported adjusted p-value < 0.4 and count >= 15. This can be changed by setting the appropriate variables.

Heatmaps are generated using Ward clustering and Euclidean distance via the `heatmap.2` package.


# Getting started

Install the R package using the following commands on the R console:
```
install.packages("devtools")
devtools::install_github("TomNash/sequencingHeatmap")
library(sequencingHeatmap)
```
Set up a directory to keep your input files, one per subset of genes. The formatting of each file will be as follows: the first line
indicates whether to cluster by gene, sample, or both; the second line provides the title of the heatmap; and subsequent lines identify
each of the genes of interst (be it by its symbol or gene ID). The following shows the first 5 lines of a file which
clusters by sample and uses gene symbol as an identifier:
```
sample
Example Heatmap Title
BRCA1
BRCA2
TP53
```

# Example
The following command could be used to create heatmaps from a given RNA sequencing output file in which results are stored in the first sheet.
The method parameter indicates that the files in the input directory use gene symbol as an identifier (as in the file above). Genes
will be sorted by adjusted p-value and only the minimum 100, 125, and 150 will be used in 3 separate iterations.
```
sequencingHeatmap(sequencing="sequencingoutput.xlsx", sheet=1, columns=5:14, inputs="inputDirectory", method="symbol", top=c(100,125,150))
```
This alternate approach uses the gene ID identifier but chooses to use all genes that meet the threshold. Input files will need to be
formatted appropriately.
```
sequencingHeatmap(sequencing="sequencingoutput.xlsx", sheet=1, columns=5:14, inputs="inputDirectory", method="geneid")
```

