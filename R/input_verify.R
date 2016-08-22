InputVerify <- function(input, sheet, columns, subsets.directory, id.method, top) {
  if (is.null(input) || tools::file_ext(input) != "xlsx"){
    stop("No sequencing output Excel file provided.")
  }
  if (is.null(sheet) || !is.numeric(sheet)){
    stop("Provide a sheet number to use from sequencing output.")    
  }
  if (is.null(columns) || !is.numeric(columns)) {
    stop("Provide a valid column range from the sheet in sequencing output.")
  }
  if (is.null(subsets.directory) || !dir.exists(subsets.directory) || length(list.files(subsets.directory)) == 0){
    stop("Provided subdirectory does not exist or is empty.")
  }
  if (is.null(id.method) || !(id.method %in% c("geneid", "symbol"))){
    stop("No identification id.method provided, use \'geneid\' or \'symbol\'")
  }
  if (!is.null(top)) {
    if (!is.double(top)) {
      stop("Invalid top n p-values selected. Enter a valid array.")
    }
  }
}
