input_verify <- function(deseq,sheet,columns,inputs,method,top) {
  if(is.null(deseq) || tools::file_ext(deseq) != "xlsx"){
    stop("No DEseq output Excel file provided.")
  }
  if(is.null(sheet) || !is.numeric(sheet)){
    stop("Provide a sheet number to use from DEseq output.")    
  }
  if(is.null(columns) || !is.numeric(columns)) {
    stop("Provide a valid column range from the sheet in DEseq output.")
  }
  if(is.null(inputs) || !dir.exists(inputs) || length(list.files(inputs)) == 0){
    stop("Provided subdirectory does not exist or is empty.")
  }
  if(is.null(method) || !(method %in% c("geneid","symbol"))){
    stop("No identification method provided, use \'geneid\' or \'symbol\'")
  }
  if(!is.null(top)) {
    if(!is.double(top)) {
      stop("Invalid top n p-values selected. Enter a valid array.")
    }
  }
}