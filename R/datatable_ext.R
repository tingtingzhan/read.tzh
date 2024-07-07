

#' @title Read Big `.csv` and `.txt` File
#' 
#' @description ..
#' 
#' @param file \link[base]{character} scalar, file name (extension not needed).
#' 
#' @param header \link[base]{logical} scalar
#' 
#' @param ... ..
#' 
#' @note 
#' \link[data.table]{fread} returns \link[data.table]{data.table} object
#' 
#' @importFrom data.table fread
#' @importFrom utils read.csv
#' @name read_csv_txt
#' @export
read_csv <- function(file, header = TRUE, ...) {
  if (file.size(file) < 2e6) return(read.csv(file = file, header = header, ...))
  fread(file = file, header = header, showProgress = FALSE, ...) # ?data.table:::as.data.frame.data.table
}



#' @importFrom data.table fread
#' @importFrom utils read.table
#' @rdname read_csv_txt
#' @export
read_txt <- function(file, header = TRUE, ...) {
  if (file.size(file) < 2e6) return(read.table(file = file, header = header, ...))
  fread(file = file, header = header, showProgress = FALSE, ...) # ?data.table:::as.data.frame.data.table
}