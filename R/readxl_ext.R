


#' @title Read Local Excel File
#'
#' @description ..
#' 
#' @param path,... parameters of \link[readxl]{read_excel}
#' 
#' @param sheets \link[base]{integer} \link[base]{vector}
#' 
#' @param pattern regular expression, pattern of the names of sheets to be selected
#' 
#' @details 
#' Function [read_excel_all] ..
#' 
#' Function [read_excel_sheets] ..
#' 
#' Function [read_excel_pattern] ..
#' 
#' @note 
#' \link[readxl]{read_excel} returns (a \link[base]{list} of) \link[tibble]{tibble} object(s). 
#' 
#' Name clash \link[officer]{read_xlsx} and \link[readxl]{read_xlsx}.
#' 
#' \CRANpkg{readxl} cannot write to Excel file.  Write to `.csv` file instead.
#' 
#' @references 
#' \url{https://cran.r-project.org/web/packages/Microsoft365R/vignettes/od_sp.html}
#' can access data stored in SharePoint Online sites and OneDrive (personal and business). 
#' But need to request approval from Jefferson.
#' 
#' @importFrom readxl excel_sheets read_excel
#' @name read_excel_ext
#' @export
read_excel_pattern <- function(path = stop(), pattern, ...) {
  sht_nm <- excel_sheets(path)
  if (!(ns <- length(sht_nm))) stop('Excel file has no sheet?')
  
  if (!is.character(pattern) || length(pattern) != 1L || anyNA(pattern) || !nzchar(pattern)) stop('pattern must be len-1 char')
  idx <- grep(pattern, x = sht_nm)
  if (!any(idx)) stop('no qualifying sheet?')
  
  sseq <- seq_len(ns)[idx]
  if (length(sseq) == 1L) return(read_excel(path = path, sheet = sseq, ...))
  
  names(sseq) <- sht_nm[idx]
  lapply(sseq, FUN = function(i) {
    cat('Sheet', sQuote(sht_nm[i]), '\n')
    read_excel(path = path, sheet = i, ...)
  })
}


#' @importFrom readxl excel_sheets read_excel
#' @rdname read_excel_ext
#' @export
read_excel_sheets <- function(path = stop(), sheets, ...) {
  sht_nm <- excel_sheets(path)
  if (!(ns <- length(sht_nm))) stop('Excel file has no sheet?')
  
  if (!is.integer(sheets)) stop('`sheets` must be integer')
  if (!length(sheets) || anyNA(sheets)) stop('integer sheets must not be len-0 or contains NA')
  if (any(sheets < 1L)) stop('sheets must be positive')
  if (any(id <- (sheets > ns))) {
    cat(sQuote(sheets[id]), 'out of range.\n')
    if (!length(sheets <- sheets[!id])) return(invisible())
  }
  
  if (length(sheets) == 1L) return(read_excel(path = path, sheet = sheets, ...))
    
  names(sheets) <- sht_nm[sheets]
  lapply(sheets, FUN = function(i) {
    cat('Sheet', sQuote(sht_nm[i]), '\n')
    read_excel(path = path, sheet = i, ...)
  })
}



#' @importFrom readxl excel_sheets read_excel
#' @rdname read_excel_ext
#' @export
read_excel_all <- function(path = stop(), ...) {
  sht_nm <- excel_sheets(path)
  if (!(ns <- length(sht_nm))) stop('Excel file has no sheet?')
  if (ns == 1L) return(read_excel(path = path, sheet = 1L, ...))

  sseq <- seq_along(sht_nm)
  names(sseq) <- sht_nm
  lapply(sseq, FUN = function(i) {
    cat('Sheet', sQuote(sht_nm[i]), '\n')
    read_excel(path = path, sheet = i, ...)
  })
}



# ?readxl::read_excel argument `col_types`:
# 'factor' is not an option (2023-02-09), and I don't want to `stringsAsFactors = TRUE` anyway!!!
# 'POSIXct' columns will be correctly handled by default






