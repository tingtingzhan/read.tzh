
#' @title Read Local Excel File
#'
#' @description ..
#' 
#' @param path,... parameters of \link[readxl]{read_excel}
#' 
#' @param sheets \link[base]{integer} \link[base]{vector}
#' 
#' @param pattern \link[base]{regex}, pattern of the names of sheets to be selected
#' 
#' @details 
#' The function [read_excel_all()] ..
#' 
#' The function [read_excel_sheets()] ..
#' 
#' The function [read_excel_pattern()] ..
#' 
#' @note 
#' The function \link[readxl]{read_excel} returns (a \link[base]{list} of) \link[tibble]{tibble} object(s). 
#' 
#' Name clash \link[officer]{read_xlsx} and \link[readxl]{read_xlsx}.
#' 
#' \CRANpkg{readxl} cannot write to Excel file.  Write to `.csv` file instead.
#' 
#' \url{https://cran.r-project.org/web/packages/Microsoft365R/vignettes/od_sp.html}
#' can access data stored in SharePoint Online sites and OneDrive (personal and business). 
#' But need to request approval from Jefferson.
#' 
#' @importFrom readxl excel_sheets read_excel
#' @name read_excel_ext
#' @export
read_excel_pattern <- function(path = stop(), pattern, ...) {
  sht <- excel_sheets(path)
  if (!(ns <- length(sht))) stop('Excel file has no sheet?')
  
  if (!is.character(pattern) || length(pattern) != 1L || anyNA(pattern) || !nzchar(pattern)) stop('pattern must be len-1 char')
  idx <- grep(pattern, x = sht)
  if (!any(idx)) stop('no qualifying sheet?')
  
  sseq <- seq_len(ns)[idx]
  if (length(sseq) == 1L) return(read_excel(path = path, sheet = sseq, ...))
  
  names(sseq) <- sht[idx]
  lapply(sseq, FUN = \(i) {
    cat('Sheet', sQuote(sht[i]), '\n')
    read_excel(path = path, sheet = i, ...)
  })
}


#' @importFrom readxl excel_sheets read_excel
#' @rdname read_excel_ext
#' @export
read_excel_sheets <- function(path = stop(), sheets, ...) {
  sht <- excel_sheets(path)
  if (!(ns <- length(sht))) stop('Excel file has no sheet?')
  
  if (!is.integer(sheets)) stop('`sheets` must be integer')
  if (!length(sheets) || anyNA(sheets)) stop('integer sheets must not be len-0 or contains NA')
  if (any(sheets < 1L)) stop('sheets must be positive')
  if (any(id <- (sheets > ns))) {
    cat(sQuote(sheets[id]), 'out of range.\n')
    if (!length(sheets <- sheets[!id])) return(invisible())
  }
  
  if (length(sheets) == 1L) return(read_excel(path = path, sheet = sheets, ...))
    
  names(sheets) <- sht[sheets]
  lapply(sheets, FUN = \(i) {
    cat('Sheet', sQuote(sht[i]), '\n')
    read_excel(path = path, sheet = i, ...)
  })
}



#' @importFrom readxl excel_sheets read_excel
#' @rdname read_excel_ext
#' @export
read_excel_all <- function(path = stop(), ...) {
  sht <- excel_sheets(path)
  if (!(ns <- length(sht))) stop('Excel file has no sheet?')
  if (ns == 1L) return(read_excel(path = path, sheet = 1L, ...))

  sseq <- seq_along(sht)
  names(sseq) <- sht
  lapply(sseq, FUN = \(i) {
    cat('Sheet', sQuote(sht[i]), '\n')
    read_excel(path = path, sheet = i, ...)
  })
}



# ?readxl::read_excel argument `col_types`:
# 'factor' is not an option (2025-03-07), and I don't want to `stringsAsFactors = TRUE` anyway!!!
# 'POSIXct' columns will be correctly handled by default






